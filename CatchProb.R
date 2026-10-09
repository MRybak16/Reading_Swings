

source("SMT_Data_Starter.R")

library(tidyverse)
library(tidymodels)
options(scipen = 999)

# Filter to Season 1

game_info_y1 <- game_info %>%
  filter(year == "y1")

ball_events_y1 <- ball_events %>%
  filter(year == "y1") %>%
  collect() %>%
  mutate(play_per_game = as.numeric(play_per_game),
         timestamp = as.numeric(timestamp),
         ball_eventcode = as.numeric(ball_eventcode),
         player_id = as.numeric(player_id))

# Identify Batted Balls (hit -> next defensive event)

first_pitch <- ball_events_y1 %>%
  group_by(game_string, play_per_game) %>%
  summarize(num_pitches = sum(ball_eventcode == 1), .groups = "drop") %>%
  filter(num_pitches == 1)

ball_events_bbip <- ball_events_y1 %>%
  group_by(game_string, play_per_game) %>%
  filter((player_id == 10 & ball_eventcode == 4) &
           !(lead(ball_eventcode) %in% c(2, 5, 11))) %>%
  ungroup() %>%
  select(game_string, play_per_game, hit_timestamp = timestamp) %>%
  anti_join(first_pitch, by = c("game_string", "play_per_game"))

ball_events_next_play <- ball_events_y1 %>%
  group_by(game_string, play_per_game) %>%
  filter(!(ball_eventcode %in% c(2, 5, 11)) &
           (lag(player_id) == 10 & lag(ball_eventcode) == 4)) %>%
  ungroup() %>%
  select(game_string, play_per_game, next_event_timestamp = timestamp) %>%
  anti_join(first_pitch, by = c("game_string", "play_per_game"))

suppressWarnings(
  hang_times <- bind_rows(ball_events_bbip, ball_events_next_play) %>%
    arrange(game_string, play_per_game) %>%
    group_by(game_string, play_per_game) %>%
    summarize(hit_timestamp = min(hit_timestamp, na.rm = TRUE),
              next_event_timestamp = min(next_event_timestamp, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(hang_time = round((next_event_timestamp - hit_timestamp) / 1000, 2)) %>%
    filter(is.finite(hang_time))
)

# ---- Calculate Hit Distance (isolate real outfield fly balls) ----

relevant_fly_balls <- ball_positions %>%
  filter(year == "y1") %>%
  filter(game_string %in% hang_times$game_string) %>%
  collect() %>%
  mutate(across(.cols = c(starts_with("ball_position"), play_per_game, timestamp),
                .fns = as.numeric)) %>%
  left_join(hang_times, by = join_by(game_string, play_per_game)) %>%
  filter(timestamp == hit_timestamp | timestamp == next_event_timestamp) %>%
  group_by(game_string, play_per_game) %>%
  mutate(hit_distance = sqrt((lead(ball_position_x) - ball_position_x)^2 +
                               (lead(ball_position_y) - ball_position_y)^2)) %>%
  ungroup() %>%
  select(game_string, play_per_game, hang_time, hit_distance,
         hit_timestamp, next_event_timestamp) %>%
  filter(hit_distance > 21)

# ---- Outfielder Tracking Data (LF = 7, CF = 8, RF = 9) ----

of_pos <- player_positions %>%
  mutate(play_per_game = as.numeric(play_per_game)) %>%
  filter(game_string %in% relevant_fly_balls$game_string &
           play_per_game %in% relevant_fly_balls$play_per_game &
           player_id %in% c(7, 8, 9)) %>%
  select(-c("year", "home_team", "away_team", "day")) %>%
  collect()

all_fly_balls <- of_pos %>%
  mutate(play_per_game = as.numeric(play_per_game),
         player_id = as.numeric(player_id)) %>%
  left_join(relevant_fly_balls, by = join_by(game_string, play_per_game)) %>%
  mutate(timestamp = as.numeric(timestamp),
         hit_timestamp = as.numeric(hit_timestamp),
         next_event_timestamp = as.numeric(next_event_timestamp),
         field_x = as.numeric(field_x),
         field_y = as.numeric(field_y)) %>%
  filter(between(timestamp, hit_timestamp, next_event_timestamp)) %>%
  select(game_string, play_per_game, player_id, timestamp,
         hang_time, field_x, field_y) %>%
  arrange(game_string, play_per_game, player_id, timestamp)

# ---- Ball Position at the Moment of the Hit ----

ball_initial <- ball_positions %>%
  filter(year == "y1") %>%
  filter(game_string %in% relevant_fly_balls$game_string) %>%
  collect() %>%
  mutate(play_per_game = as.numeric(play_per_game),
         timestamp = as.numeric(timestamp),
         ball_position_x = as.numeric(ball_position_x),
         ball_position_y = as.numeric(ball_position_y),
         ball_position_z = as.numeric(ball_position_z)) %>%
  inner_join(relevant_fly_balls %>% select(game_string, play_per_game, hit_timestamp),
             by = c("game_string", "play_per_game")) %>%
  filter(timestamp == hit_timestamp) %>%
  select(game_string, play_per_game,
         ball_x = ball_position_x, ball_y = ball_position_y, ball_z = ball_position_z)

# ---- Each Outfielder's Starting Position on the Play ----

outfielder_start_positions <- all_fly_balls %>%
  group_by(game_string, play_per_game, player_id) %>%
  slice_min(timestamp, with_ties = FALSE) %>%
  ungroup() %>%
  rename(start_x = field_x, start_y = field_y, start_timestamp = timestamp)

# ---- Identify the Responsible Outfielder ----
#' For each play, of the 3 tracked outfielders, pick whichever one starts
#' CLOSEST to the ball at the moment of the hit. This treats LF/CF/RF
#' symmetrically instead of assuming CF always fields the ball, and
#' should better reflect which fielder actually had the play.

first_fielder_positions <- outfielder_start_positions %>%
  left_join(ball_initial, by = c("game_string", "play_per_game")) %>%
  mutate(initial_distance = sqrt((ball_x - start_x)^2 + (ball_y - start_y)^2)) %>%
  group_by(game_string, play_per_game) %>%
  slice_min(initial_distance, with_ties = FALSE) %>%
  ungroup()

# ---- Initial Angle (responsible fielder relative to the ball) ----

angle_df <- first_fielder_positions %>%
  mutate(initial_angle = (atan2(ball_y - start_y, ball_x - start_x) * 180 / pi) %% 360) %>%
  select(game_string, play_per_game, initial_angle)

# ---- Initial Fielder Speed (first 3 tracked steps of the responsible fielder) ----

speed_df <- all_fly_balls %>%
  inner_join(first_fielder_positions %>% select(game_string, play_per_game, player_id),
             by = c("game_string", "play_per_game", "player_id")) %>%
  arrange(game_string, play_per_game, timestamp) %>%
  group_by(game_string, play_per_game) %>%
  mutate(dx = field_x - lag(field_x),
         dy = field_y - lag(field_y),
         dt = (timestamp - lag(timestamp)) / 1000,
         step_speed = sqrt(dx^2 + dy^2) / dt) %>%
  summarize(initial_speed = mean(head(step_speed, 3), na.rm = TRUE), .groups = "drop")

# ---- Opportunity Time (pitch thrown -> first defensive touch) ----

opportunity_times <- ball_events_y1 %>%
  group_by(game_string, play_per_game) %>%
  summarize(pitch_timestamp = min(timestamp[ball_eventcode == 1], na.rm = TRUE),
            terminal_timestamp = min(timestamp[ball_eventcode %in% c(2, 16)], na.rm = TRUE),
            .groups = "drop") %>%
  filter(is.finite(pitch_timestamp) & is.finite(terminal_timestamp)) %>%
  mutate(opportunity_time = round((terminal_timestamp - pitch_timestamp) / 1000, 2)) %>%
  select(game_string, play_per_game, opportunity_time)

# ---- Catch Status ----
#' Any of the 3 outfielders (7/8/9) acquiring the ball counts, but only
#' if that acquisition happens BEFORE any bounce is recorded on the play
#' -- otherwise a grounder picked up after landing gets mislabeled as a
#' catch, since ball_eventcode == 2 fires for both cases.

catch_status_df <- ball_events_y1 %>%
  arrange(game_string, play_per_game, timestamp) %>%
  group_by(game_string, play_per_game) %>%
  summarize(
    bounce_ts = min(timestamp[ball_eventcode == 16], na.rm = TRUE),
    acquire_ts = min(timestamp[ball_eventcode == 2 & player_id %in% c(7, 8, 9)], na.rm = TRUE),
    catch_status = case_when(
      is.finite(acquire_ts) & (!is.finite(bounce_ts) | acquire_ts < bounce_ts) ~ "Catch",
      is.finite(acquire_ts) ~ "No_Catch",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  select(game_string, play_per_game, catch_status)

# ---- Assemble Final Modeling Data Set ----

fly_balls_df <- first_fielder_positions %>%
  select(game_string, play_per_game, initial_distance) %>%
  left_join(angle_df, by = c("game_string", "play_per_game")) %>%
  left_join(speed_df, by = c("game_string", "play_per_game")) %>%
  left_join(catch_status_df, by = c("game_string", "play_per_game")) %>%
  left_join(opportunity_times, by = c("game_string", "play_per_game")) %>%
  mutate(catch_status = factor(catch_status, levels = c("Catch", "No_Catch"))) %>%
  filter(!is.na(catch_status),
         is.finite(initial_distance),
         is.finite(initial_angle),
         is.finite(initial_speed),
         is.finite(opportunity_time))

message("Final rows available for modeling: ", nrow(fly_balls_df))
print(table(fly_balls_df$catch_status))

# ---- Train/Test Split ----

set.seed(2026)

fly_balls_split <- initial_split(data = fly_balls_df, prop = 0.8, strata = catch_status)
fly_balls_training <- training(fly_balls_split)
fly_balls_testing <- testing(fly_balls_split)

model_formula <- catch_status ~ initial_distance + initial_angle + initial_speed + opportunity_time

# ---- Regularized Logistic Regression (glmnet) ----

logistic_regression_model <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet") %>%
  fit(model_formula, data = fly_balls_training)

# ---- Random Forest (alternative / comparison model) ----

random_forest_model <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification") %>%
  fit(model_formula, data = fly_balls_training)

# ---- Predictions ----

fly_balls_testing_final <- data.frame(
  actual_catch_status = fly_balls_testing$catch_status,
  logistic_reg_class = predict(logistic_regression_model, new_data = fly_balls_testing, type = "class") %>% pull(),
  logistic_reg_catch_prob = predict(logistic_regression_model, new_data = fly_balls_testing, type = "prob")[[1]],
  random_forest_class = predict(random_forest_model, new_data = fly_balls_testing, type = "class") %>% pull(),
  random_forest_catch_prob = predict(random_forest_model, new_data = fly_balls_testing, type = "prob")[[1]]
)

# ---- Evaluation ----

logistic_reg_conf_mat <- fly_balls_testing_final %>% conf_mat(actual_catch_status, logistic_reg_class)
random_forest_conf_mat <- fly_balls_testing_final %>% conf_mat(actual_catch_status, random_forest_class)

summary(logistic_reg_conf_mat)
summary(random_forest_conf_mat)

# ---- Sanity check: are probabilities actually spread out now? ----

message("Logistic regression predicted probability spread:")
print(summary(fly_balls_testing_final$logistic_reg_catch_prob))

message("Random forest predicted probability spread:")
print(summary(fly_balls_testing_final$random_forest_catch_prob))


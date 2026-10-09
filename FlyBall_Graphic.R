#' Catch Probability GIF Generator
#' Fixed version: no longer uses eval(body(animate_play)) inside local(),
#' which was silently triggering an early return() out of
#' generate_play_gif() itself (since local() shares the calling function's
#' return context) — every call was quietly bailing out before it ever
#' built the plot, styled it, or saved a file.

source("C:/Users/merba/OneDrive/Documents/SMt_Read_Swings/SMT_Data_Starter.R")
source("C:\Users\merba\OneDrive\Documents\SMt_Read_Swings\FlyBall_GIFs 1.R")

library(gifski)
library(ggplot2)
library(gganimate)
library(sportyR)
library(dplyr)

dir.create("all_plays", showWarnings = FALSE)

options(gganimate.dev_args = list(width = 6, height = 6, units = 'in', res = 200))

# ---- Helper: build tracking_data for one play (standalone, no eval/local tricks) ----

build_tracking_data <- function(game_string_input, play_per_game_input) {
  
  fps <- player_positions %>%
    filter(game_string == game_string_input & play_per_game == play_per_game_input) %>%
    collect() %>%
    mutate(across(c(timestamp, field_x, field_y, player_id), as.numeric)) %>%
    filter(player_id < 14) %>%
    mutate(fps = timestamp - lag(timestamp), .by = "player_id") %>%
    count(fps) %>%
    slice_max(n) %>%
    pull(fps)
  
  # Guard: if fps couldn't be determined, fall back to a sane default
  if (length(fps) == 0 || is.na(fps) || fps <= 0) {
    fps <- 50
  }
  
  time_of_pitch <- ball_events %>%
    filter(game_string == game_string_input &
             play_per_game == play_per_game_input &
             ball_eventcode == 1) %>%
    collect() %>%
    pull(timestamp) %>%
    as.numeric()
  
  ball_tracking_data <- ball_positions %>%
    filter(game_string == game_string_input & play_per_game == play_per_game_input) %>%
    collect() %>%
    mutate(type = "ball", player_id = NA) %>%
    select(game_string:timestamp, player_id, type,
           position_x = ball_position_x, position_y = ball_position_y,
           position_z = ball_position_z, everything())
  
  player_tracking_data <- player_positions %>%
    filter(game_string == game_string_input & play_per_game == play_per_game_input) %>%
    collect() %>%
    mutate(player_id = as.numeric(player_id)) %>%
    mutate(type = case_when(player_id <= 9 ~ "defense",
                            between(player_id, 10, 13) ~ "offense",
                            between(player_id, 14, 17) ~ "umpire",
                            player_id %in% c(18, 19) ~ "coach"),
           position_z = NA) %>%
    select(game_string:timestamp, player_id, type,
           position_x = field_x, position_y = field_y, position_z, everything())
  
  bind_rows(player_tracking_data, ball_tracking_data) %>%
    mutate(across(c(timestamp, position_x, position_y, position_z), as.numeric)) %>%
    arrange(timestamp) %>%
    mutate(timestamp_adj = plyr::round_any(timestamp, fps)) %>%
    filter(timestamp >= time_of_pitch) %>%
    mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))
}

# ---- Main function: predict, build tracking data, trim to the catch, render, save ----

generate_play_gif <- function(row_idx, dataset = fly_balls_df, model = logistic_regression_model) {
  
  if (row_idx < 1 || row_idx > nrow(dataset)) {
    stop(paste("Invalid row index. Choose a number between 1 and", nrow(dataset)))
  }
  
  current_play <- dataset[row_idx, ]
  game_str     <- current_play$game_string
  play_pg      <- current_play$play_per_game
  
  prob_df <- predict(model, new_data = current_play, type = "prob")
  catch_prob_val <- if (".pred_Catch" %in% colnames(prob_df)) prob_df$.pred_Catch else prob_df[[1]]
  catch_prob_pct <- paste0(round(catch_prob_val * 100, 1), "%")
  
  file_name <- paste0("play_", game_str, "_", play_pg, ".gif")
  file_path <- file.path("all_plays", file_name)
  
  message(sprintf("[%d/%d] Animating Game: %s | Play: %d | Catch Prob: %s",
                  row_idx, nrow(dataset), game_str, play_pg, catch_prob_pct))
  
  tryCatch({
    
    # Find exact timestamp when the ball is caught or play ends
    # Event codes: 2 = ball acquired, 5 = end of play, 7 = acquired unknown field pos
    catch_events <- ball_events %>%
      filter(
        game_string == game_str &
          play_per_game == play_pg &
          ball_eventcode %in% c(2, 5, 7)
      ) %>%
      collect() %>%
      pull(timestamp) %>%
      as.numeric()
    
    # ---- Build tracking data directly (no eval/local trick) ----
    tracking_data <- build_tracking_data(game_str, play_pg)
    
    if (nrow(tracking_data) == 0) {
      stop("No tracking data found for this game/play combination.")
    }
    
    # ---- Trim to the catch/end-of-play moment ----
    cutoff_time <- NA_real_
    if (length(catch_events) > 0 && !all(is.na(catch_events))) {
      cutoff_time <- min(catch_events, na.rm = TRUE)
    } else {
      ball_pts <- tracking_data %>% filter(type == "ball")
      if (nrow(ball_pts) > 0) {
        max_z_time <- ball_pts$timestamp[which.max(ball_pts$position_z)]
        post_peak  <- ball_pts %>% filter(timestamp >= max_z_time)
        if (nrow(post_peak) > 0) {
          cutoff_time <- post_peak$timestamp[which.min(post_peak$position_z)]
        }
      }
    }
    
    if (!is.na(cutoff_time)) {
      tracking_data <- tracking_data %>% filter(timestamp <= cutoff_time)
    }
    
    if (nrow(tracking_data) == 0) {
      stop("Tracking data was empty after trimming to the catch/end-of-play cutoff.")
    }
    
    # Re-index frame_id after trimming so animation frames are contiguous
    tracking_data <- tracking_data %>%
      mutate(frame_id = match(timestamp_adj, unique(timestamp_adj)))
    
    final_nframes <- max(tracking_data$frame_id, na.rm = TRUE)
    
    # Identify the target fielder
    target_fielder <- ball_events %>%
      filter(
        game_string == game_str,
        play_per_game == play_pg,
        ball_eventcode %in% c(2, 7),
        player_id <= 9
      ) %>%
      collect() %>%
      pull(player_id) %>%
      tail(1)
    
    # Find the at-bat of the fly ball
    current_at_bat <- lineups %>%
      filter(
        game_string == game_str,
        play_per_game == play_pg
      ) %>%
      collect() %>%
      slice(1)
    
    
    # Find the first play of that at-bat
    start_of_at_bat <- lineups %>%
      filter(
        game_string == game_str,
        at_bat == current_at_bat$at_bat
      ) %>%
      collect() %>%
      arrange(play_per_game) %>%
      slice(1)
    
    # Find target fielder's position during that first play
    hypo_start <- player_positions %>%
      filter(
        game_string == game_str,
        play_per_game == start_of_at_bat$play_per_game,
        player_id == target_fielder
      ) %>%
      collect() %>%
      arrange(timestamp) %>%
      slice(1)
    
    # Identify the landing/catch point of the ball
    ball_landing <- ball_positions %>%
      filter(
        game_string == game_str,
        play_per_game == play_pg
      ) %>%
      collect() %>%
      arrange(timestamp) %>%
      slice_tail(n = 1) %>%
      mutate(
        position_x = as.numeric(ball_position_x),
        position_y = as.numeric(ball_position_y)
      )
    
    # Add hypothetical player route to the landing/catch point
    hypo_frames <- tracking_data %>%
      distinct(frame_id, timestamp, timestamp_adj) %>%
      arrange(frame_id)
    
    n_hypo_frames <- nrow(hypo_frames)
    
    hypothetical_player <- data.frame(
      game_string = rep(game_str, n_hypo_frames),
      play_per_game = rep(as.character(play_pg), n_hypo_frames),
      timestamp = hypo_frames$timestamp,
      player_id = rep(999, n_hypo_frames),
      type = rep("hypothetical", n_hypo_frames),
      position_x = seq(
        as.numeric(hypo_start$field_x[1]),
        as.numeric(ball_landing$position_x[1]),
        length.out = n_hypo_frames
      ),
      position_y = seq(
        as.numeric(hypo_start$field_y[1]),
        as.numeric(ball_landing$position_y[1]),
        length.out = n_hypo_frames
      ),
      position_z = rep(NA_real_, n_hypo_frames),
      timestamp_adj = hypo_frames$timestamp_adj,
      frame_id = hypo_frames$frame_id
    )
    
    tracking_data <- bind_rows(
      tracking_data,
      hypothetical_player
    )
    
    
    if (!is.finite(final_nframes) || final_nframes < 1) {
      stop("Could not determine a valid frame count for this play.")
    }
    
    # Plot
    raw_plot <- geom_baseball(league = "MiLB") +
      geom_point(data = tracking_data %>% filter(type != "ball"),
                 aes(x = position_x, y = position_y, fill = type),
                 shape = 21, size = 3, show.legend = FALSE) +
      geom_text(data = tracking_data %>% filter(type == "defense"),
                aes(x = position_x, y = position_y, label = player_id),
                color = "black", size = 2, show.legend = FALSE) +
      geom_point(data = tracking_data %>% filter(type == "ball"),
                 aes(x = position_x, y = position_y, size = position_z),
                 fill = "white", shape = 21, show.legend = FALSE) +
      scale_fill_manual(values = c("offense" = "#005AB5",
                                   "defense" = "#FEFE62",
                                   "coach"   = "#1A85FF",
                                   "umpire"  = "black")) +
      transition_time(frame_id) +
      shadow_wake(0.1, exclude_layer = c(1:16))
    
    # ---- Apply on-field telemetry overlay & dark theme ----
    styled_anim <- raw_plot +
      annotate(
        "rect", xmin = -180, xmax = -80, ymin = 350, ymax = 415,
        fill = "#0E1117", alpha = 0.85, color = "#00E5FF", linewidth = 0.6
      ) +
      annotate(
        "text", x = -130, y = 400,
        label = "CATCH PROBABILITY",
        color = "#A0A0A0", size = 2.3, fontface = "bold"
      ) +
      annotate(
        "text", x = -130, y = 375,
        label = catch_prob_pct,
        color = "#00E5FF", size = 5.5, fontface = "bold"
      ) +
      labs(
        title = paste("SMT TRACKING | GAME:", game_str),
        subtitle = paste0("Play ID: ", play_pg, "  \u2022  Actual Result: ", current_play$catch_status)
      ) +
      theme(
        plot.background  = element_rect(fill = "#0E1117", color = NA),
        panel.background = element_rect(fill = "#0E1117", color = NA),
        plot.title    = element_text(color = "white", face = "bold", size = 12, hjust = 0.05, vjust = -1),
        plot.subtitle = element_text(color = "#9E9E9E", size = 9, hjust = 0.05, vjust = -1)
      )
    
    # ---- Render GIF ----
    rendered_gif <- animate(
      styled_anim,
      nframes = final_nframes,
      fps = 20,
      end_pause = 10,
      renderer = gifski_renderer()
    )
    
    # ---- Save output ----
    anim_save(filename = file_path, animation = rendered_gif)
    message(" -> Saved: ", file_path)
    
  }, error = function(e) {
    message(sprintf(" -> Failed on Row %d (%s_%d): %s", row_idx, game_str, play_pg, e$message))
  })
}

# ---- Loop helper: generate GIFs for every row in a dataset ----

generate_all_play_gifs <- function(dataset = fly_balls_df, model = logistic_regression_model) {
  for (i in seq_len(nrow(dataset))) {
    generate_play_gif(i, dataset = dataset, model = model)
  }
}
# ---- Test on one play ---- *Randomly picked 2, but can change for trial and error
generate_play_gif(2, dataset = fly_balls_df)

# ---- Uncomment to generate GIFs for every play in fly_balls_df ----
generate_all_play_gifs()


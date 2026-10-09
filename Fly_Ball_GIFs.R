# PLAY  ANIMATION

# Run Starter and Catch_Probability Code
source("C:/Users/merba/OneDrive/Documents/SMt_Read_Swings/SMT_Data_Starter.R")
source("C://Users//merba/OneDrive//Documents//SMt_Read_Swings//Catch_Prob_2.R")

# Install and Open Gifski 

install.packages("gifski")
library(gifski)

# Create file folder for gifs

dir.create("all_plays", showWarnings = FALSE)

# Create loop through fly balls
for (i in 1:nrow(fly_balls_df)) {
  
  game_str <- fly_balls_df$game_string[i]
  play_pg <- fly_balls_df$play_per_game[i]
  
  # Generate Frames
  animation <- animate_play(game_str, play_pg)
  
  # Get list of files
  png_files <- list.files(pattern = "\\.png$", full.names = TRUE)
  png_files <- png_files[order(file.info(png_files)$mtime)]
  
  # Define destination file 
  file_name <- paste0("play_", game_str, "_", play_pg, ".gif")
  file_path <- file.path("all_plays", paste0("play_", game_str, "_", play_pg, ".gif"))
  
  # Combine PNGs into a single gif for each play
  gifski(png_files, gif_file = file_path, width = 800, height = 600, delay = 0.1)
  
  # Remove the unnecessary PNG files
  file.remove(png_files)
  
  message("saved: ", file_name)
}
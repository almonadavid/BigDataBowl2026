
## CeeDee Lamb play

original_data <- main_data |> 
  filter(game_id == 2023111209 & play_id == 1830 & player_role == "Targeted Receiver") |> 
  select(game_id, play_id, nfl_id, frame_id, x, y, pre_throw_frame)

# values from placement_errors in 04_placement_quality
expected_x <- 112.5
expected_y <- 0.9

start_x <- 102.13
start_y <- 3.623333

n_post_throw <- sum(original_data$pre_throw_frame == 0)

expected_data <- original_data |> 
  mutate(
    nfl_id = 99999,
    x = case_when(
      pre_throw_frame == 1 ~ NA_real_,
      pre_throw_frame == 0 ~ start_x + (expected_x - start_x) * (row_number() - 50) / n_post_throw
    ),
    y = case_when(
      pre_throw_frame == 1 ~ NA_real_,
      pre_throw_frame == 0 ~ start_y + (expected_y - start_y) * (row_number() - 50) / n_post_throw
    )
  ) |> 
  select(game_id, play_id, nfl_id, frame_id, x, y, pre_throw_frame)

expected_data <- expected_data |> 
  mutate(color2 = "#8A00C4")


test <- bind_rows(main_data, expected_data) |> 
  arrange(game_id, play_id, nfl_id, frame_id)
# in animating_plays_trails.R, replace main_data with test and set gameid = 2023111209 & playid = 1830

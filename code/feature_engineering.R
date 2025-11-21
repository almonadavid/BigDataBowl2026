
library(tidyverse)
future::plan("multisession")


# Q: “How does a wide receiver adjust their downfield movement when the pass is under-thrown?” 
# a.k.a “where would the receiver have gone if the throw was perfect?”

main_data <- read_csv('data/main_data.csv')

### frame-by-frame features: 
# receiver_dist_to_near_sideline
# closest_def_dist, closest_def_s_x, closest_def_s_y, closest_def_a_x, closest_def_a_y


# done:
#  x, y, s_x, s_y, a_x, a_y, x_relative_to_qb_at_throw, y_relative_to_qb_at_throw,
# frames_since_throw, ball_x, ball_y, ball_s_x, ball_s_y, ball_a_x, ball_a_y,
# down, yards_to_go, route_of_targeted_receiver, dropback_type, dropback_distance, 
# team_coverage_man_zone, team_coverage_type, defenders_in_the_box, pass_location_type
# absolute_yardline_number, receiver_dist_from_endzone


ball_data <- main_data |> 
  filter(is.na(nfl_id)) |> 
  group_by(game_id, play_id) |> 
  mutate(
    dx = x - lag(x),
    dy = y - lag(y),
    ball_s = sqrt(dx^2 + dy^2) * 10,
    ball_a = (ball_s - lag(ball_s)) * 10,
    ball_dir = (90 - (atan2(dy, dx) * 180 / pi)) %% 360,
    ball_dir_rad = pi * (ball_dir / 180),
    ball_dir_x = sin(ball_dir_rad),
    ball_dir_y = cos(ball_dir_rad),
    ball_s_x = ball_dir_x * ball_s,
    ball_s_y = ball_dir_y * ball_s,
    ball_a_x = ball_dir_x * ball_a,
    ball_a_y = ball_dir_y * ball_a,
  ) |> 
  select(game_id, play_id, frame_id, ball_x = x, ball_y = y, ball_s_x, ball_s_y, 
         ball_a_x, ball_a_y)

modeling_data <- main_data |>
  group_by(game_id, play_id) |>
  mutate(throw_frame_num = first(frame_id[throw_frame == 1])) |>
  mutate(
    qb_x_at_throw = first(x[is.na(nfl_id) & frame_id == throw_frame_num]),
    qb_y_at_throw = first(y[is.na(nfl_id) & frame_id == throw_frame_num])
  ) |>
  mutate(
    x_relative_to_qb_at_throw = x - qb_x_at_throw,
    y_relative_to_qb_at_throw = y - qb_y_at_throw,
    frames_since_throw = frame_id - throw_frame_num,
    receiver_dist_from_endzone = 110 - x
  ) |> 
  filter(pre_throw_frame == 0 & player_role == "Targeted Receiver") |> # add pass_length_cat == 'deep'
  ungroup() |> 
  left_join(ball_data, by = c('game_id', 'play_id', 'frame_id')) |> 
  select(game_id, play_id, frame_id, x, y, s_x, s_y, a_x, a_y, ball_x, ball_y, ball_s_x, ball_s_y, ball_a_x, 
         ball_a_y, frames_since_throw, down, yards_to_go, absolute_yardline_number, route_of_targeted_receiver, 
         dropback_type, dropback_distance, team_coverage_man_zone, team_coverage_type, 
         defenders_in_the_box, pass_location_type, receiver_dist_from_endzone)
  

##


















# after first model, use Sumer supplementary data and compare results
# separate model with dx, dy as response
# ball_dir_x, ball_dir_y, dir_x, dir_y,

### field boundaries in modeling

# After you predict future x/y
predicted <- predict(model, new_data) %>%
  mutate(
    pred_x = pmin(pmax(pred_x, 0), 120),      # hard clip length
    pred_y = pmin(pmax(pred_y, 0), 53.3)      # hard clip width
  )

# Deviation
# Point-by-Point Euclidean Distance
# Mean Deviation: Average all the \(d_{i}\) values to get the average deviation along the path.
# Maximum Deviation: Find the largest \(d_{i}\) value, which identifies the point of greatest divergence.
# Root Mean Square Deviation (RMSD): Square each \(d_{i}\), sum them up, divide by the number of points, and take the square root.

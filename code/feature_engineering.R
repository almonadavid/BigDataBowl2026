
library(tidyverse, quietly = TRUE)
future::plan("multisession")


# Q: “How does a wide receiver adjust their downfield movement when the pass is under-thrown?” 
# a.k.a “where would the receiver have gone if the throw was perfect?”

main_data <- suppressMessages(read_csv('data/main_data.csv'))

### frame-by-frame features: 
# receiver_dist_to_near_sideline


# done:
#  x, y, s_x, s_y, a_x, a_y, x_relative_to_qb_at_throw, y_relative_to_qb_at_throw,
# frames_since_throw, ball_x, ball_y, ball_s_x, ball_s_y, ball_a_x, ball_a_y,
# down, yards_to_go, route_of_targeted_receiver, dropback_type, dropback_distance, 
# team_coverage_man_zone, team_coverage_type, defenders_in_the_box, pass_location_type
# absolute_yardline_number, receiver_dist_from_endzone, closest_def_dist


## Extracting ball features: ball x/y, speed and acc. components
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

## Getting frame-by-frame distance from the targeted receiver to the closest db
db_distances <- main_data |> 
  select(game_id, play_id, nfl_id, frame_id, x, y, player_role) |> 
  filter(!is.na(nfl_id)) |> 
  group_by(game_id, play_id, frame_id) |> 
  mutate(
   receiver_x = x[player_role == 'Targeted Receiver'] ,
   receiver_y = y[player_role == 'Targeted Receiver'],
   dist_to_receiver = sqrt((x - receiver_x)^2 + (y - receiver_y)^2)
  ) |> 
  filter(player_role != 'Targeted Receiver') |> 
  slice_min(n = 1, order_by = dist_to_receiver, with_ties = FALSE) |> 
  ungroup() |> 
  select(game_id, play_id, frame_id, dist_to_receiver)
  
  
  
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
    qb_time_to_throw = throw_frame_num,
    receiver_dist_from_endzone = 110 - x
  ) |> 
  filter(pre_throw_frame == 0 & player_role == "Targeted Receiver" & pass_length >= 15) |>
  ungroup() |> 
  left_join(db_distances, by = c('game_id', 'play_id', 'frame_id')) |> 
  # left_join(ball_data, by = c('game_id', 'play_id', 'frame_id')) |>  # remove ball features
  select(game_id, week, play_id, frame_id, x, y, s_x, s_y, a_x, a_y, qb_time_to_throw, frames_since_throw, 
         down, yards_to_go, absolute_yardline_number, route_of_targeted_receiver, 
         dropback_type, dropback_distance, team_coverage_man_zone, team_coverage_type, 
         defenders_in_the_box, pass_location_type, receiver_dist_from_endzone, closest_db_dist = dist_to_receiver) |>  # ball_x, ball_y, ball_s_x, ball_s_y, ball_a_x, ball_a_y
  drop_na()


# skimr::skim(modeling_data)

## SAVE
write_csv(modeling_data, 'data/modeling_data.csv')










# after first model, use Sumer supplementary data and compare results
# separate model with dx, dy as response
# ball_dir_x, ball_dir_y, dir_x, dir_y,

### field boundaries in modeling

# After you predict future x/y
# predicted <- predict(model, new_data) %>%
#   mutate(
#     pred_x = pmin(pmax(pred_x, 0), 120),      # hard clip length
#     pred_y = pmin(pmax(pred_y, 0), 53.3)      # hard clip width
#   )

# Deviation
# Point-by-Point Euclidean Distance
# Mean Deviation: Average all the \(d_{i}\) values to get the average deviation along the path.
# Maximum Deviation: Find the largest \(d_{i}\) value, which identifies the point of greatest divergence.
# Root Mean Square Deviation (RMSD): Square each \(d_{i}\), sum them up, divide by the number of points, and take the square root.

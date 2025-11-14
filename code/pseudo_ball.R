
library(purrr)

df <- pre_throw |> 
  filter(player_position == "QB") |>
  group_by(game_id, play_id) |> 
  slice_tail(n = 1) |>
  mutate(last_frame_id = max(frame_id)) |> 
  ungroup() |> 
  select(game_id, play_id, nfl_id, last_frame_id, x, y, ball_land_x, ball_land_y, num_frames_output) |> 
  mutate(nfl_id = NA_real_)

get_pseudo_trajectory <- function(df, g = 9.81) {
  #' @description: function to derive pseudo play-by-play coordinates for the ball
  #' @params: dataframe containing the last pre-throw frame for each QB pass, AND gravitational acceleration value
  #' @output: dataframe of generate coordinates of ball trajectory from last frame of pre-throw to ball_land_x/y
  #' @assumption: I assume that the ball’s pre-throw coordinates are the same as the quarterback’s (passer’s) pre-throw coordinates, since the ball is in their possession immediately before the throw.
  
  df |>
    mutate(flight_duration = num_frames_output * 0.1) |>
    pmap_dfr(function(game_id, play_id, nfl_id, last_frame_id, x, y, ball_land_x, ball_land_y, num_frames_output, flight_duration, ...) {
      time_sequence <- seq(0, flight_duration - 0.1, by = 0.1)
      
      vx_initial <- (ball_land_x - x) / flight_duration
      vy_initial <- (ball_land_y - y + 0.5 * g * flight_duration^2) / flight_duration
      
      x_vals <- x + vx_initial * time_sequence
      y_vals <- y + vy_initial * time_sequence - 0.5 * g * time_sequence^2
      
      data.frame(
        game_id = game_id,
        play_id = play_id,
        nfl_id = nfl_id,
        frame_id = seq(last_frame_id + 1, last_frame_id + num_frames_output),
        x = round(x_vals, 2),
        y = round(y_vals, 2)
      )
    })
}


pseudo_ball_traj <- get_pseudo_trajectory(df) # this is post_throw ball trajectory
# head(pseudo_ball_trajectory)

# Adding pseudo ball trajectory to post_throw
post_throw <- bind_rows(post_throw, pseudo_ball_traj) |> 
  arrange(game_id, play_id, nfl_id, frame_id)

# ---------------------------------------------------------------------------------------------------------------------

pre_throw_psuedo_ball <- pre_throw |> # assuming that the ball’s pre-throw coordinates are the same as the quarterback’s (passer’s) pre-throw coordinates
  filter(player_position == "QB") |>
  select(game_id, play_id, nfl_id, frame_id, x, y, ball_land_x, ball_land_y, num_frames_output) |> 
  mutate(
    nfl_id = NA_real_,
    player_to_predict = TRUE
  )

# Adding a duplicate of QB coordinates as the ball coordinates to pre_throw
pre_throw <- bind_rows(pre_throw, pre_throw_psuedo_ball) |> 
  arrange(game_id, play_id, nfl_id, frame_id)



# next: 
# use field_control.R
#



# the idea is that a perfect QB throw should land to the receiver in stride, not breakage.


library(tidyverse)

main_data <- suppressMessages(read.csv('data/main_data.csv'))

# deep routes: 'GO', 'CORNER', 'POST' (WHEEL...??)

deep_go_plays <- main_data |> 
  filter(player_role == 'Targeted Receiver' & pass_length > 15 & route_of_targeted_receiver == 'GO')

# smooth acceleration values with GAM
library(mgcv)

deep_go_plays <- deep_go_plays |>
  group_by(game_id, play_id) |>
  mutate(
    smoothed_a = predict(gam(a ~ s(frame_id, bs = "cs"), data = pick(everything())))
  ) |> ungroup() |> relocate(smoothed_a, .after = a)


# Direction bins based on Lucca's TEndencIQ
deep_go_plays <- deep_go_plays |> 
  mutate(
    dir_bin = case_when(
      dir < 0 | dir > 180 ~ 'backwards',
      (dir >= 0 & dir < 36) | (dir >= 144 & dir <= 180) ~ 'steep inside/outside',
      (dir >= 36 & dir < 72) | (dir >= 108 & dir < 144) ~ 'inside/outside',
      dir >= 72 & dir < 108 ~ 'forward'
    )
  ) |> 
  relocate(dir_bin, .after = dir)


# Isolating the play descriptions of 'GO' routes
description_go_route <- deep_go_plays |> 
  group_by(game_id, play_id) |> 
  slice(1) |> 
  ungroup() |> 
  select(game_id, play_id, play_description)


#-------------------------------------------------------------------------------------------------------
### Angle between the line of best fit before vs. after the throw frame
angle_between_lines <- function(x, y, pre_throw_frame) {
  
  before_throw_model <- lm(y ~ x, subset = pre_throw_frame == 1)
  after_throw_model <- lm(y ~ x, subset = pre_throw_frame == 0)
  
  # Slopes
  m1 <- coef(before_throw_model)[2]
  m2 <- coef(after_throw_model)[2]
  
  # Angle between lines
  theta <- atan(abs(m1 - m2) / (1 + m1*m2))
  theta * 180 / pi
}

angle_between <- deep_go_plays |>
  group_by(game_id, play_id) |>
  summarise(theta_degrees = angle_between_lines(x, y, pre_throw_frame),
            .groups = 'drop')

# theta_degrees: How much the receiver’s actual movement path (x–y trajectory) changed before vs. after the throw.
# High: they changed running direction sharply.
# Low: their path stayed smooth and consistent.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Counting changes in dir_bin when ball-in-air
num_dir_changes <- deep_go_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  select(game_id, play_id, dir_bin) |> 
  mutate(is_change = dir_bin != lag(dir_bin) & !is.na(lag(dir_bin))) |> 
  summarise(n_changes = sum(is_change, na.rm = TRUE), .groups='drop')

# High n_changes: The receiver kept switching directional categories.
# This means lots of route adjustment while the ball was in the air.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Direction difference between x/y at throw frame vs x/y at catch frame
dir_diff <- deep_go_plays |>
  group_by(game_id, play_id) |> 
  summarise(
    dir_at_throw = dir[throw_frame == 1],
    dir_at_end = dir[last(frame_id)],
    dir_change = abs(dir_at_end - dir_at_throw),
    lateral_drift = abs(y[throw_frame == 1] - y[last(frame_id)]),
    .groups = "drop"
  ) |> select(game_id, play_id, dir_change, lateral_drift)

# dir_change: How much the receiver’s body orientation changed between the throw frame and the catch/end frame.
# High: they turned their body a lot.
# Low: they kept facing essentially the same direction.

# lateral_drift: How much the receiver moved side-to-side (y-direction) from throw to catch.

#-------------------------------------------------------------------------------------------------------

  
#-------------------------------------------------------------------------------------------------------
### Avg. speed before vs after throw
avg_speed_change <- deep_go_plays |> 
  group_by(game_id, play_id) |>
  summarise(
    avg_speed_pre_throw = mean(s[pre_throw_frame == 1], na.rm = TRUE),
    avg_speed_post_throw = mean(s[pre_throw_frame == 0], na.rm = TRUE),
    speed_change = avg_speed_post_throw - avg_speed_pre_throw,
    .groups = "drop"
  ) |> select(game_id, play_id, speed_change)

# Negative speed change: The receiver slowed down after the throw compared to before it.
# Positive speed change: The receiver sped up after the throw compared to before it.
  
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Mean Turning Rate while ball-in-air
turning_rate <- deep_go_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  mutate(delta_dir = dir - lag(dir)) |> 
  summarise(avg_turn_rate = mean(abs(delta_dir) / 10, na.rm = TRUE), .groups = "drop")
  
# High turning rate: The receiver was making large or frequent directional changes 
# while the ball was in the air — lots of adjustment, possibly reacting to ball trajectory or defender.
# Low turning rate: The receiver ran very straight with minimal directional change, 
# indicating a clean, uninterrupted path to the ball.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Play result
play_result <- deep_go_plays |> 
  group_by(game_id, play_id) |>
  summarise(pass_result = unique(pass_result), .groups = "drop")

# C: complete
# I: incomplete
# IN: interception

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Deceleration at catch
catch_deceleration <- deep_go_plays |>
  group_by(game_id, play_id) |>
  slice_tail(n = 5) |>
  summarise(
    decel_at_catch = round(first(smoothed_a) - last(smoothed_a), 4),
    .groups = "drop"
  )

# decel_at_catch > 0: The receiver slowed down approaching the catch.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Acceleration while ball-in-air
avg_acceleration <- deep_go_plays |>
  filter(pre_throw_frame == 0) |> 
  group_by(game_id, play_id) |>
  summarise(
    avg_abs_accel = mean(smoothed_a, na.rm = TRUE),
    max_accel = max(smoothed_a, na.rm = TRUE),
    .groups = "drop"
  )

# both: Higher value shows that the receiver was changing speed more aggressively while the ball was in the air.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Speed volatility with ball-in-air
speed_volatility <- deep_go_plays |>
  filter(pre_throw_frame == 0) |>
  group_by(game_id, play_id) |>
  summarise(
    speed_cv = sd(s, na.rm = TRUE) / mean(s, na.rm = TRUE), #coefficient of variation
    .groups = "drop"
  )

# High value means the receiver’s speed fluctuated a lot, low values mean the speed was consistent during the play.

#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
### Momentum ratio
momentum <- deep_go_plays |> 
  group_by(game_id, play_id) |> 
  summarise(
    momentum_at_throw = first(player_weight) * s[throw_frame == 1],
    momentum_at_catch = first(player_weight) * s[last(frame_id)],
    momentum_ratio = momentum_at_catch / momentum_at_throw,
    .groups = "drop"
  ) |> select(game_id, play_id, momentum_ratio)

# How much the receiver’s momentum changed during the ball flight.
# 1.0 = in stride, <1 = slowed for the ball, >1 = sped up to chase the ball.

#-------------------------------------------------------------------------------------------------------



deep_go_summary <- play_result |> 
  left_join(angle_between, by = c('game_id', 'play_id')) |>
  left_join(num_dir_changes, by = c('game_id', 'play_id')) |> 
  left_join(dir_diff, by = c('game_id', 'play_id')) |> 
  left_join(avg_speed_change, by = c('game_id', 'play_id')) |> 
  left_join(turning_rate, by = c('game_id', 'play_id')) |> 
  left_join(catch_deceleration, by = c('game_id', 'play_id')) |>
  left_join(avg_acceleration, by = c('game_id', 'play_id')) |> 
  left_join(speed_volatility, by = c('game_id', 'play_id')) |>
  left_join(momentum, by = c('game_id', 'play_id')) 
  


corr_matrix <- cor(deep_go_summary |> select(-game_id, -play_id, -pass_result))

# momentum and avg_abs_accel with 0.9 corr

# compare score with cp (completion probability) or cpoe in nflreadr

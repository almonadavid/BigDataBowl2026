
library(tidyverse)

main_data <- suppressMessages(read.csv('data/main_data.csv'))

## Note:
# The data does not contain all 22 players.
# The tracking data only includes the following players:
#   - The Targeted receiver
#   - Defensive Players: 
#     - Within 5 yards of player at pass release OR 
#     - Able to reach land location of the ball, given a speed of 12 yds/second and the observed airtime of the pass
# These filters will sometimes include defensive players who do not cover the targeted receiver or go to the ball. When predicting a real play, it is important to include these players as they could cover the targeted receiver, even though they do not in the given case.


## CASE STUDY: game_id: 2023100806, play_id: 1892

# Isolating the play descriptions of 'GO' routes
description_go_route <- main_data |> 
  group_by(game_id, play_id) |> 
  slice(1) |> 
  ungroup() |> 
  filter(route_of_targeted_receiver == 'GO') |> # deep routes: 'GO', 'CORNER', 'POST' (WHEEL...??)
  select(game_id, play_id, play_description)



# Play Description: (1:07) (Shotgun) P.Mahomes pass deep middle to M.Valdes-Scantling to DET 30 for 34 yards (J.Jacobs).
play_one <- main_data |> 
  filter(game_id == 2023090700 & play_id == 1679 & player_role == 'Targeted Receiver')

# Speed-Time Plot
ggplot(data = play_one) +
  # geom_line(aes(x = frame_id, y = s)) +
  geom_smooth(aes(x = frame_id, y = s), method = 'gam', se = FALSE) +
  geom_vline(xintercept = play_one$frame_id[play_one$throw_frame == 1], color = 'red', linetype = 'dashed') +
  theme_bw() +
  labs(y = 'Speed', x = 'Frames')

# Acceleration-Time Plot
ggplot(data = play_one) +
  # geom_line(aes(x = frame_id, y = a)) +
  geom_smooth(aes(x = frame_id, y = a), method = 'gam', se = FALSE) +
  geom_vline(xintercept = play_one$frame_id[play_one$throw_frame == 1], color = 'red', linetype = 'dashed') +
  theme_bw() +
  labs(y = 'Acceleration', x = 'Frames')




## the idea is that a perfect QB throw should land to the receiver in stride, not breakage.

# get different variables, then k-means cluster like John NY Giants metric



# Play Desc: (11:21) (No Huddle, Shotgun) K.Cousins pass deep middle to J.Addison for 39 yards, TOUCHDOWN. Penalty on TB-J.Dean, Illegal Contact, declined.
play_two <- main_data |> 
  filter(game_id == 2023091004 & play_id == 1326 & player_role == 'Targeted Receiver')


# Speed-Time Plot
ggplot(data = play_two) +
  # geom_line(aes(x = frame_id, y = s)) +
  geom_smooth(aes(x = frame_id, y = s), method = 'gam', se = FALSE) +
  geom_vline(xintercept = play_two$frame_id[play_two$throw_frame == 1], color = 'red', linetype = 'dashed') +
  theme_bw() +
  labs(y = 'Speed', x = 'Frames')

# Acceleration-Time Plot
ggplot(data = play_two) +
  # geom_line(aes(x = frame_id, y = a)) +
  geom_smooth(aes(x = frame_id, y = a), method = 'gam', se = FALSE) +
  geom_vline(xintercept = play_two$frame_id[play_two$throw_frame == 1], color = 'red', linetype = 'dashed') +
  theme_bw() +
  labs(y = 'Acceleration', x = 'Frames')

play_two <- play_two |> 
  mutate(
    dir_bin = case_when(
      dir < 0 | dir > 180 ~ 'backwards',
      between(dir, 0, 36 - 1e-9) | between(dir, 144, 180) ~ 'steep inside/outside',
      between(dir, 36, 72 - 1e-9) | between(dir, 108, 144 - 1e-9) ~ 'inside/outside',
      between(dir, 72, 108 - 1e-9) ~ 'forward'
    )
  ) |> 
  relocate(dir_bin, .after = dir)





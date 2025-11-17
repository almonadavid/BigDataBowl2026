
library(tidyverse)
library(gganimate)
library(matlib)
library(pracma)
library(future)
library(sf)


###### Field control Final #####

# Initialize an empty dataframe so that it's in the global environment
week_track <- data.frame()
play <- data.frame()
game <- data.frame()
frame <- data.frame()


radius_calc <- function(dist_to_ball) {
  #' @param dist_to_ball: the distance between a player and the ball
  #' @return the player's influence radius with respect to the ball (or QB?)

  return(4 + 6 * (dist_to_ball >= 15) + (dist_to_ball ** 3) / 560 * (dist_to_ball < 15))
  }


player_influence <- function(nfl_id, x_point, y_point, frame) {
  #' @param nfl_id: the ID of the player whose influence we are calculating
  #' @param x_point: X coordinate of the point we want to calculate influence at
  #' @param y_point: Y coordinate of the point we want to calculate influence at
  #' @return player_influence: the specified player's influence at point (x_point, y_point)
  #'
  #' A huge thanks is owed to the creator of this kaggle notebook: https://www.kaggle.com/code/pednt9/vip-hint-coded/notebook
  #' Below is their code repurposed from Python to R

  # TODO
  player_frame <- frame |>
    filter(nflId == nfl_id)
  
  ball_frame <- frame |>
    filter(team == "football")
  
  # This will follow the space control defined by Bornn and Fernandez (2018), with minor adjustments to fit our data better
  # Need the basic player and ball information
  point <- c(x_point, y_point)
  player_x <- player_frame |> pull(x) |> first() # first() just in case there's a very weird error, might not even be necessary
  player_y <- player_frame |> pull(y) |> first()
  player_s <- player_frame |> pull(s) |> first()
  player_team <- player_frame |> pull(team) |> first()
  player_dir <- player_frame |> pull(dir) |> first()
  ball_x <- ball_frame |> pull(x) |> first()
  ball_y <- ball_frame |> pull(y) |> first()
  
  player_s_ratio <- player_s^2 / 13^2 # Might want to change this value at some point
  
  # We'll need these values as vectors for the long matrix multiplication later
  player_point <- c(player_x, player_y)
  #player_speed <- c(player_s_x, player_s_y)
  
  # Need the euclidean distance from the player to the ball
  player_ball_dist <- sqrt((player_x - ball_x)^2 + (player_y - ball_y)^2)
  influence_radius <- radius_calc(player_ball_dist)
  
  # Now we need to initialize some of the intermediate matrices we'll use
  R <- matrix(data = c(cos(player_dir), sin(player_dir), -sin(player_dir), cos(player_dir)), ncol = 2, nrow = 2)
  S <- matrix(data = c(influence_radius + (influence_radius * player_s_ratio), 0, 0, influence_radius - (influence_radius * player_s_ratio)), ncol = 2, nrow = 2) # This could be speed in x/y direction instead?
  
  # Matrix multiplication
  COV <- ((R %*% S) %*% S) %*% matlib::inv(R)
  
  # Find mu which is essentially an expected end point for the player in question
  mu <- player_point + (c(cos(player_dir), sin(player_dir)) * 0.1 * player_s)
  
  norm_fact <- (1 / sqrt((2 * pi)^2 * matlib::Det(COV)))
  intermed_scalar_player <- (player_point - mu) %*% matlib::inv(COV) %*% (player_point - mu)
  
  this_player_influence <- norm_fact * exp(-0.5 * intermed_scalar_player[1,1])
  
  # TODO - CHANGE THIS TO NOT BE EMPTY
  
  intermed_scalar_point <- (point - mu) %*% matlib::inv(COV) %*% (point - mu)
  
  this_point_influence <- norm_fact * exp(-0.5 * intermed_scalar_point[1,1])
  
  influence_value <- this_point_influence / this_player_influence
  
  
  player_frame_influence <- data.frame(nflId = nfl_id, team = player_team, x = x_point, y = y_point, influence = influence_value) # CHANGE THIS FROM 0
  
  
  
  return(player_frame_influence)
  }


find_frame_influence <- function(frame_id, play) {
  #' @param frame_id: the frame ID for a given play that we are calculating player influence for
  #' @param play: the tracking data for the play
  #' @return a dataframe of each player's influence at a given point

  frame <- play |>
    filter(frameId == frame_id)
  
  frame |>
    filter(pff_role == "Pass Block" | team == "football") |>
    pull(x) -> pocket_points_x
  
  frame |>
    filter(pff_role == "Pass Block" | team == "football") |>
    pull(y) -> pocket_points_y
  
  pocket_points_x <- append(pocket_points_x, pocket_points_x[1])
  pocket_points_y <- append(pocket_points_y, pocket_points_y[1])
  
  pocket_points <- list(cbind(x = pocket_points_x, y = pocket_points_y))
  
  pocket <- st_polygon(pocket_points)
  
  convex_hull <- st_convex_hull(pocket)
  
  convex_hull_points <- st_make_grid(convex_hull, what = "centers")
  
  convex_hull_points <- convex_hull_points[convex_hull]
  all_points <- map(.x = convex_hull_points, .f = st_coordinates)
  
  all_points <- do.call(rbind, all_points) |>
    as.data.frame()
  
  los <- frame |>
    pull(los) |>
    first()
  
  ball_y <- frame |>
    filter(team == "football") |>
    pull(y) |> first()
  
  ball_x <- frame |>
    filter(team == "football") |>
    pull(x) |> first()
  
  # x_max <- min(ball_x + 5, 120)
  # x_min <- max(ball_x - 5, 0)
  # y_max <- min(ball_y + 5, 160/3) # Window is too big as of right now
  # y_min <- max(ball_y - 5, 0)
  
  this_frame_player_ids <- frame |>
    filter(!is.na(nflId)) |>
    pull(nflId) |>
    unique()
  
  player_id_sequence <- c()
  for (i in 1:length(this_frame_player_ids)) {
    player_id_sequence <- c(player_id_sequence, rep(this_frame_player_ids[i], length(all_points$X)))
    }
  
  all_points_x <- rep(all_points$X, length(this_frame_player_ids))
  all_points_y <- rep(all_points$Y, length(this_frame_player_ids))
  
  grid <- data.frame(player_id = player_id_sequence, x_point = all_points_x, y_point = all_points_y)
  
  future::plan("multisession")
  all_players_frame_influence <- furrr::future_pmap_dfr(.l = list(grid$player_id, grid$x_point, grid$y_point), .f = player_influence, frame = frame) |>
    mutate(frameId = frame_id)
  
  
  # TODO - there will have to be a little bit more work done right here to establish the amount of space "owned" by a player
  # As of this point there's one observation per player per point, should be aggregated down to one observation per player
  
  return(all_players_frame_influence)
  }

#'
#' This will take the play ID and then create a grid of points that we are interested in (3 yards beyond LOS and 7 yards behind, 5 yards to left or right)
#' The field control for each player will be calculated at each point
#' 
#'
#' @param play_id: the ID of the play we're passing to `player_influence`
#' @param game: the tracking data for the game
#' @param game_id: the ID of the game the play is in. Handed from `find_game_influence`
#'
find_play_influence <- function(play_id, game) {
  # TODO
  
  play <- game |>
    filter(playId == play_id)
  
  this_play_frame_ids <- play |>
    pull(frameId) |>
    unique()
  # first find the IDs for players we're interested in for this play
  future::plan("multisession")
  all_players_play_influence <- furrr::future_map_dfr(.x = this_play_frame_ids, .f = find_frame_influence, play = play) |>
    mutate(playId = play_id)
  
  # TODO - CHANGE THIS TO NOT BE EMPTY
  return(all_players_play_influence)
  }



find_game_influence <- function(game_id, week_track) {
  #' @param game_id: the ID of the game we are finding field control for
  #' @param week_track: the tracking data for the week
  #' @return dataframe of player ID, frame ID, player ID, and field control for given points

  # TODO
  
  game <- week_track |>
    filter(gameId == game_id)
  
  this_game_play_ids <- game |>
    pull(playId) |>
    unique()
  
  game_influence <- furrr::future_map_dfr(.x = this_game_play_ids, .f = find_play_influence, game = game) |>
    mutate(gameId = game_id)
  
  # TODO - CHANGE THIS TO NOT BE EMPTY
  return(game_influence)
  }


find_week_influence <- function(week_track) {
  #' @param week_track: tracking data for a given week
  #' @return concatenated rows from calling find_game_influence multiple times

  # TODO - this should be done, double check at the end
  # First, standardize the coordinates and the direction and orientation so all the plays are going in the same direction
  play_end_events <- c("pass_forward", "autoevent_passforward", "run", "qb_sack", "qb_strip_sack", "out_of_bounds",
                       "fumble_offense_recovered", "handoff", "fumble", "first_contact") # handoff is questionable. since we're concerned with pass protection, handoff could indicate a trick play
  week_track <- week_track |>
    mutate(x = ifelse(playDirection == "left", 120 - x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y),
           dir = case_when(playDirection == "left" & dir <= 180 ~ dir + 180,
                           playDirection == "left" & dir > 180 ~ dir - 180,
                           T ~ dir),
           o = case_when(playDirection == "left" & o <= 180 ~ o + 180,
                         playDirection == "left" & o > 180 ~ o - 180,
                         T ~ o),
           dir = dir / 180 * pi,
           o = o / 180 * pi) |>
    left_join(pff) |> # Now add the info from PFF and the play by play info
    left_join(plays) |>
    group_by(gameId, playId) |>
    mutate(rushing_qb = pff_role == "Pass Rush",
           snap_frame = max(frameId[event == "ball_snap"]),
           end_frame = first(frameId[event %in% play_end_events]),
           los = first(x[team == "football" & frameId == snap_frame]),
           end_box_up = max(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T),
           end_box_down = min(y[pff_role == "Pass Block" & frameId == snap_frame], na.rm = T)) |>
    ungroup() |>
    filter(pff_role %in% c("Pass Rush", "Pass Block") | team == "football") |> # Only keep the blockers, rushers, and the ball 
    filter(frameId == snap_frame | frameId == end_frame) |>
    group_by(gameId, playId) |>
    mutate(n_frames = length(unique(frameId))) |>
    filter(n_frames == 2) |>
    select(-n_frames)
  
  this_week_game_ids <- week_track |>
    pull(gameId) |>
    unique()
  
  week_field_control <- furrr::future_map_dfr(.x = this_week_game_ids, .f = find_game_influence, week_track = week_track)
  
  return(week_field_control)
}

week_track <- read_csv("week1.csv")
pff <- read_csv("pffScoutingData.csv")
plays <- read_csv("plays.csv")
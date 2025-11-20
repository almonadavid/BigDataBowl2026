

# https://www.kaggle.com/code/pablollanderos33/generate-play-animations-with-ggplot/notebook


fetch_play <- function(df, playid, gameid) {
  play <- df |> 
    filter(game_id == gameid & play_id == playid)
  return(play)
}


## Plotting the field
plot_field <- function(field_color="#ffffff", line_color = "#000000") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      legend.title.align = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = field_color, color = "black"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 7,
      family = "mono",
      colour = line_color
    ) +
    
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 7,
      family = "mono",
      colour = line_color
    )
  
  return(field)
}

## Plotting one frame of a play
plot_frame <- function(one_play, frame, plot_vel = F) {
  #  Take one frame from one play, plot a scatter plot image.
  # Check for data
  if(is.null(one_play)) {
    print("error: need to provide play data")
    return()
  }
  if(is.null(frame)) {
    print("error: need to provide frame of play to visualize")
    return()
  }
  # * get play metadata ----
  play_desc <- one_play$play_description[1]
  play_dir <- one_play$play_direction[1]
  yards_togo <- one_play$yards_to_go[1]
  los <- one_play$absolute_yardline_number[1]
  togo_line <- los + yards_togo
  
  fr <- one_play  |>  
    filter(frame_id == frame)
  
  colores <- unique(fr$color1)
  names(colores) <- colores
  
  #  velocity angle in radians
  fr$dir_rad <- fr$dir * pi / 180
  
  #  velocity components
  fr$v_x <- sin(fr$dir_rad) * fr$s
  fr$v_y <- cos(fr$dir_rad) * fr$s
  
  if(plot_vel == T) {
    # one frame scatterplot
    one_frame <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = fr,
        mapping = aes(x = x, y = y, color = color1 )
      ) +
      geom_segment(
        data = fr,
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y, color =color1 ),
        linewidth = 1, arrow = arrow(length = unit(0.1, "cm"))
      ) + 
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2026"
      ) +
      theme(legend.position="none")
  }
  else {
    # one frame scatterplot
    one_frame <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = fr,
        mapping = aes(x = x, y = y, color = color1 )
      ) +
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2026"
      ) +
      theme(legend.position="none")
  }
  
  return(one_frame)
}

# ---- establish game and play to plot ----
playid = 1704  #736 # YT timestamp ... 
gameid = 2023092403 #2023090700
# YT link: ...
#-------------------------------------------

# exctract play df ----
play <- fetch_play(main_data, playid, gameid)

print(play$play_description[1])

# # plot a specific frame
# plot_frame(play, 1, TRUE)


## Animating a play
play_animation <- function(one_play, plot_vel = F) {
  
  # Check for data
  if(is.null(one_play)) {
    print("error: need to provide play data")
    return()
  }
  
  # Get play metadata
  play_desc <- one_play$play_description[1]
  play_dir <- one_play$play_direction[1]
  yards_togo <- one_play$yards_to_go[1]
  los <- one_play$absolute_yardline_number[1]
  togo_line <- los + yards_togo
  
  colores <- unique(one_play$color1)
  names(colores) <- colores
  
  # Velocity angle in radians
  one_play$dir_rad <- one_play$dir * pi / 180
  
  # Velocity components
  one_play$v_x <- sin(one_play$dir_rad) * one_play$s
  one_play$v_y <- cos(one_play$dir_rad) * one_play$s
  
  if (plot_vel == T) {
    anim <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      ) +
      # Trail points w/ no border
      geom_point(
        data = one_play,
        mapping = aes(x = x, y = y, fill = color1, color = color1),  # color too for trail
        size = 4, shape = 21, stroke = 0
      ) +
      geom_point(
        data = one_play,
        mapping = aes(x = x, y = y, fill = color1),
        size = 4, shape = 21, color = 'black', stroke = 0.6
      ) +
      geom_segment(
        data = one_play,
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y, color = color1),
        linewidth = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) + 
      scale_fill_manual(values = colores) +
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2026"
      ) +
      theme(legend.position="none") +
      # animation stuff
      transition_time(frame_id) +
      ease_aes('linear') +
      shadow_trail(distance = 0.009, alpha = 0.8, size = 1.5, exclude_layer = c(8, 9))
    # shadow_wake(wake_length = 1, size = FALSE, alpha = 0.2, exclude_layer = 8, wrap = FALSE)
  }
  
  else {
    anim <- plot_field() +
      # line of scrimmage
      annotate(
        "segment",
        x = los, xend = los, y = 0, yend = 160/3,
        colour = "#0d41e1"
      ) +
      # 1st down marker
      annotate(
        "segment",
        x = togo_line, xend = togo_line, y = 0, yend = 160/3,
        colour = "#f9c80e"
      )+
      geom_point(
        data = one_play,
        mapping = aes(x = x, y = y, color = color1),
        size = 4
      ) +
      scale_colour_manual(values = colores) +
      labs(
        title = play_desc,
        caption = "Data: Big Data Bowl 2026"
      ) +
      theme(legend.position="none") +
      # animation stuff
      transition_time(frame_id) +
      ease_aes('linear') +
      shadow_mark(linetype = 'solid', size = 0.75)
  }
  return(anim)
}


# *ensure length of play matches number of frames
play_length <- length(unique(play$frame_id))

# customize duration of gif
duration = 10

# Animate our play
p_anim <- animate(
  play_animation(play, plot_vel = T),
  duration = duration,
  fps = 15, 
  nframes = play_length,
  width = 850,
  height = 450,
  end_pause = 10,
  renderer = gifski_renderer()
)
p_anim

anim_save("play_with_trails.gif", p_anim)
# anim_save("play_TRUE.gif", p_anim)


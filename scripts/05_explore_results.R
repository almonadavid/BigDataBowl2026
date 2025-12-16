
library(tidyverse)
library(data.table)
library(gt)
library(ggridges)
library(nflreadr)
library(magick)

source("scripts/04_placement_quality.R")
# ball_placement_quality <- suppressMessages(fread('data/ball_placement_quality.csv'))


## distribution of placement score ------------------------------------------------
ball_placement_quality |>
  ggplot(aes(x = placement_score)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(placement_score, na.rm = TRUE), color = "Mean"), linetype = "solid") +
  geom_vline(aes(xintercept = median(placement_score, na.rm = TRUE), color = "Median"), linetype = "dashed") +
  scale_color_manual(
    name = "",
    values = c("Mean" = "blue", "Median" = "red")
  ) +
  labs(
    x = "Placement Score",
    y = "Density"
  ) +
  theme_bw(base_size = 15)



## placement score by pass result -------------------------------------------------
ball_placement_quality |> 
  left_join(play_result, by = c('game_id', 'play_id')) |>
  group_by(pass_result) |> 
  summarise(
    Total = n(),
    `Average Placement Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> rename("Pass Result" = "pass_result") |> 
  gt()

ball_placement_quality |> 
  left_join(play_result, by = c('game_id', 'play_id')) |>
  drop_na() |> 
  mutate(pass_result = factor(case_when(
    pass_result == "I" ~ "Incomplete",
    pass_result == "IN" ~ "Interception", 
    pass_result == "C" ~ "Complete",
    TRUE ~ pass_result
  ), levels = c("Interception", "Incomplete", "Complete"))) |> 
  ggplot(aes(x = placement_score, y = pass_result, fill = pass_result)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
  labs(
    title = "Distribution of Placement Score by Pass Result",
    x = "Placement Score", 
    y = "Pass Result",
    caption = "Quantile line = median"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")



## placement score by QB ----------------------------------------------------------
play_passer <- arrow::read_parquet("data/pre_throw_tracking.parquet") |> 
  filter(player_role == "Passer") |> 
  distinct(game_id, play_id, .keep_all = TRUE) |> 
  transmute(
    game_id,
    play_id,
    passer = player_name
  )

# controlling for pass length, receiver route and team coverage type
control_lm <- lm(placement_score ~ pass_length + route_of_targeted_receiver + team_coverage_type, data = ball_placement_quality)

ball_placement_quality <- ball_placement_quality |>
  mutate(
    expected_placement = predict(control_lm, newdata = pick(everything())),
    placement_over_expected = placement_score - expected_placement
  )

qb_ranks <- ball_placement_quality |>
  left_join(play_passer, by = c('game_id', 'play_id')) |>
  group_by(passer) |>
  summarise(
    possession_team = first(possession_team),
    n = n(),
    mean_placement = mean(placement_score, na.rm = TRUE),
    adj_placement = mean(placement_over_expected, na.rm = TRUE)
  ) |>
  filter(n >= 200) |> # min. 200 pass attempts
  arrange(desc(adj_placement)) |> 
  mutate(rank = row_number()) |> relocate(rank) |> 
  gt() |> 
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(mean_placement),
    decimals = 2,
  ) |>
  fmt_number(
    columns = c(adj_placement),
    decimals = 2,
  ) |>
  data_color(
    columns = c(adj_placement),
    colors = scales::col_numeric(
      palette = c("#DEEBF7", "#08306B"),
      domain = NULL
    )
  ) |> 
  cols_label(
    rank = md("**Rank**"),
    passer = md("**Player**"),
    possession_team = md("**Team**"),
    n = md("**Total Pass Attempts**"),
    mean_placement = md("**Placement Score**"),
    adj_placement = md("**POE**")
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_header(md("**QB Rankings**"),
             md("(Minimum 200 pass attempts)")) |> 
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Average placement score over expected across all pass attempts",
    locations = cells_column_labels(
      columns = adj_placement
    )
  )

gtsave(qb_ranks, "viz/qb_ranks.png")


## placement score by receiver ----------------------------------------------------
bottom_receiver_ranks <- ball_placement_quality |>
  group_by(player_name) |>
  summarise(
    possession_team = first(possession_team),
    player_position = first(player_position),
    n = n(),
    mean_placement = mean(placement_score, na.rm = TRUE)
  ) |>
  filter(n >= 50) |>
  arrange(mean_placement) |>
  mutate(rank = row_number()) |> relocate(rank) |> 
  slice_head(n=20) |> 
  gt() |> 
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(mean_placement),
    decimals = 2,
  ) |>
  data_color(
    columns = c(mean_placement),
    colors = scales::col_numeric(
      palette = c("#67000D", "#FEE0D2"),
      domain = NULL
    )
  ) |> 
  cols_label(
    rank = md("**Rank**"),
    player_name = md("**Player**"),
    player_position = md("**Position**"),
    possession_team = md("**Team**"),
    n = md("**Total Targeted Passes**"),
    mean_placement = md("**Placement Score**")
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_header(md("**Highest Receiver Adjustment Burden**"),
             md("(Minimum 50 targeted passes)")) |> 
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Average placement score across all targeted passes",
    locations = cells_column_labels(
      columns = mean_placement
    )
  )

gtsave(bottom_receiver_ranks, "viz/bottom_receiver_ranks.png")


top_receiver_ranks <- ball_placement_quality |>
  group_by(player_name) |>
  summarise(
    possession_team = first(possession_team),
    player_position = first(player_position),
    n = n(),
    mean_placement = mean(placement_score, na.rm = TRUE)
  ) |>
  filter(n >= 50) |>
  arrange(desc(mean_placement)) |>
  mutate(rank = row_number()) |> relocate(rank) |> 
  slice_head(n=20) |> 
  gt() |> 
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(mean_placement),
    decimals = 2,
  ) |>
  data_color(
    columns = c(mean_placement),
    colors = scales::col_numeric(
      palette = c("#e4e4d9", "#215f00"),
      domain = NULL
    )
  ) |> 
  cols_label(
    rank = md("**Rank**"),
    player_name = md("**Player**"),
    player_position = md("**Position**"),
    possession_team = md("**Team**"),
    n = md("**Total Targeted Passes**"),
    mean_placement = md("**Placement Score**")
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_header(md("**Lowest Receiver Adjustment Burden**"),
             md("(Minimum 50 targeted passes)")) |> 
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title")) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_footnote(
    footnote = "Average placement score across all targeted passes",
    locations = cells_column_labels(
      columns = mean_placement
    )
  )

gtsave(top_receiver_ranks, "viz/top_receiver_ranks.png")

# combine qb and receiver tables
combined <- image_append(c(image_read("viz/top_receiver_ranks.png"), image_read("viz/bottom_receiver_ranks.png")))
image_write(combined, "viz/receiver_ranks.png")

## placement score by route --------------------------------------------------------
ball_placement_quality |>
  group_by(route_of_targeted_receiver) |> 
  drop_na() |> 
  summarise(
    Total = n(),
    `Average Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> arrange(desc(`Average Score`)) |> 
  rename("Receiver's Route" = "route_of_targeted_receiver") |> 
  gt()


## placement score by coverage type --------------------------------------------------------
ball_placement_quality |>
  group_by(team_coverage_man_zone) |> 
  drop_na() |> 
  summarise(
    Total = n(),
    `Average Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> arrange(desc(`Average Score`)) |> 
  rename("Coverage Type" = "team_coverage_man_zone") |> 
  gt()


## placement score by pass length --------------------------------------------------
ball_placement_quality |> 
  mutate(
    pass_bin = case_when(
      pass_length < 0 ~ 'Behind LOS',
      pass_length < 15 ~ '0-14',
      pass_length <= 25 ~ '15-25',
      pass_length <= 40 ~ '26-40',
      TRUE ~ '40+'
    )
  ) |> 
  drop_na() |> 
  mutate(pass_bin = factor(pass_bin, levels = c("40+", "26-40", "15-25", '0-14', 'Behind LOS'))) |> 
  ggplot(aes(x = placement_score, y = pass_bin, fill = pass_bin)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
  labs(
    title = "Distribution of Placement Score by Pass Length",
    x = "Placement Score", 
    y = "Pass Length (yards)",
    caption = "Quantile line = median"
  ) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")


## placement score vs. completion probability from nflreadr
# dictionary: https://nflreadr.nflverse.com/articles/dictionary_pbp.html
pbp_pass <- load_pbp(c(2023)) |>
  progressr::with_progress() |> 
  filter(play_type == "pass" & lateral_reception == 0) |>
  select(game_id = old_game_id, play_id, season, cp) |>
  mutate(game_id = as.double(game_id))


left_join(ball_placement_quality, pbp_pass, by = c('game_id', 'play_id')) |>
  drop_na() |> 
  ggplot(aes(x = placement_score, y = cp)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  labs(
    x = "Placement Score",
    y = "Completion Probability"
  ) +
  theme_bw(base_size = 17) +
  ggpubr::stat_cor(method = "pearson")

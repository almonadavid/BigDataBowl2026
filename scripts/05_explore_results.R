
library(tidyverse)
library(data.table)
library(gt)
library(ggridges)
library(nflreadr)
library(magick)
library(car)

options(scipen = 999)

source("scripts/04_placement_quality.R")
# OR just load saved dataset
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
  group_by(pass_result) |> 
  summarise(
    Total = n(),
    `Average Placement Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> rename("Pass Result" = "pass_result") |> 
  gt()

ball_placement_quality |> 
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
# summary(control_lm)
# vif(control_lm)

ball_placement_quality <- ball_placement_quality |>
  mutate(
    expected_placement = predict(control_lm, newdata = pick(everything())),
    placement_over_expected = placement_score - expected_placement
  )

qb_ranks <- ball_placement_quality |>
  mutate(
    pass_bin = factor(case_when(
      pass_length < 10 ~ 'Short',
      pass_length < 20 ~ 'Medium',
      TRUE ~ 'Deep')
    )
  ) |>
  group_by(passer) |>
  summarise(
    possession_team = first(possession_team),
    n = n(),
    mean_placement = round(mean(placement_score, na.rm = TRUE), 2),
    adj_placement = round(mean(placement_over_expected, na.rm = TRUE), 2),
    n_short = sum(pass_bin == "Short"),
    adj_placement_short = round(mean(placement_over_expected[pass_bin == "Short"], na.rm = TRUE), 2),
    n_medium = sum(pass_bin == "Medium"),
    adj_placement_medium = round(mean(placement_over_expected[pass_bin == "Medium"], na.rm = TRUE), 2),
    n_deep = sum(pass_bin == "Deep"),
    adj_placement_deep = round(mean(placement_over_expected[pass_bin == "Deep"], na.rm = TRUE), 2)
  ) |>
  filter(n >= 150) |>
  arrange(desc(adj_placement)) |> 
  mutate(rank = row_number()) |> 
  relocate(rank) |> 
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
  data_color(
    columns = starts_with("adj_placement"),
    colors = scales::col_numeric(
      palette = c("#DEEBF7", "#08306B"),
      domain = NULL
    )
  ) |>  
  tab_spanner(label = md("**Overall**"), columns = c(n, mean_placement, adj_placement)) |>
  tab_spanner(label = md("**Short (< 10 yds)**"), columns = c(n_short, adj_placement_short)) |>
  tab_spanner(label = md("**Medium (10-19 yds)**"), columns = c(n_medium, adj_placement_medium)) |>
  tab_spanner(label = md("**Deep (20+ yds)**"), columns = c(n_deep, adj_placement_deep)) |>
  cols_label(
    rank = md("**Rank**"),
    passer = md("**Player**"),
    possession_team = md("**Team**"),
    n = md("**Total Pass Attempts**"),
    mean_placement = md("**Placement Score**"),
    adj_placement = md("**POE**"),
    n_short = md("**Total Attempts**"),
    adj_placement_short = md("**POE**"),
    n_medium = md("**Total Attempts**"),
    adj_placement_medium = md("**POE**"),
    n_deep = md("**Total Attempts**"),
    adj_placement_deep = md("**POE**")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  tab_header(
    md("**QB Rankings**"),
    md("(Minimum 150 pass attempts)")
  ) |> 
  tab_style(
    style = cell_borders(sides = "top"),
    locations = cells_title("title")) |> 
  tab_options(table.border.top.style = "a") |> 
  tab_footnote(
    footnote = "Average placement score over expected across all pass attempts",
    locations = cells_column_labels(
      columns = adj_placement
    )
  )

gtsave(qb_ranks, "viz/qb_ranks.png", vwidth = 1400)


## placement score by player position ---------------------------------------------
ball_placement_quality |>
  filter(player_position %in% c("TE", "RB", "WR")) |> 
  mutate(player_position = factor(player_position, levels = c("WR", "TE", "RB"))) |> 
  ggplot(aes(x = placement_score, y = player_position, fill = player_position)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
  labs(
    title = "Distribution of Placement Score by Player Position",
    x = "Placement Score", 
    y = "Player Position",
    caption = "Quantile line = median"
  ) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")



## placement score by receiver ----------------------------------------------------
lower_quartile <- -1.50 # rounded up from -1.41 quantile(ball_placement_quality$placement_score, 0.25)

top20_receiver_ranks <- ball_placement_quality |> 
  filter(player_position == 'WR') |> 
  group_by(player_name) |>
  summarise(
    possession_team = first(possession_team),
    n = n(),
    difficult_targets = sum(placement_score <= lower_quartile),
    difficult_catches = sum(placement_score <= lower_quartile & pass_result == 'C'),
    difficult_catch_pct = round(difficult_catches / difficult_targets * 100, 2)
  ) |>
  filter(n >= 50) |>
  arrange(desc(difficult_catch_pct)) |>
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
  data_color(
    columns = c(difficult_catch_pct),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  cols_label(
    rank = md("**Rank**"),
    player_name = md("**Player**"),
    possession_team = md("**Team**"),
    n = md("**Total Targeted Passes**"),
    difficult_catch_pct = md("**% Difficult Completions**"),
    difficult_targets = md("**Difficult Passes**"),
    difficult_catches = md("**Difficult Catches**")
  ) |> 
  cols_align(align = "center", columns = everything()) |> 
  tab_header(
    md("**Top 20 WRs on Difficult Passes**"),
    md("(Minimum 50 targeted passes)")
    ) |> 
  tab_style(
    style = cell_borders(sides = "top"),
    locations = cells_title("title")
    ) |> 
  tab_options(table.border.top.style = "a") |> 
  tab_footnote(
    footnote = "Total number of pass targets with a placement score below the lower quartile of -1.50",
    locations = cells_column_labels(columns = difficult_targets)
    ) |> 
  tab_footnote(
    footnote = "Total number of catches for passes with a placement score below the lower quartile of -1.50",
    locations = cells_column_labels(columns = difficult_catches)
  )

gtsave(top20_receiver_ranks, "viz/top20_receiver_ranks.png")

## placement score by route --------------------------------------------------------
ball_placement_quality |>
  group_by(route_of_targeted_receiver) |> 
  drop_na() |> 
  summarise(
    Total = n(),
    `Placement Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> arrange(desc(`Placement Score`)) |> 
  rename("Receiver's Route" = "route_of_targeted_receiver") |> 
  gt()


## placement score by coverage type --------------------------------------------------------
ball_placement_quality |>
  group_by(team_coverage_man_zone) |> 
  drop_na() |> 
  summarise(
    Total = n(),
    `Placement Score` = round(mean(placement_score, na.rm = TRUE), 2)
  ) |> arrange(desc(`Placement Score`)) |> 
  rename("Coverage Type" = "team_coverage_man_zone") |> 
  gt()


# ball_placement_quality |>
#   ggplot(aes(x = placement_score, y = reorder(route_of_targeted_receiver, placement_score, FUN = median), fill = route_of_targeted_receiver)) +
#   geom_density_ridges(alpha = 0.7, scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
#   scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
#   theme_bw(base_size = 15) +
#   theme(legend.position = "none")

## placement score by pass length --------------------------------------------------
ball_placement_quality |> 
  mutate(
    pass_bin = case_when(
      pass_length < 0 ~ 'Behind LOS',
      pass_length < 15 ~ '0-14',
      pass_length < 25 ~ '15-24',
      pass_length <= 39 ~ '25-39',
      TRUE ~ '40+'
    )
  ) |> 
  drop_na() |> 
  mutate(pass_bin = factor(pass_bin, levels = c("40+", "25-39", "15-24", '0-14', 'Behind LOS'))) |> 
  ggplot(aes(x = placement_score, y = pass_bin, fill = pass_bin)) +
  geom_density_ridges(alpha = 0.6, scale = 1.5, quantile_lines = TRUE, quantiles = 2) + # quantile_fun = mean
  scale_y_discrete(expand = expansion(mult = c(0.1, 0))) +
  labs(
    #title = "Distribution of Placement Score by Pass Length",
    x = "Placement Score", 
    y = "Pass Length (yards)",
    caption = "Quantile line = median"
  ) +
  theme_bw(base_size = 17) +
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
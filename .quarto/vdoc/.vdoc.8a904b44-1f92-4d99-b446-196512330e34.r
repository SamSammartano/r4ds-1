#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
billboard <- read_csv("data/billboard.csv")
billboard |>
  select(artist, track, date.entered, wk1:wk4)

summary(billboard$date.entered)
#
#
#
wk1_data <- billboard |>
  drop_na(wk1)

n_wk1 <- nrow(wk1_data)
n_wk1

ggplot(wk1_data, aes(x = wk1)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  labs(
    title = "Distribution of Week 1 Rankings",
    subtitle = paste("Number of values used:", n_wk1),
    x = "Week 1 ranking",
    y = "Count"
  )
#
#
#
wk6_data <- billboard |>
  drop_na(wk6)

n_wk6 <- nrow(wk6_data)
n_wk6

ggplot(wk6_data, aes(x = wk6)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  labs(
    title = "Distribution of Week 6 Rankings",
    subtitle = paste("Number of values used:", n_wk6),
    x = "Week 6 ranking",
    y = "Count"
  )
#
#
#
billboard |>
  mutate(song = str_c(artist, " - ", track)) |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  ) |>
  mutate(week = parse_number(week)) |>
  ggplot(aes(x = week, y = rank, group = song)) +
  geom_line(alpha = 0.25, na.rm = TRUE) +
  scale_y_reverse() +
  labs(
    title = "Billboard Rankings Over Time",
    x = "Week",
    y = "Rank"
  )
#
#
#
billboard |>
  summarise(
    across(
      c(wk1, wk4, wk10, wk20, wk40, wk76),
      list(
        missing = ~ sum(is.na(.x)),
        present = ~ sum(!is.na(.x))
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("week", "status"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = status, values_from = value)
#
#
#
billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  ) |>
  arrange(artist, track, week) |>
  group_by(artist, track) |>
  summarise(
    reentered = any(
      !is.na(rank) & lag(cumany(is.na(rank)), default = FALSE)
    ),
    .groups = "drop"
  ) |>
  count(reentered, name = "songs")
#
#
#
billboard |>
  mutate(
    wk6_status = case_when(
      is.na(wk6) ~ "Missing by week 6",
      wk6 < wk1 ~ "Improved",
      wk6 == wk1 ~ "Same",
      wk6 > wk1 ~ "Worse"
    ),
    wk6_status = factor(
      wk6_status,
      levels = c("Improved", "Same", "Worse", "Missing by week 6")
    )
  ) |>
  count(wk6_status, name = "songs")
#
#
#
#

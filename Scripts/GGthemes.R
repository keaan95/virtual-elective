install.packages("tidyverse")

install.packages("espnscrapeR")

install.packages("ggthemes")

install.packages("remotes")
remotes::install_github("jthomasmock/espnscrapeR")

library(espnscrapeR)


library(ggthemes)

require(tidyverse)


nfl_stand <- 2014:2020 %>% 
  map_dfr(get_nfl_standings)

nfl_stand %>% 
  arrange(year, conf, desc(win_percent)) %>% 
  select(year, conf, team_name, abb_name, logos, win_percent, differential)

nfl_stand$div_losses

diff_df <- nfl_stand %>% arrange(season, conf, desc(win_pct)) %>% 
  select(season, conf, team_name, team_abb, team_logo, win_pct, pts_diff)


playoff_label_scatter <- tibble(
  differential = c(25,-125), y = c(0.3, 0.8), 
  label = c("Missed<br>Playoffs", "Made<br>Playoffs"),
  color = c("#D50A0A", "#013369")
)


nfl_stand %>%
  mutate(
    color = case_when(
      season < 2020 & seed <= 6 ~ "blue",
      season == 2020 & seed <= 7 ~ "blue",
      TRUE ~ "red"
    )
  ) %>% ggplot(aes(x = pts_diff, y = win_pct)) +
  geom_vline(xintercept = 0, size = 0.75, color = "#737373") +
  geom_point(aes(color = color, size =2))+
  scale_color_identity() +
  ggtext::geom_richtext(
    data = playoff_label_scatter,
    aes(x = differential, y = y, label = label, color = color),
    fill = "#f0f0f0", label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt"), # remove padding
    family = "Chivo", hjust = 0.1, fontface = "bold",
    size = 8
  ) +
  scale_color_identity() +
  labs(x = "Points Differential", y = "Win Percent",
       title = "Playoff teams typically have a positive point differential",
       subtitle = "Data through week 15 of the 2020 NFL Season",
       caption = str_to_upper("Plot: @thomas_mock | Data: ESPN")) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(.0, 1, by = .10)
  ) +
  scale_x_continuous(
    breaks = seq(-200, 250, by = 50)
  )+
  ggthemes::theme_fivethirtyeight() 


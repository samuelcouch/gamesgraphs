library(dplyr)
library(tidyverse)
library(openxlsx)

source("R/main.R")

match_data <- read_oracleselixir_match_data("data/match_data/2019-spring-match-data-OraclesElixir-2019-03-16.xlsx")
match_data$date <- convertToDateTime(match_data$date)
match_data <- match_data %>% 
  mutate(fpts = if_else(position == 'TEAM',
                        calc_dk_team_points(teamtowerkills, teamdragkills, teambaronkills, fb, result, gamelength),
                        calc_dk_player_points(k, d, a, cspm*gamelength)))

full_schedule <- match_data %>% 
  filter(position == "TEAM") %>% 
  select(gameid, week, game, league, team) %>% 
  group_by(gameid, week, game, league) %>% 
  summarise(opponent = paste(team, collapse = ""))

match_data <- match_data %>% 
  left_join(full_schedule) %>% 
  mutate(opponent = str_remove(opponent, as.character(team)))

save(match_data, file = "lol-oyster/data/match_data.RData")

position_avg <- match_data %>%
  filter(position != "TEAM") %>%
  group_by(position, league) %>% 
  summarise(position_avg_fpts = mean(fpts))

player_data <- match_data %>% 
  filter(position != "TEAM") %>% 
  group_by(player, position, team, league) %>% 
  summarise(gp = n(), 
            k = sum(k), d = sum(d), a = sum(a), 
            kda = round((k+a)/d, 1), 
            ka = round((k/(k+a)), 3)*100, 
            kp = round((k+a)/sum(teamkills), 3)*100,
            avg_fpts = mean(fpts),
            avg_l_fpts = mean(fpts[result == 0]))

player_data <- player_data %>% 
  left_join(position_avg) %>% 
  mutate(diff = round(avg_fpts - position_avg_fpts, 2))

save(player_data, file = "lol-oyster/data/player_data.RData")

team_data <- match_data %>%
  filter(position == "TEAM") %>% 
  group_by(team, league) %>%
  summarise(gp = n(),
            w = sum(result == 1),
            l = sum(result == 0),
            avg_fpts = mean(fpts, na.rm = TRUE),
            avg_w_fpts = mean(fpts[result == 1], na.rm = TRUE),
            avg_l_fpts = mean(fpts[result == 0], na.rm = TRUE),
            agt = mean(gamelength, na.rm = TRUE),
            kills = sum(k, na.rm = TRUE), deaths = sum(d, na.rm = TRUE), asst = sum(a, na.rm = TRUE),
            towers = sum(teamtowerkills, na.rm = TRUE),
            dragons = sum(teamdragkills, na.rm = TRUE),
            barons = sum(teambaronkills, na.rm = TRUE),
            fbp = round(sum(fb == 1, na.rm = TRUE)/gp, 3)*100,
            kpw = round(sum(k[result == 1], na.rm = TRUE)/w, 2),
            dpw = round(sum(d[result == 1], na.rm = TRUE)/w, 2),
            kpl = round(sum(k[result == 0], na.rm = TRUE)/l, 2),
            dpl = round(sum(d[result == 0], na.rm = TRUE)/l, 2))

save(team_data, file = "lol-oyster/data/team_data.RData")
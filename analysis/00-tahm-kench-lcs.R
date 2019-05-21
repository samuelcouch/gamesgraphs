cat("\014") # Clear your console
rm(list = ls()) #clear your environment

setwd("~/projects/games-graphs")
source(file.path(paste0(getwd(),"/header.R")))

library(tidyverse)
library(openxlsx)
library(extrafont)
library(ggthemr)
ggthemr("fresh")

match_data <- read_oracleselixir_match_data(paste0(DATA_ROOT, "match_data/2019-spring-match-data-OraclesElixir-2019-05-12.xlsx"))

# Allows us to creat an opponent collumn for each data point
full_schedule <- match_data %>% 
  filter(position == "TEAM") %>% 
  select(gameid, week, game, league, team) %>% 
  group_by(gameid, week, game, league) %>% 
  summarise(opponent = paste(team, collapse = ""))

match_data <- match_data %>% 
  left_join(full_schedule) %>% 
  mutate(opponent = str_remove(opponent, as.character(team)))

match_data <- match_data %>% 
  mutate(match_code = paste0(gameid, team))

lcs_matches <- match_data %>% 
  filter(league == "LCS")

team_results <- lcs_matches %>% 
  filter(position == "TEAM")

player_results <- lcs_matches %>% 
  filter(position != "TEAM")

total_matches <- length(unique(team_results$gameid))
subtitle_string <- paste0("2019 Spring Split LCS (", total_matches, " total matches)")

all_champ_bans <- team_results %>% 
  select(ban1:ban5) %>% 
  gather(ban_order, champion, ban1:ban5) %>%
  filter(!is.na(champion)) %>% 
  group_by(champion) %>% 
  summarise(total_bans = n())

top_champ_bans <- all_champ_bans %>% 
  arrange(desc(total_bans)) %>% 
  ungroup() %>% 
  mutate(champion = reorder(champion, total_bans)) %>% 
  top_n(15)

top_champ_bans_plot <- top_champ_bans %>% 
  ggplot(aes(x = champion, y = total_bans)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = champion, y = .1, label = paste0(round(total_bans,2),sep="")),
            hjust=-0.25, vjust=.5, size = 3.5, color = 'white') +
  labs(x = "Champion", y = "Total Bans", title="Top banned champions", subtitle = subtitle_string) +
  coord_flip()

ggsave("plots/00_tahm_kench/top_champ_bans.png", plot = top_champ_bans_plot)

all_used_champions <- player_results %>% 
  select(gameid, team, player, position, champion, result)

champ_winrates <- all_used_champions %>% 
  group_by(champion) %>% 
  summarise(total_uses = n(),
            wins = sum(result == 1),
            losses = sum(result == 0),
            winrate = round((as.double(wins)/as.double(total_uses))*100, 2))

top_champs <- champ_winrates %>% 
  filter(total_uses >= 15) %>% 
  arrange(desc(winrate)) %>% 
  mutate(champion = reorder(champion, winrate)) %>% 
  top_n(15)

top_champs_plot <- top_champs %>% 
  ggplot(aes(x = champion, y = winrate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = champion, y = .1, label = paste0(round(winrate, 1), "%", sep="")),
            hjust=-0.25, vjust=.5, size = 3.5, color = 'white') +
  labs(x = "Champion", y = "Win Rate", title="Top champions by win rate", subtitle = subtitle_string) +
  coord_flip()

ggsave("plots/00_tahm_kench/top_champs.png", plot = top_champs_plot)

top_used_champions <- all_used_champions %>% 
  group_by(champion) %>% 
  summarise(total_use = n()) %>% 
  arrange(desc(total_use)) %>% 
  ungroup() %>% 
  mutate(champion = reorder(champion, total_use)) %>% 
  top_n(15)

top_used_champions_plot <- top_used_champions %>% 
  ggplot(aes(x = champion, y = total_use)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = champion, y = .1, label = paste0(round(total_use,2),sep="")),
            hjust=-0.25, vjust=.5, size = 3.5, color = 'white') +
  labs(x = "Champion", y = "Total Uses", title="Top Used champions", subtitle = subtitle_string) +
  coord_flip()

ggsave("plots/00_tahm_kench/top_used_champions.png", plot = top_used_champions_plot)

tk_positions <- player_results %>% 
  filter(champion == "Tahm Kench") %>% 
  group_by(position) %>% 
  summarise(total_uses = n())

tk_games_used <- player_results %>% 
  filter(champion == "Tahm Kench") %>% 
  select(gameid, team, match_code)

tk_team_data <- team_results %>% 
  filter(match_code %in% unique(tk_games_used$match_code)) %>% 
  summarise(all_d = mean(d),
            win_d = mean(d[result == 1]),
            loss_d = mean(d[result == 0]))

else_team_data<- team_results %>% 
  filter(!(match_code %in% unique(tk_games_used$match_code))) %>% 
  summarise(all_d = mean(d),
            win_d = mean(d[result == 1]),
            loss_d = mean(d[result == 0]))

tk_adc <- player_results %>% 
  filter(position == "ADC", match_code %in% unique(tk_games_used$match_code)) %>% 
  summarise(all_d = mean(d),
            win_d = mean(d[result == 1]),
            loss_d = mean(d[result == 0]),
            all_k = mean(k),
            win_k = mean(k[result == 1]),
            loss_k = mean(k[result == 0]),
            all_a = mean(a),
            win_a = mean(a[result == 1]),
            loss_a = mean(a[result == 0]))

no_tk_adc <- player_results %>% 
  filter(position == "ADC") %>% 
  summarise(all_d = mean(d),
            win_d = mean(d[result == 1]),
            loss_d = mean(d[result == 0]),
            all_k = mean(k),
            win_k = mean(k[result == 1]),
            loss_k = mean(k[result == 0]),
            all_a = mean(a),
            win_a = mean(a[result == 1]),
            loss_a = mean(a[result == 0]))

# Who played TK and with how much success
tk_players <- player_results %>% 
  filter(champion == "Tahm Kench") %>% 
  group_by(team, player) %>% 
  summarise(total_uses = n(),
            wins = sum(result == 1),
            losses = sum(result == 0),
            winrate = round((as.double(wins)/as.double(total_uses))*100, 2))

# How many times a team was banned from using TK
team_tk_bans <- team_results %>%
  select(ban1:ban5, opponent) %>% 
  gather(ban_order, champion, ban1:ban5) %>%
  filter(!is.na(champion), champion == "Tahm Kench") %>% 
  group_by(opponent) %>% 
  summarise(total_bans = n())

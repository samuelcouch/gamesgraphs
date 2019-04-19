library(tidyverse)
library(openxlsx)
library(extrafont)
library(ggthemr)
ggthemr("fresh")

source("R/main.R")

match_data <- read_oracleselixir_match_data("data/match_data/2019-spring-match-data-OraclesElixir-2019-04-17.xlsx")

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

top_champ_bans %>% 
  ggplot(aes(x = champion, y = total_bans)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = champion, y = .1, label = paste0(round(total_bans,2),sep="")),
            hjust=-0.25, vjust=.5, size = 3.5, color = 'white') +
  labs(x = "Champion", y = "Total Bans", title="Top banned champions", subtitle = subtitle_string) +
  coord_flip()

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
  arrange(desc(winrate))

top_used_champions <- all_used_champions %>% 
  group_by(champion) %>% 
  summarise(total_use = n()) %>% 
  arrange(desc(total_use)) %>% 
  ungroup() %>% 
  mutate(champion = reorder(champion, total_use)) %>% 
  top_n(15)

top_used_champions %>% 
  ggplot(aes(x = champion, y = total_use)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = champion, y = .1, label = paste0(round(total_use,2),sep="")),
            hjust=-0.25, vjust=.5, size = 3.5, color = 'white') +
  labs(x = "Champion", y = "Total Uses", title="Top Used champions", subtitle = subtitle_string) +
  coord_flip()




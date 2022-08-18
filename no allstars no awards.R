library(tidyverse)
library(rvest)
library(polite)
library(janitor)


advanced=read_csv("Data/Advanced.csv") %>% filter(lg !="ABA") %>% 
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% arrange(desc(season),player)

distinct_allstars=read_csv("Data/All-Star Selections.csv") %>% filter(lg !="ABA") %>% distinct(player)

distinct_awards=read_csv("Data/Player Award Shares.csv") %>% filter(str_detect(award,"aba",negate=TRUE)) %>% distinct(player,player_id)

distinct_end_of_season_teams=read_csv("Data/End of Season Teams.csv") %>% filter(lg !="ABA") %>% distinct(player,player_id)

distinct_end_of_season_teams_voting=read_csv("Data/End of Season Teams (Voting).csv") %>% filter(lg !="ABA") %>% 
  distinct(player,player_id)

no_allstars_no_awards=anti_join(advanced,distinct_awards) %>% 
  anti_join(.,distinct_end_of_season_teams) %>%
  anti_join(.,distinct_allstars) %>% 
  anti_join(.,distinct_end_of_season_teams_voting) %>% 
  group_by(player_id,player) %>% 
  summarize(first_seas=min(season),last_seas=max(season),career_ws=sum(ws,na.rm=TRUE),career_vorp=sum(vorp,na.rm=TRUE),
            ws_per_seas=career_ws/max(experience),vorp_per_seas=career_vorp/max(experience))

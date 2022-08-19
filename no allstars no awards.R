library(tidyverse)
library(rvest)
library(polite)
library(janitor)

realgm_bow=bow('https://basketball.realgm.com/',user_agent="Sumitro Datta",force=TRUE)

realgm_award_scrape<-function(path_to_scrape){
  session = nod(realgm_bow,path=path_to_scrape)
  df=scrape(session) %>%
    html_elements(css="[class='tablesaw']") %>% 
    html_table() %>% .[[1]] %>% clean_names() %>% 
    select(season:team) %>% 
    mutate(season=as.numeric(str_sub(season,start=-4))) %>%
    mutate(team=case_when(str_detect(team,"Baltimore")~"Baltimore Bullets",
                          str_detect(team,"Sixers")~"Philadelphia 76ers",
                          str_detect(team,"San Fran")~"Philadelphia Warriors",
                          (str_detect(team,"Hornets") & season==2006)~"New Orleans/Oklahoma City Hornets",
                          (str_detect(player,"Penny") & season==1994)~"Orlando Magic",
                          TRUE~team)) %>%
    mutate(player=case_when(str_detect(player,"Heinsohn")~"Tom Heinsohn",
                            str_detect(player,"Satch Sanders")~"Tom Sanders",
                            str_detect(player,"Otto Porter")~"Otto Porter Jr.",
                            str_detect(player,"James McAdoo")~"James Michael McAdoo",
                            str_detect(player,"Ginobili")~"Manu Ginóbili",
                            str_detect(player,"Guokas")~"Matt Guokas",
                            str_detect(player,"Charlie Share")~"Chuck Share",
                            str_detect(player,"Ben Swain")~"Bennie Swain",
                            str_detect(player,"Maury King")~"Maurice King",
                            str_detect(player,"Ronnie Watts")~"Ron Watts",
                            str_detect(player,"Emmette Bryant")~"Em Bryant",
                            str_detect(player,"John Q. Trapp")~"John Trapp",
                            str_detect(player,"Hawthorne Wingo")~"Harthorne Wingo",
                            str_detect(player,"Mo Cheeks")~"Maurice Cheeks",
                            str_detect(player,"David Greenwood")~"Dave Greenwood",
                            str_detect(player,"Bobby Hansen")~"Bob Hansen",
                            str_detect(player,"Petruska")~"Richard Petruška",
                            str_detect(player,"Charles A. Jones")~"Charles Jones",
                            str_detect(player,"Tabak")~"Žan Tabak",
                            str_detect(player,"Kukoc")~"Toni Kukoč",
                            str_detect(player,"Medvedenko")~"Stanislav Medvedenko",
                            str_detect(player,"J.R. Rider")~"Isaiah Rider",
                            str_detect(player,"Bateer")~"Mengke Bateer",
                            str_detect(player,"Milicic")~"Darko Miličić",
                            str_detect(player,"Nesterovic")~"Rasho Nesterović",
                            str_detect(player,"Vujacic")~"Sasha Vujačić",
                            str_detect(player,"Yue Sun")~"Sun Yue",
                            str_detect(player,"Stojakovic")~"Peja Stojaković",
                            str_detect(player,"Kuzmic")~"Ognjen Kuzmić",
                            str_detect(player,"Dellavedova")~"Matthew Dellavedova",
                            str_detect(player,"Jokic")~"Nikola Jokić",
                            str_detect(player,"Doncic")~"Luka Dončić",
                            str_detect(player,"Turkoglu")~"Hedo Türkoğlu",
                            str_detect(player,"Penny Hardaway")~"Anfernee Hardaway",
                            str_detect(player,"Bojan Bogdan")~"Bojan Bogdanović",
                            str_detect(player,"McCollum")~"CJ McCollum",
                            (str_detect(player,"Cliff Robinson") & season==1996)~"Clifford Robinson",
                            str_detect(player,"Schroder")~"Dennis Schröder",
                            str_detect(player,"Drazen")~"Dražen Petrović",
                            str_detect(player,"Eddie Johnson, Jr.")~"Eddie Johnson",
                            str_detect(player,"Ilyasova")~"Ersan İlyasova",
                            str_detect(player,"Dragic")~"Goran Dragić",
                            str_detect(player,"Greivis")~"Greivis Vásquez",
                            str_detect(player,"Porzingis")~"Kristaps Porziņģis",
                            str_detect(player,"Mike Dunleavy, Sr.")~"Mike Dunleavy",
                            str_detect(player,"Pekovic")~"Nikola Peković",
                            str_detect(player,"Vucevic")~"Nikola Vučević",
                            str_detect(player,"Willy Hernangomez")~"Willy Hernangómez",
                            str_detect(player,"Dario Saric")~"Dario Šarić",
                            str_detect(player,"Mirotic")~"Nikola Mirotić",
                            str_detect(player,"Valanciunas")~"Jonas Valančiūnas",
                            str_detect(player,"Jianlian")~"Yi Jianlian",
                            str_detect(player,"Rebraca")~"Željko Rebrača",
                            str_detect(player,"Charles Daniel")~"Charles Smith",
                            str_detect(player,"Cadillac")~"Greg Anderson",
                            TRUE~player))
  return(df)
}


team_summaries=read_csv("Data/Team Summaries.csv") %>% select(season:team,tm=abbreviation)

champions=realgm_award_scrape(path_to_scrape="nba/awards/by-type/NBA-Champion/47") %>%
  left_join(.,team_summaries)

players_of_month=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Player-Of-The-Month/29") %>%
  left_join(.,team_summaries)

players_of_week=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Player-Of-The-Week/30") %>%
  left_join(.,team_summaries)

rookies_of_month=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Rookie-Of-The-Month/31") %>%
  left_join(.,team_summaries)

realgm_dfs_bbref_leaders=bind_rows(champions,players_of_month,players_of_week,rookies_of_month)
advanced=read_csv("Data/Advanced.csv") %>% filter(lg !="ABA")

players_in_realgm_dfs=semi_join(advanced,realgm_dfs) %>%
  distinct(player,player_id)

players_not_in_realgm_dfs=anti_join(advanced,players_in_realgm_dfs) %>% 
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% arrange(desc(season),player)

distinct_allstars=read_csv("Data/All-Star Selections.csv") %>% filter(lg !="ABA") %>% distinct(player)

distinct_awards=read_csv("Data/Player Award Shares.csv") %>% filter(str_detect(award,"aba",negate=TRUE)) %>% distinct(player,player_id)

distinct_end_of_season_teams=read_csv("Data/End of Season Teams.csv") %>% filter(lg !="ABA") %>% distinct(player,player_id)

distinct_end_of_season_teams_voting=read_csv("Data/End of Season Teams (Voting).csv") %>% filter(lg !="ABA") %>% 
  distinct(player,player_id)

no_allstars_no_awards=anti_join(players_not_in_realgm_dfs,distinct_awards) %>% 
  anti_join(.,distinct_end_of_season_teams) %>%
  anti_join(.,distinct_allstars) %>% 
  anti_join(.,distinct_end_of_season_teams_voting) %>% 
  group_by(player_id,player) %>% 
  summarize(first_seas=min(season),last_seas=max(season),career_ws=sum(ws,na.rm=TRUE),career_vorp=sum(vorp,na.rm=TRUE),
            ws_per_seas=career_ws/max(experience),vorp_per_seas=career_vorp/max(experience))

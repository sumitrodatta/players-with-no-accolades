library(tidyverse)
library(rvest)
library(polite)
library(janitor)

realgm_bow=bow('https://basketball.realgm.com/',user_agent="Sumitro Datta",force=TRUE)
bbref_bow=bow("https://www.basketball-reference.com/",user_agent="Sumitro Datta",force=TRUE)

bbref_leaders_scrape<-function(path_to_scrape){
  session = nod(bbref_bow,path=path_to_scrape)
  df=scrape(session) %>% html_elements(css="#leaders") %>% html_table() %>% .[[1]] %>% clean_names() %>% 
    filter(lg !="ABA") %>% mutate(season=as.numeric(str_sub(season,end=4))+1) %>% 
    mutate(player=ifelse(str_detect(player,"\\*"),str_sub(player,end=-2),player))
  return(df)
}

bbref_awards_scrape<-function(path_to_scrape){
  session = nod(bbref_bow,path=paste0("awards/",path_to_scrape,".html"))
  df=scrape(session) %>% html_elements(css=paste0("#",path_to_scrape,"_NBA")) %>% 
    html_table() %>% .[[1]] %>% row_to_names(1) %>% clean_names() %>% 
    filter(lg !="ABA") %>% mutate(season=as.numeric(str_sub(season,end=4))+1) %>%
    mutate(player=case_when(
      str_detect(player,"Tie")~str_sub(player,end=-7),
      TRUE~player)) %>%
    select(season:tm)
  return(df)
}

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

scoring_leaders=bbref_leaders_scrape("leaders/pts_per_g_yearly.html")
reb_leaders=bbref_leaders_scrape("leaders/trb_per_g_yearly.html")
ast_leaders=bbref_leaders_scrape("leaders/ast_per_g_yearly.html")
stl_leaders=bbref_leaders_scrape("leaders/stl_per_g_yearly.html")
blk_leaders=bbref_leaders_scrape("leaders/blk_per_g_yearly.html")

finals_mvps=bbref_awards_scrape("finals_mvp")
ecf_mvps=bbref_awards_scrape("ecf_mvp")
wcf_mvps=bbref_awards_scrape("ecf_mvp")
all_star_mvps=bbref_awards_scrape("all_star_mvp")
comeback_player=bbref_awards_scrape("cpoy")
seeding_games_player=bbref_awards_scrape("player_of_the_seeding_games")
hustle=bbref_awards_scrape("hustle")

all_seeding_games_teams=nod(bbref_bow,path=paste0("awards/all_seeding_games.html")) %>% scrape(.) %>% 
  html_elements(css="#awards_all_seeding_games") %>% 
  html_table() %>% .[[1]] %>% clean_names() %>% mutate(season=as.numeric(str_sub(season,end=4))+1) %>% 
  pivot_longer(cols=x:x_5,values_to = "player") %>% select(-name) %>%
  mutate(player=if_else(player=="Michael Porter","Michael Porter Jr.",player)) %>% rename(number_tm=tm)

bbref_scraped=bind_rows(scoring_leaders,reb_leaders,ast_leaders,stl_leaders,blk_leaders,
                        finals_mvps,ecf_mvps,wcf_mvps,all_star_mvps,comeback_player,seeding_games_player,
                        hustle)

team_summaries=read_csv("Data/Team Summaries.csv") %>% select(season:team,tm=abbreviation)

champions=realgm_award_scrape(path_to_scrape="nba/awards/by-type/NBA-Champion/47") %>%
  left_join(.,team_summaries)

players_of_month=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Player-Of-The-Month/29") %>%
  left_join(.,team_summaries)

players_of_week=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Player-Of-The-Week/30") %>%
  left_join(.,team_summaries)

rookies_of_month=realgm_award_scrape(path_to_scrape="nba/awards/by-type/Rookie-Of-The-Month/31") %>%
  left_join(.,team_summaries)

realgm_bbref=bind_rows(champions,players_of_month,players_of_week,rookies_of_month,
                                   bbref_scraped) %>% select(-age)

advanced=read_csv("Data/Advanced.csv") %>% filter(lg !="ABA")

players_in_realgm_bbref=semi_join(advanced,realgm_bbref) %>%
  distinct(player,player_id)

players_not_in_realgm_bbref=anti_join(advanced,players_in_realgm_bbref) %>% 
  anti_join(.,all_seeding_games_teams %>% distinct(player)) %>%
  mutate(tm=ifelse(tm=="TOT","1TOT",tm)) %>% 
  group_by(player_id,season) %>% arrange(tm) %>% slice(1) %>% 
  mutate(tm=ifelse(tm=="1TOT","TOT",tm)) %>% arrange(desc(season),player)

# distinct_def_rookie_teams_voting=read_csv("Data/All NBA Voting Ballots.csv") %>% filter(award %in% c("All-Defense","All-Rook")) %>%
#   filter(player !="Abstained") %>%
#   group_by(year,award,player) %>% summarize(tot_pts=sum(points_given)) %>%
#   mutate(player = case_when(
#     str_detect(player,"Ginobili")~"Manu Ginóbili",
#     str_detect(player,"Bembry")~"DeAndre' Bembry",
#     str_detect(player,"Alex Abrines")~"Álex Abrines",
#     str_detect(player,"James Ennis|Frank Mason")~paste(player,"III"),
#     str_detect(player,"Juan Hernan")~"Juancho Hernangómez",
#     str_detect(player,"Xavier Tillman")~"Xavier Tillman Sr.",
#     str_detect(player,"Alperen")~"Alperen Şengün",
#     str_detect(player,"Marjanovic|Bojan|Bogdan Bogdanovic|Mirotic|Jokic|Antic|Nurkic")~
#       stringi::stri_replace_last_regex(player,pattern="c",replacement = "ć"),
#     str_detect(player,"Damjan")~"Damjan Rudež",
#     str_detect(player,"Vucevic")~"Nikola Vučević",
#     str_detect(player,"Willy Hernangomez")~"Willy Hernangómez",
#     str_detect(player,"Dario Saric")~"Dario Šarić",
#     str_detect(player,"Valanciunas")~"Jonas Valančiūnas",
#     str_detect(player,"Porzingis")~"Kristaps Porziņģis",
#     str_detect(player,"Davis Bertans")~"Dāvis Bertāns",
#     str_detect(player,'Schroder')~"Dennis Schröder",
#     str_detect(player,"Harry Giles|Robert Williams")~str_sub(player,end=-5),
#     str_detect(player,"Skal Lab")~"Skal Labissière",
#     str_detect(player,"Milos Teo")~"Miloš Teodosić",
#     str_detect(player,"Luka Donc")~"Luka Dončić",
#     str_detect(player,"Theo Male")~"Théo Maledon",
#     TRUE~player
#   )) %>% left_join(.,advanced) %>% ungroup() %>% distinct(player,player_id)

distinct_allstars=read_csv("Data/All-Star Selections.csv") %>% filter(lg !="ABA") %>% distinct(player)

distinct_awards=read_csv("Data/Player Award Shares.csv") %>% filter(str_detect(award,"aba",negate=TRUE),winner) %>% distinct(player,player_id)

distinct_end_of_season_teams=read_csv("Data/End of Season Teams.csv") %>% filter(lg !="ABA") %>% distinct(player,player_id)

# distinct_end_of_season_teams_voting=read_csv("Data/End of Season Teams (Voting).csv") %>% filter(lg !="ABA") %>% 
#   distinct(player,player_id)

no_allstars_no_awards=anti_join(players_not_in_realgm_bbref,distinct_awards) %>% 
  anti_join(.,distinct_end_of_season_teams) %>%
  anti_join(.,distinct_allstars) %>% 
#  anti_join(.,distinct_end_of_season_teams_voting) %>% 
#  anti_join(.,distinct_def_rookie_teams_voting) %>%
  group_by(player_id,player) %>% 
  summarize(first_seas=min(season),last_seas=max(season),career_ws=sum(ws,na.rm=TRUE),career_vorp=sum(vorp,na.rm=TRUE),
            peak_ws=max(ws,na.rm = TRUE),peak_vorp=max(vorp,na.rm=TRUE),
            ws_per_seas=career_ws/max(experience),vorp_per_seas=career_vorp/max(experience)) %>% arrange(desc(career_ws)) %>%
  mutate(active=last_seas %in% 2020:2022,.after="last_seas")

write_csv(no_allstars_no_awards,"No Allstars, No Awards.csv")

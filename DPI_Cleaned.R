library(tidyverse)
library(nflscrapR)
library(teamcolors)
library(ggrepel)


pbp=data.frame(matrix(ncol=0,nrow=0))



##########################################################################################2012-2018 Data
for (i in 10:18){
  
  
  pbpread <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_20",i,".csv")))
  
  pbp0 <- pbpread%>%
    select(game_id,home_team, away_team, posteam, defteam, yardline_100, half_seconds_remaining, game_half,down,
           goal_to_go, ydstogo, yards_gained,  desc, qb_dropback, play_type, shotgun, pass_length, pass_location, air_yards, yards_after_catch,
           ep, epa, air_epa, wp, wpa, third_down_converted, third_down_failed, pass_attempt, sack, pass_touchdown, complete_pass,
           passer_player_name, receiver_player_name, penalty, penalty_player_name, penalty_yards) %>%
    rename(home = home_team)%>%
    mutate(PA = if_else(str_detect(desc, "play action"), 1, 0))%>%
    mutate(DPI = if_else(str_detect(desc, "Defensive Pass Interference"), 1, 0))%>%
    mutate(OPI = if_else(str_detect(desc, "Offensive Pass Interference"), 1, 0))
  
  pbp <- rbind(pbp0,pbp)
  
}


PI0 <- pbp%>%
  filter(OPI == 1| DPI == 1)%>%
  mutate(description = desc)%>%
  separate(
    desc,
    into = c("P1", "P2", "P3"),
    sep = "pass",
    convert = TRUE
  )%>%
  separate(
    P1,
    into = c("P1a", "P1b", "P1c"),
    sep = "[)]",
    convert = TRUE)%>%
  select(1:14,16,15,17:44)%>%
  mutate(QB = coalesce(P1c, P1b))%>%
  mutate(QB = str_replace(QB,"TWO-POINT CONVERSION ATTEMPT.", ""))%>%
  mutate(QB = str_replace(QB,"FUMBLES \\(Aborted", ""))%>%
  mutate(QB = str_replace(QB,"flea flicker", ""))%>%
  mutate(QB = str_replace(QB,"C.Robinson reported in as eligible.  M.Stafford", "M.Stafford"))%>%
  mutate(QB = str_replace(QB,"C.Fleming reported in as eligible.  J.Brissett", "J.Brissett"))%>%
  mutate(QB = str_replace(QB,"L.Thomas and B.Qvale reported in as eligible.  J.McCown", "J.McCown"))%>%
  mutate(QB = str_replace(QB,"K.Pamphile reported in as eligible.  J.Winston", "J.Winston"))%>%
  mutate(QB = str_replace(QB,"B.Sowell reported in as eligible.  M.Trubisky", "M.Trubisky"))%>%
  mutate(QB = str_replace(QB,"L.Wester reported in as eligible.  J.Winston", "J.Winston"))%>%
  mutate(QB = str_replace(QB,"J.Walker reported in as eligible.  A.Rodgers", "A.Rodgers"))%>%
  mutate(QB = str_replace(QB,"#72 Linkenbach reports eligible. R.Tannehill", "R.Tannehill"))%>%
  mutate(QB = str_replace(QB,"C.Lucas reported in as eligible.  M.Stafford", "M.Stafford"))%>%
  mutate(QB = str_replace(QB,"A.Shipley reported in as eligible.  C.Palmer", "C.Palmer"))%>%
  mutate(QB = str_replace(QB,"O.Cousins reported in as eligible.  J.McCown", "J.McCown"))%>%
  mutate(QB = str_replace(QB,"R.Reiff reported in as eligible.  M.Stafford", "M.Stafford"))%>%
  mutate(QB = str_replace(QB,"D.Weems reported in as eligible.  T.Siemian", "T.Siemian"))%>%
  mutate(QB = str_replace(QB,"I.Seumalo reported in as eligible.  C.Wentz", "C.Wentz"))%>%
  mutate(QB = str_replace(QB,"C.Fleming reported in as eligible.  T.Brady", "T.Brady"))%>%
  mutate(QB = str_replace(QB,"Jason Campbell in at QB for CLE. J.Campbell", "J.Campbell"))%>%
  mutate(QB = str_replace(QB,"G.Fant reported in as eligible.  R.Wilson", "R.Wilson"))%>%
  mutate(QB = str_replace(QB,"S.Young reported in as eligible.  M.Moore", "M.Moore"))%>%
  mutate(QB = str_replace(QB,"A.Blythe reported in as eligible.  A.Luck", "A.Luck"))%>%
  mutate(QB = str_replace(QB,"K.Lamm reported in as eligible.  B.Weeden", "B.Weeden"))%>%
  mutate(QB = str_replace(QB,"B.Sowell reported in as eligible.  A.Luck", "A.Luck"))%>%
  mutate(QB = str_replace(QB,"at TB 39, and recovers at TB 40. D.Brees", "D.Brees"))%>%
  mutate(QB = str_replace(QB,"#72 Eligible M.Glennon", "M.Glennon"))%>%
  mutate(QB = str_replace(QB,"PENALTY on DEN-D.Ware, Defensive Pass Interference, 2 yards, enforced at DEN 4 - No Play.", "A. Smith"))%>%
  mutate(QB = str_replace(QB,"J.Britt to SEA 35 for -6 yards. FUMBLES, recovered by SEA-R.Wilson at SEA 35. R.Wilson", "R.Wilson"))%>%
  mutate(QB = str_replace(QB,"E.Britton reported in as eligible.  J.Clausen", "J.Clausen"))%>%
  mutate(QB = str_replace(QB,"S.Kelemete reported in as eligible.  D.Brees", "D.Brees"))%>%
  mutate(QB = str_replace(QB,"D.Kirkland reported in as eligible.  D.Carr", "D.Carr"))%>%
  mutate(QB = str_trim(QB))%>%
  mutate(lengthQB = str_length(QB))%>%
  select(1:12,18:45)%>%
  filter(play_type=="no_play" | play_type == "pass")


######################Text Fixer#################

###WR Data#########
descriptiondf <- PI0%>%
  select(description)%>%
  mutate(desc = description)%>%
  separate(
    description,
    into = c("foo", "Play", "bar"),
    sep = "left to",
    convert = TRUE)%>%
  separate(
    Play,
    into = c("WRLeft","Stuff"),
    sep = "\\. ",
    convert = TRUE)%>%
  select(5,2)%>%
  mutate(description = desc)%>%
  separate(
    description,
    into = c("foo", "Play", "bar"),
    sep = "middle to",
    convert = TRUE)%>%
  separate(
    Play,
    into = c("WRMiddle","Stuff"),
    sep = "\\. ",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(description = desc)%>%
  separate(
    description,
    into = c("foo", "Play", "bar"),
    sep = "right to",
    convert = TRUE)%>%
  separate(
    Play,
    into = c("WRRight","Stuff"),
    sep = "\\. ",
    convert = TRUE)%>%
  select(1,2,3,5)%>%
  mutate(description = desc)%>%
  separate(
    description,
    into = c("foo", "Play", "bar"),
    sep = "intended for",
    convert = TRUE)%>%
  separate(
    Play,
    into = c("Intercepted","Stuff"),
    sep = "Intercepted by",
    convert = TRUE)%>%
  separate(
    Intercepted,
    into = c("Intercepted","Stuff"),
    sep = "INTERCEPTED",
    convert = TRUE)%>%
  select(1,2,3,4,6)%>%
  mutate(WR = coalesce(WRLeft, WRRight, WRMiddle, Intercepted))%>%
  select(1,6)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "for",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "\\(",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "pushed",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "\\[",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "ran",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WRlength = str_length(WR))%>%
  separate(
    WR,
    into = c("WR","Stuff"),
    sep = "to",
    convert = TRUE)%>%
  select(1,2,4)%>%
  mutate(WR = str_trim(WR))%>%
  mutate(WRlength = str_length(WR))

###############DPI Only (To Peep QBs)
DPIdf <- PI0%>%
  inner_join(descriptiondf,by = c("description" = "desc"))%>%
  mutate(WR = if_else(WR=="D", "De. Thomas", WR))%>%
  mutate(WR = if_else(WR=="Jo", "JO. Brown", WR))%>%
  mutate(WR = if_else(WR=="Ja", "Ja. Brown", WR))%>%
  mutate(WR = if_else(WR=="J.G", "J.Grant", WR))%>%
  mutate(WR = if_else(WR=="R.G", "R.Grant", WR))%>%
  mutate(WR = if_else(WR=="B.S", "B.Stokley", WR))%>%
  mutate(WR = if_else(WR=="D.B", "D.Branch", WR))%>%
  mutate(WR = if_else(WR=="K.W", "K.Wright", WR))%>%
  mutate(WR = if_else(WR=="L.S", "L.Stocker", WR))%>%
  mutate(WR = if_else(WR=="C.Sut", "C.Sutton", WR))%>%
  mutate(WR = if_else(WR=="T.Bur", "T.Burton", WR))%>%
  mutate(WR = if_else(WR=="Q.Pat", "Q.Patton", WR))%>%
  mutate(WR = if_else(WR=="T.Hil", "T.Hilton", WR))%>%
  mutate(WR = if_else(WR=="N.Washing", "N.Washington", WR))%>%
  mutate(WR = if_else(WR=="C.Hamil", "C.Hamilton", WR))%>%
  mutate(WRlength = str_length(WR))%>%
  select(game_id,description,posteam,QB,WR,penalty_player_name,DPI, OPI,penalty_yards,down,ydstogo,yardline_100,ep,epa,wp,wpa)%>%
  filter(DPI==1)%>%
  mutate(season = substr(game_id,1,4))



QBepadf <- DPIdf%>%
  group_by(QB,posteam,season)%>%
  summarize(DPIepa = sum(epa,na.rm=TRUE),DPIcalls = n())%>%
  select(QB,season,posteam,DPIcalls,DPIepa)%>%
  arrange(desc(DPIepa))%>%
  filter(str_length(QB)<20)

Dropbacks <- pbp%>%
  mutate(season = substr(game_id,1,4))%>%
  group_by(passer_player_name,posteam,season)%>%
  summarize(dropbacks=sum(qb_dropback, na.rm = TRUE))%>%
  filter(!is.na(passer_player_name))

QBDPIdf <- QBepadf%>%
  inner_join(Dropbacks,by = c("QB" = "passer_player_name","season"="season","posteam"="posteam"))%>%
  mutate(DPIperATT = DPIepa/(DPIcalls+dropbacks))%>%
  arrange(desc(DPIperATT))%>%
  filter(dropbacks>=160)%>%
  left_join(nflteams, by = c("posteam" = "abbr"))%>%
  mutate(gglabel = case_when(
    QB == 'J.Flacco' ~ paste0(QB,"\'",substr(season,3,4)),
    DPIperATT > .05 ~ paste0(QB,"\'",substr(season,3,4)),
    posteam == 'DEN' ~ paste0(QB,"\'",substr(season,3,4))
  )
  )

ggplot(QBDPIdf, aes(dropbacks,DPIepa)) + 
  geom_point(shape = 21, size = 3,aes(color = team,fill=team))+
  geom_smooth(method = lm, aes(x = dropbacks,y = DPIepa))+
  scale_fill_teams(1,guide = FALSE) + 
  scale_color_teams(2, guide = FALSE) +
  geom_text_repel(aes(label=gglabel))+
  xlab("EPA from DPI")
  labs(title = "Points added via Defensive Pass Interference",
       subtitle = "Minimum dropbacks >=160 from 2010-2018 NFL Seasons",
       caption = "Data sourced from nflscrapR")





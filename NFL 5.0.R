library(tidyverse)
library(nflreadr)

weekly_boxscores <- load_player_stats(c(get_current_season()-13, get_current_season()-12, get_current_season()-11, get_current_season()-10, get_current_season()-9, get_current_season()-8, get_current_season()-7, get_current_season()-6, get_current_season()-5, get_current_season()-4, get_current_season()-3, get_current_season()-2, get_current_season()-1, get_current_season()), stat_type = "offense") %>% 
  arrange(-season, -week)
schedule <- load_schedules(c(year(today()-60)-14, year(today()-60)-13, year(today()-60)-12, year(today()-60)-11, year(today()-60)-10, year(today()-60)-9, year(today()-60)-8, year(today()-60)-7, year(today()-60)-6, year(today()-60)-5, year(today()-60)-4, year(today()-60)-3, year(today()-60)-2, year(today()-60)-1, year(today()-60)))
schedule$gametime <- gsub(":", "", schedule$gametime)
schedule <- schedule %>% 
  mutate(time_of_game=case_when(as.numeric(gametime)>=1800~"Night",
                                as.numeric(gametime)>=1400 & as.numeric(gametime)<1800~"Afternoon",
                                as.numeric(gametime)>=1200 & as.numeric(gametime)<1400~"Day",
                                TRUE~"Morning"),
         Home_Rest_Type=case_when(home_rest>=13~"Bye Week",
                                  home_rest<13 & home_rest>=10~"Mini Bye",
                                  home_rest<10 & home_rest>=7~"Normal",
                                  home_rest<7~"Short Week"),
         Away_Rest_Type=case_when(away_rest>=13~"Bye Week",
                                  away_rest<13 & away_rest>=10~"Mini Bye",
                                  away_rest<10 & away_rest>=7~"Normal",
                                  away_rest<7~"Short Week"),
         stadium_id=case_when(grepl("LON", stadium_id)==TRUE~"London",
                              grepl("GER", stadium_id)==TRUE | grepl("FRA", stadium_id)==TRUE~"Germany",
                              grepl("MEX", stadium_id)==TRUE~"Mexico",
                              grepl("SAO", stadium_id)==TRUE~"Brazil",
                              TRUE~stadium_id),
         home_div_game=case_when(home_team=="BUF" | home_team=="NYJ" | home_team=="MIA" | home_team=="NE"~"AFC East",
                                 home_team=="PIT" | home_team=="BAL" | home_team=="CLE" | home_team=="CIN"~"AFC North",
                                 home_team=="TEN" | home_team=="HOU" | home_team=="IND" | home_team=="JAX"~"AFC South",
                                 home_team=="KC" | home_team=="LAC" | home_team=="DEN" | home_team=="LV"~"AFC West",
                                 home_team=="WAS" | home_team=="DAL" | home_team=="NYG" | home_team=="PHI"~"NFC East",
                                 home_team=="GB" | home_team=="CHI" | home_team=="DET" | home_team=="MIN"~"NFC North",
                                 home_team=="TB" | home_team=="ATL" | home_team=="NO" | home_team=="CAR"~"NFC South",
                                 home_team=="SF" | home_team=="SEA" | home_team=="LA" | home_team=="ARI"~"NFC West"),
         away_div_game=case_when(away_team=="BUF" | away_team=="NYJ" | away_team=="MIA" | away_team=="NE"~"AFC East",
                                 away_team=="PIT" | away_team=="BAL" | away_team=="CLE" | away_team=="CIN"~"AFC North",
                                 away_team=="TEN" | away_team=="HOU" | away_team=="IND" | away_team=="JAX"~"AFC South",
                                 away_team=="KC" | away_team=="LAC" | away_team=="DEN" | away_team=="LV"~"AFC West",
                                 away_team=="WAS" | away_team=="DAL" | away_team=="NYG" | away_team=="PHI"~"NFC East",
                                 away_team=="GB" | away_team=="CHI" | away_team=="DET" | away_team=="MIN"~"NFC North",
                                 away_team=="TB" | away_team=="ATL" | away_team=="NO" | away_team=="CAR"~"NFC South",
                                 away_team=="SF" | away_team=="SEA" | away_team=="LA" | away_team=="ARI"~"NFC West"))
Todays_Games <- filter(schedule, gameday==today())
Current_Week_Games <- filter(schedule, season==get_current_season() & week==get_current_week())
oldteam <- c("OAK", "SD", "STL", "LAR")
newteam <- c("LV", "LAC", "LA", "LA")
schedule$home_team[schedule$home_team %in% oldteam] <- newteam[match(schedule$home_team, oldteam, nomatch = 0)]
schedule$away_team[schedule$away_team %in% oldteam] <- newteam[match(schedule$away_team, oldteam, nomatch = 0)]
weekly_boxscores$opponent_team[weekly_boxscores$opponent_team %in% oldteam] <- newteam[match(weekly_boxscores$opponent_team, oldteam, nomatch = 0)]
if (Todays_Games$game_type[1]=="REG") {
  weekly_boxscores <- filter(weekly_boxscores, season_type=="REG")
} else {
  weekly_boxscores <- weekly_boxscores
}
for (i in 1:nrow(weekly_boxscores)) {
  stats <- filter(schedule, season==weekly_boxscores$season[i] & week==weekly_boxscores$week[i] & (away_team==weekly_boxscores$recent_team[i] | home_team==weekly_boxscores$recent_team[i]))
  if (stats$location=="Neutral" & stats$home_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Neutral"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$away_div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
    weekly_boxscores$rest[i]=stats$Home_Rest_Type
    weekly_boxscores$stadium_id[i]=stats$stadium_id
  } else if (stats$location=="Neutral" & stats$away_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Neutral"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$home_div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
    weekly_boxscores$rest[i]=stats$Away_Rest_Type
    weekly_boxscores$stadium_id[i]=stats$stadium_id
  } else if (stats$location!="Neutral" & stats$home_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Home"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$away_div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
    weekly_boxscores$rest[i]=stats$Home_Rest_Type
    weekly_boxscores$stadium_id[i]=stats$stadium_id
  } else if (stats$location!="Neutral" & stats$away_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Away"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$home_div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
    weekly_boxscores$rest[i]=stats$Away_Rest_Type
    weekly_boxscores$stadium_id[i]=stats$stadium_id
  }
}
weekly_boxscores <- weekly_boxscores %>% 
  mutate(Month=month(game_date),
         Weekday=weekdays(as.Date.character(game_date)),
         position=case_when(player_id=="00-0033357"~"TE",
                            TRUE~position))
Team_Stats <- data.frame(unique(weekly_boxscores$opponent_team))
colnames(Team_Stats) <- "Team"
for (i in 1:nrow(Team_Stats)) {
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i])
  stats <- head(unique(data.frame(stats$game_date)),7)
  Team_Stats$Cutoff[i]=stats$stats.game_date[nrow(stats)]
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i])
  stat <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="QB")
  Team_Stats$OppPlays[i]=(sum(stats$attempts) + sum(stats$carries) + sum(stats$sacks))/7
  Team_Stats$`QB Pass Atts`[i]=sum(stat$attempts)/7
  Team_Stats$`QB Rush Atts`[i]=sum(stat$carries)/7
  Team_Stats$`QB Targets`[i]=sum(stat$targets)/7
  Team_Stats$`QB Pass Atts/Play`[i]=sum(stat$attempts)/(sum(stats$attempts) + sum(stats$carries) + sum(stats$sacks))
  Team_Stats$`QB Comp%`[i]=sum(stat$completions)/sum(stat$attempts)
  Team_Stats$`QB Yards/Att`[i]=sum(stat$passing_yards)/sum(stat$attempts)
  Team_Stats$`QB INT%`[i]=sum(stat$interceptions)/sum(stat$attempts)
  Team_Stats$`QB Pass TD%`[i]=sum(stat$passing_tds)/sum(stat$attempts)
  Team_Stats$`QB Rush Yards/Att`[i]=sum(stat$rushing_yards)/sum(stat$carries)
  Team_Stats$`QB TD%`[i]=(sum(stat$receiving_tds) + sum(stat$rushing_tds))/(sum(stat$carries) + sum(stat$targets))
  stat <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="RB")
  Team_Stats$`RB Rush Atts`[i]=sum(stat$carries)/7
  Team_Stats$`RB Targets`[i]=sum(stat$targets)/7
  Team_Stats$`RB Rush Yards/Att`[i]=sum(stat$rushing_yards)/sum(stat$carries)
  Team_Stats$`RB Rec%`[i]=sum(stat$receptions)/sum(stat$targets)
  Team_Stats$`RB Rec Yards/Tar`[i]=sum(stat$receiving_yards)/sum(stat$targets)
  Team_Stats$`RB TD%`[i]=(sum(stat$receiving_tds) + sum(stat$rushing_tds))/(sum(stat$carries) + sum(stat$targets))
  stat <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="WR")
  Team_Stats$`WR Rush Atts`[i]=sum(stat$carries)/7
  Team_Stats$`WR Targets`[i]=sum(stat$targets)/7
  Team_Stats$`WR Rush Yards/Att`[i]=sum(stat$rushing_yards)/sum(stat$carries)
  Team_Stats$`WR Rec%`[i]=sum(stat$receptions)/sum(stat$targets)
  Team_Stats$`WR Rec Yards/Tar`[i]=sum(stat$receiving_yards)/sum(stat$targets)
  Team_Stats$`WR TD%`[i]=(sum(stat$receiving_tds) + sum(stat$rushing_tds))/(sum(stat$carries) + sum(stat$targets))
  stat <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="TE")
  Team_Stats$`TE Rush Atts`[i]=sum(stat$carries)/7
  Team_Stats$`TE Targets`[i]=sum(stat$targets)/7
  Team_Stats$`TE Rec%`[i]=sum(stat$receptions)/sum(stat$targets)
  Team_Stats$`TE Rec Yards/Tar`[i]=sum(stat$receiving_yards)/sum(stat$targets)
  Team_Stats$`TE TD%`[i]=(sum(stat$receiving_tds) + sum(stat$rushing_tds))/(sum(stat$carries) + sum(stat$targets))
}
Props <- read.csv("Football Sims/nfl-player-props-overview.csv", skip = 1) %>% 
  select(Player, Team, Opp, `X.1`, `X.5`, `X.9`, `X.13`, `X.17`, `X.21`, `X.25`, `X.29`, `X.33`)
colnames(Props) <- c("Player", "Team", "Opp", "Comps", "Pass Att", "Pass Yds", "Passing TDs", "Ints", "Recs", "Rush + Rec Yds", "Rec Yds", "Rush Yds")
oldnames <- c("Riq Woolen", "AJ Barner", "Hollywood Brown", "De'Von Achane", "DJ Chark", "DeMario Douglas", "JaMycal Hasty", "Scotty Miller", "DJ Moore", "Drew Ogletree", "Joshua Palmer", "DJ Turner", "KaVontae Turpin", "Jeff Wilson")
newnames <- c("Tariq Woolen", "A.J. Barner", "Marquise Brown", "Devon Achane", "D.J. Chark", "Demario Douglas", "Jamycal Hasty", "Scott Miller", "D.J. Moore", "Andrew Ogletree", "Josh Palmer", "D.J. Turner", "Kavontae Turpin", "Jeffery Wilson")
Props$Player[Props$Player %in% oldnames] <- newnames[match(Props$Player, oldnames, nomatch = 0)]

Players=Props
oldteam <- "LA"
newteam <- "LAR"
Players$Team[Players$Team %in% oldteam] <- newteam[match(Players$Team, oldteam, nomatch = 0)]
Current_Week_Games$away_team[Current_Week_Games$away_team %in% oldteam] <- newteam[match(Current_Week_Games$away_team, oldteam, nomatch = 0)]
Current_Week_Games$home_team[Current_Week_Games$home_team %in% oldteam] <- newteam[match(Current_Week_Games$home_team, oldteam, nomatch = 0)]
weekly_boxscores$opponent_team[weekly_boxscores$opponent_team %in% oldteam] <- newteam[match(weekly_boxscores$opponent_team, oldteam, nomatch = 0)]
Team_Stats$Team[Team_Stats$Team %in% oldteam] <- newteam[match(Team_Stats$Team, oldteam, nomatch = 0)]
Todays_Games$away_team[Todays_Games$away_team %in% oldteam] <- newteam[match(Todays_Games$away_team, oldteam, nomatch = 0)]
Todays_Games$home_team[Todays_Games$home_team %in% oldteam] <- newteam[match(Todays_Games$home_team, oldteam, nomatch = 0)]

for (i in 1:nrow(Players)) {
  stats <- filter(Current_Week_Games, away_team==Players$Team[i] | home_team==Players$Team[i])
  if (stats$location=="Neutral" & stats$away_team==Players$Team[i]) {
    Players$Home_Away[i]="Neutral"
    Players$Opponent[i]=stats$home_team
    Players$DivGame[i]=stats$home_div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
    Players$Rest[i]=stats$Away_Rest_Type
    Players$Month[i]=month(as.Date(stats$gameday))
    Players$DayOfWeek[i]=weekdays(as.Date(stats$gameday))
    Players$Location[i]=stats$stadium_id
  } else if (stats$location=="Neutral" & stats$home_team==Players$Team[i]) {
    Players$Home_Away[i]="Neutral"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$away_div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
    Players$Rest[i]=stats$Home_Rest_Type
    Players$Month[i]=month(as.Date(stats$gameday))
    Players$DayOfWeek[i]=weekdays(as.Date(stats$gameday))
    Players$Location[i]=stats$stadium_id
  } else if (stats$location!="Neutral" & stats$away_team==Players$Team[i]) {
    Players$Home_Away[i]="Away"
    Players$Opponent[i]=stats$home_team
    Players$DivGame[i]=stats$home_div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
    Players$Rest[i]=stats$Away_Rest_Type
    Players$Month[i]=month(as.Date(stats$gameday))
    Players$DayOfWeek[i]=weekdays(as.Date(stats$gameday))
    Players$Location[i]=stats$stadium_id
  } else {
    Players$Home_Away[i]="Home"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$away_div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
    Players$Rest[i]=stats$Home_Rest_Type
    Players$Month[i]=month(as.Date(stats$gameday))
    Players$DayOfWeek[i]=weekdays(as.Date(stats$gameday))
    Players$Location[i]=stats$stadium_id
  }
  stats <- filter(weekly_boxscores, player_display_name==Players$Player[i])
  if (nrow(stats)>=5) {
    Players$PlayerId[i]=stats$player_id[1]
  } else {
    Players$PlayerId[i]=NA
  }
}
Players <- filter(Players, !is.na(Players$PlayerId))
for (i in 1:nrow(Players)) {
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 10)
  Players$Position[i]=stats$position[1]
  Players$Cutoff[i]=stats$game_date[nrow(stats)]
  stats <- filter(weekly_boxscores, opponent_team==Players$Opponent[i])
  stats <- head(data.frame(unique(stats$game_date)), 10)
  Players$OppCutoff[i]=stats$unique.stats.game_date.[nrow(stats)]
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
  if (Players$Position[i]=="QB") {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Plays`[i]=mean(stat$Plays)
    Players$`Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Pass Atts`[i]=mean(stats$attempts)
    Players$`Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Rush Att`[i]=mean(stats$carries)
    Players$`Rec%`[i]=NA
    Players$`Rec Yards/Tar`[i]=NA
    Players$`Targets`[i]=mean(stats$targets)
    Players$`TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE") {
    Players$`Pass Att/Play`[i]=NA
    Players$`Plays`[i]=NA
    Players$`Comp%`[i]=NA
    Players$`Pass Yards/Att`[i]=NA
    Players$`INT%`[i]=NA
    Players$`Passing TD%`[i]=NA
    Players$`Pass Atts`[i]=NA
    Players$`Rush Yards/Att`[i]=NA
    Players$`Rush Att`[i]=mean(stats$carries)
    Players$`Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Targets`[i]=mean(stats$targets)
    Players$`TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Pass Att/Play`[i]=NA
    Players$`Plays`[i]=NA
    Players$`Comp%`[i]=NA
    Players$`Pass Yards/Att`[i]=NA
    Players$`INT%`[i]=NA
    Players$`Passing TD%`[i]=NA
    Players$`Pass Atts`[i]=NA
    Players$`Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Rush Att`[i]=mean(stats$carries)
    Players$`Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Targets`[i]=mean(stats$targets)
    Players$`TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
  if (nrow(stats)<3) {
    Players$`Home/Away Pass Att/Play`[i]=NA
    Players$`Home/Away Comp%`[i]=NA
    Players$`Home/Away Pass Yards/Att`[i]=NA
    Players$`Home/Away INT%`[i]=NA
    Players$`Home/Away Passing TD%`[i]=NA
    Players$`Home/Away Rush Yards/Att`[i]=NA
    Players$`Home/Away Rec%`[i]=NA
    Players$`Home/Away Rec Yards/Tar`[i]=NA
    Players$`Home/Away TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Home/Away Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Home/Away Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Home/Away Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Home/Away INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Home/Away Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Home/Away Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Home/Away Rec%`[i]=NA
    Players$`Home/Away Rec Yards/Tar`[i]=NA
    Players$`Home/Away TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Home/Away Pass Att/Play`[i]=NA
    Players$`Home/Away Comp%`[i]=NA
    Players$`Home/Away Pass Yards/Att`[i]=NA
    Players$`Home/Away INT%`[i]=NA
    Players$`Home/Away Passing TD%`[i]=NA
    Players$`Home/Away Rush Yards/Att`[i]=NA
    Players$`Home/Away Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Home/Away Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Home/Away TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Home/Away Pass Att/Play`[i]=NA
    Players$`Home/Away Comp%`[i]=NA
    Players$`Home/Away Pass Yards/Att`[i]=NA
    Players$`Home/Away INT%`[i]=NA
    Players$`Home/Away Passing TD%`[i]=NA
    Players$`Home/Away Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Home/Away Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Home/Away Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Home/Away TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
  if (nrow(stats)<3) {
    Players$`vDiv Pass Att/Play`[i]=NA
    Players$`vDiv Comp%`[i]=NA
    Players$`vDiv Pass Yards/Att`[i]=NA
    Players$`vDiv INT%`[i]=NA
    Players$`vDiv Passing TD%`[i]=NA
    Players$`vDiv Rush Yards/Att`[i]=NA
    Players$`vDiv Rec%`[i]=NA
    Players$`vDiv Rec Yards/Tar`[i]=NA
    Players$`vDiv TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`vDiv Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`vDiv Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`vDiv Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`vDiv INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`vDiv Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`vDiv Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`vDiv Rec%`[i]=NA
    Players$`vDiv Rec Yards/Tar`[i]=NA
    Players$`vDiv TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`vDiv Pass Att/Play`[i]=NA
    Players$`vDiv Comp%`[i]=NA
    Players$`vDiv Pass Yards/Att`[i]=NA
    Players$`vDiv INT%`[i]=NA
    Players$`vDiv Passing TD%`[i]=NA
    Players$`vDiv Rush Yards/Att`[i]=NA
    Players$`vDiv Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`vDiv Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`vDiv TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`vDiv Pass Att/Play`[i]=NA
    Players$`vDiv Comp%`[i]=NA
    Players$`vDiv Pass Yards/Att`[i]=NA
    Players$`vDiv INT%`[i]=NA
    Players$`vDiv Passing TD%`[i]=NA
    Players$`vDiv Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`vDiv Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`vDiv Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`vDiv TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
  if (nrow(stats)<3) {
    Players$`Time of Game Pass Att/Play`[i]=NA
    Players$`Time of Game Comp%`[i]=NA
    Players$`Time of Game Pass Yards/Att`[i]=NA
    Players$`Time of Game INT%`[i]=NA
    Players$`Time of Game Passing TD%`[i]=NA
    Players$`Time of Game Rush Yards/Att`[i]=NA
    Players$`Time of Game Rec%`[i]=NA
    Players$`Time of Game Rec Yards/Tar`[i]=NA
    Players$`Time of Game TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Time of Game Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Time of Game Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Time of Game Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Time of Game INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Time of Game Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Time of Game Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Time of Game Rec%`[i]=NA
    Players$`Time of Game Rec Yards/Tar`[i]=NA
    Players$`Time of Game TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Time of Game Pass Att/Play`[i]=NA
    Players$`Time of Game Comp%`[i]=NA
    Players$`Time of Game Pass Yards/Att`[i]=NA
    Players$`Time of Game INT%`[i]=NA
    Players$`Time of Game Passing TD%`[i]=NA
    Players$`Time of Game Rush Yards/Att`[i]=NA
    Players$`Time of Game Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Time of Game Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Time of Game TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Time of Game Pass Att/Play`[i]=NA
    Players$`Time of Game Comp%`[i]=NA
    Players$`Time of Game Pass Yards/Att`[i]=NA
    Players$`Time of Game INT%`[i]=NA
    Players$`Time of Game Passing TD%`[i]=NA
    Players$`Time of Game Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Time of Game Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Time of Game Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Time of Game TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
  if (nrow(stats)<3) {
    Players$`Surface Pass Att/Play`[i]=NA
    Players$`Surface Comp%`[i]=NA
    Players$`Surface Pass Yards/Att`[i]=NA
    Players$`Surface INT%`[i]=NA
    Players$`Surface Passing TD%`[i]=NA
    Players$`Surface Rush Yards/Att`[i]=NA
    Players$`Surface Rec%`[i]=NA
    Players$`Surface Rec Yards/Tar`[i]=NA
    Players$`Surface TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Surface Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Surface Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Surface Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Surface INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Surface Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Surface Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Surface Rec%`[i]=NA
    Players$`Surface Rec Yards/Tar`[i]=NA
    Players$`Surface TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Surface Pass Att/Play`[i]=NA
    Players$`Surface Comp%`[i]=NA
    Players$`Surface Pass Yards/Att`[i]=NA
    Players$`Surface INT%`[i]=NA
    Players$`Surface Passing TD%`[i]=NA
    Players$`Surface Rush Yards/Att`[i]=NA
    Players$`Surface Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Surface Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Surface TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Surface Pass Att/Play`[i]=NA
    Players$`Surface Comp%`[i]=NA
    Players$`Surface Pass Yards/Att`[i]=NA
    Players$`Surface INT%`[i]=NA
    Players$`Surface Passing TD%`[i]=NA
    Players$`Surface Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Surface Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Surface Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Surface TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & rest==Players$Rest[i]), 7)
  if (nrow(stats)<3) {
    Players$`Rest Pass Att/Play`[i]=NA
    Players$`Rest Comp%`[i]=NA
    Players$`Rest Pass Yards/Att`[i]=NA
    Players$`Rest INT%`[i]=NA
    Players$`Rest Passing TD%`[i]=NA
    Players$`Rest Rush Yards/Att`[i]=NA
    Players$`Rest Rec%`[i]=NA
    Players$`Rest Rec Yards/Tar`[i]=NA
    Players$`Rest TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Rest Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Rest Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Rest Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Rest INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Rest Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Rest Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Rest Rec%`[i]=NA
    Players$`Rest Rec Yards/Tar`[i]=NA
    Players$`Rest TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Rest Pass Att/Play`[i]=NA
    Players$`Rest Comp%`[i]=NA
    Players$`Rest Pass Yards/Att`[i]=NA
    Players$`Rest INT%`[i]=NA
    Players$`Rest Passing TD%`[i]=NA
    Players$`Rest Rush Yards/Att`[i]=NA
    Players$`Rest Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Rest Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Rest TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Rest Pass Att/Play`[i]=NA
    Players$`Rest Comp%`[i]=NA
    Players$`Rest Pass Yards/Att`[i]=NA
    Players$`Rest INT%`[i]=NA
    Players$`Rest Passing TD%`[i]=NA
    Players$`Rest Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Rest Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Rest Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Rest TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & stadium_id==Players$Location[i]), 7)
  if (nrow(stats)<3) {
    Players$`Stadium Pass Att/Play`[i]=NA
    Players$`Stadium Comp%`[i]=NA
    Players$`Stadium Pass Yards/Att`[i]=NA
    Players$`Stadium INT%`[i]=NA
    Players$`Stadium Passing TD%`[i]=NA
    Players$`Stadium Rush Yards/Att`[i]=NA
    Players$`Stadium Rec%`[i]=NA
    Players$`Stadium Rec Yards/Tar`[i]=NA
    Players$`Stadium TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Stadium Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Stadium Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Stadium Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Stadium INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Stadium Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Stadium Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Stadium Rec%`[i]=NA
    Players$`Stadium Rec Yards/Tar`[i]=NA
    Players$`Stadium TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Stadium Pass Att/Play`[i]=NA
    Players$`Stadium Comp%`[i]=NA
    Players$`Stadium Pass Yards/Att`[i]=NA
    Players$`Stadium INT%`[i]=NA
    Players$`Stadium Passing TD%`[i]=NA
    Players$`Stadium Rush Yards/Att`[i]=NA
    Players$`Stadium Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Stadium Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Stadium TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Stadium Pass Att/Play`[i]=NA
    Players$`Stadium Comp%`[i]=NA
    Players$`Stadium Pass Yards/Att`[i]=NA
    Players$`Stadium INT%`[i]=NA
    Players$`Stadium Passing TD%`[i]=NA
    Players$`Stadium Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Stadium Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Stadium Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Stadium TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==Players$Month[i]), 7)
  if (nrow(stats)<3) {
    Players$`Month Pass Att/Play`[i]=NA
    Players$`Month Comp%`[i]=NA
    Players$`Month Pass Yards/Att`[i]=NA
    Players$`Month INT%`[i]=NA
    Players$`Month Passing TD%`[i]=NA
    Players$`Month Rush Yards/Att`[i]=NA
    Players$`Month Rec%`[i]=NA
    Players$`Month Rec Yards/Tar`[i]=NA
    Players$`Month TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Month Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Month Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Month Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Month INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Month Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Month Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Month Rec%`[i]=NA
    Players$`Month Rec Yards/Tar`[i]=NA
    Players$`Month TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Month Pass Att/Play`[i]=NA
    Players$`Month Comp%`[i]=NA
    Players$`Month Pass Yards/Att`[i]=NA
    Players$`Month INT%`[i]=NA
    Players$`Month Passing TD%`[i]=NA
    Players$`Month Rush Yards/Att`[i]=NA
    Players$`Month Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Month Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Month TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Month Pass Att/Play`[i]=NA
    Players$`Month Comp%`[i]=NA
    Players$`Month Pass Yards/Att`[i]=NA
    Players$`Month INT%`[i]=NA
    Players$`Month Passing TD%`[i]=NA
    Players$`Month Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Month Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Month Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Month TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==Players$DayOfWeek[i]), 7)
  if (nrow(stats)<3) {
    Players$`Day of Week Pass Att/Play`[i]=NA
    Players$`Day of Week Comp%`[i]=NA
    Players$`Day of Week Pass Yards/Att`[i]=NA
    Players$`Day of Week INT%`[i]=NA
    Players$`Day of Week Passing TD%`[i]=NA
    Players$`Day of Week Rush Yards/Att`[i]=NA
    Players$`Day of Week Rec%`[i]=NA
    Players$`Day of Week Rec Yards/Tar`[i]=NA
    Players$`Day of Week TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Day of Week Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Day of Week Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Day of Week Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Day of Week INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Day of Week Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Day of Week Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Day of Week Rec%`[i]=NA
    Players$`Day of Week Rec Yards/Tar`[i]=NA
    Players$`Day of Week TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Day of Week Pass Att/Play`[i]=NA
    Players$`Day of Week Comp%`[i]=NA
    Players$`Day of Week Pass Yards/Att`[i]=NA
    Players$`Day of Week INT%`[i]=NA
    Players$`Day of Week Passing TD%`[i]=NA
    Players$`Day of Week Rush Yards/Att`[i]=NA
    Players$`Day of Week Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Day of Week Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Day of Week TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Day of Week Pass Att/Play`[i]=NA
    Players$`Day of Week Comp%`[i]=NA
    Players$`Day of Week Pass Yards/Att`[i]=NA
    Players$`Day of Week INT%`[i]=NA
    Players$`Day of Week Passing TD%`[i]=NA
    Players$`Day of Week Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Day of Week Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Day of Week Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Day of Week TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
  if (nrow(stats)<3) {
    Players$`Opponent Pass Att/Play`[i]=NA
    Players$`Opponent Comp%`[i]=NA
    Players$`Opponent Pass Yards/Att`[i]=NA
    Players$`Opponent INT%`[i]=NA
    Players$`Opponent Passing TD%`[i]=NA
    Players$`Opponent Rush Yards/Att`[i]=NA
    Players$`Opponent Rec%`[i]=NA
    Players$`Opponent Rec Yards/Tar`[i]=NA
    Players$`Opponent TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Opponent Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Opponent Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Opponent Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Opponent INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Opponent Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Opponent Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Opponent Rec%`[i]=NA
    Players$`Opponent Rec Yards/Tar`[i]=NA
    Players$`Opponent TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Opponent Pass Att/Play`[i]=NA
    Players$`Opponent Comp%`[i]=NA
    Players$`Opponent Pass Yards/Att`[i]=NA
    Players$`Opponent INT%`[i]=NA
    Players$`Opponent Passing TD%`[i]=NA
    Players$`Opponent Rush Yards/Att`[i]=NA
    Players$`Opponent Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Opponent Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Opponent TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Opponent Pass Att/Play`[i]=NA
    Players$`Opponent Comp%`[i]=NA
    Players$`Opponent Pass Yards/Att`[i]=NA
    Players$`Opponent INT%`[i]=NA
    Players$`Opponent Passing TD%`[i]=NA
    Players$`Opponent Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Opponent Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Opponent Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Opponent TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & season_type!="REG"), 7)
  if (nrow(stats)<3) {
    Players$`Playoff Pass Att/Play`[i]=NA
    Players$`Playoff Comp%`[i]=NA
    Players$`Playoff Pass Yards/Att`[i]=NA
    Players$`Playoff INT%`[i]=NA
    Players$`Playoff Passing TD%`[i]=NA
    Players$`Playoff Rush Yards/Att`[i]=NA
    Players$`Playoff Rec%`[i]=NA
    Players$`Playoff Rec Yards/Tar`[i]=NA
    Players$`Playoff TD%`[i]=NA
  } else if (Players$Position[i]=="QB" & nrow(stats)>=3) {
    stat <- stats %>% 
      select(recent_team, season, week)
    for (l in 1:nrow(stat)) {
      df <- filter(weekly_boxscores, recent_team==stat$recent_team[l] & season==stat$season[l] & week==stat$week[l])
      stat$Plays[l]=sum(df$attempts) + sum(df$sacks) + sum(df$carries)
    }
    Players$`Playoff Pass Att/Play`[i]=sum(stats$attempts)/sum(stat$Plays)
    Players$`Playoff Comp%`[i]=sum(stats$completions)/sum(stats$attempts)
    Players$`Playoff Pass Yards/Att`[i]=sum(stats$passing_yards)/sum(stats$attempts)
    Players$`Playoff INT%`[i]=sum(stats$interceptions)/sum(stats$attempts)
    Players$`Playoff Passing TD%`[i]=sum(stats$passing_tds)/sum(stats$attempts)
    Players$`Playoff Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Playoff Rec%`[i]=NA
    Players$`Playoff Rec Yards/Tar`[i]=NA
    Players$`Playoff TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else if (Players$Position[i]=="TE" & nrow(stats)>=3) {
    Players$`Playoff Pass Att/Play`[i]=NA
    Players$`Playoff Comp%`[i]=NA
    Players$`Playoff Pass Yards/Att`[i]=NA
    Players$`Playoff INT%`[i]=NA
    Players$`Playoff Passing TD%`[i]=NA
    Players$`Playoff Rush Yards/Att`[i]=NA
    Players$`Playoff Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Playoff Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Playoff TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  } else {
    Players$`Playoff Pass Att/Play`[i]=NA
    Players$`Playoff Comp%`[i]=NA
    Players$`Playoff Pass Yards/Att`[i]=NA
    Players$`Playoff INT%`[i]=NA
    Players$`Playoff Passing TD%`[i]=NA
    Players$`Playoff Rush Yards/Att`[i]=sum(stats$rushing_yards)/sum(stats$carries)
    Players$`Playoff Rec%`[i]=sum(stats$receptions)/sum(stats$targets)
    Players$`Playoff Rec Yards/Tar`[i]=sum(stats$receiving_yards)/sum(stats$targets)
    Players$`Playoff TD%`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/(sum(stats$carries) + sum(stats$targets))
  }
  stats <- filter(Team_Stats, Team==Players$Opponent[i])
  Players[i,129:156]=stats[3:30]
}
Bets <- data.frame()
stats <- filter(Players, !is.na(`Pass Att`)) %>% 
  select(`Pass Att/Play`, `Home/Away Pass Att/Play`, `vDiv Pass Att/Play`, `Time of Game Pass Att/Play`, `Surface Pass Att/Play`, `Rest Pass Att/Play`, `Stadium Pass Att/Play`, `Month Pass Att/Play`, `Day of Week Pass Att/Play`, `Opponent Pass Att/Play`, `Playoff Pass Att/Play`, Plays, `QB Pass Atts/Play`, `Pass Att`, Player, Team, Opp, OppPlays)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB Pass Atts/Play`[i]/mean(Team_Stats$`QB Pass Atts/Play`))*(stats$Plays[i]*(stats$OppPlays[i]/mean(Team_Stats$OppPlays)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Pass Att`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Pass Att`, "Passing Attempts"),
         PercentChange=(Projection-`Pass Att`)/`Pass Att`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Comps`)) %>% 
  select(`Comp%`, `Home/Away Comp%`, `vDiv Comp%`, `Time of Game Comp%`, `Surface Comp%`, `Rest Comp%`, `Stadium Comp%`, `Month Comp%`, `Day of Week Comp%`, `Opponent Comp%`, `Playoff Comp%`, `Pass Atts`, `QB Comp%`, `Comps`, Player, Team, Opp, `QB Pass Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB Comp%`[i]/mean(Team_Stats$`QB Comp%`))*(stats$`Pass Atts`[i]*(stats$`QB Pass Atts`[i]/mean(Team_Stats$`QB Pass Atts`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Comps`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Comps`, "Completions"),
         PercentChange=(Projection-`Comps`)/`Comps`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Pass Yds`)) %>% 
  select(`Pass Yards/Att`, `Home/Away Pass Yards/Att`, `vDiv Pass Yards/Att`, `Time of Game Pass Yards/Att`, `Surface Pass Yards/Att`, `Rest Pass Yards/Att`, `Stadium Pass Yards/Att`, `Month Pass Yards/Att`, `Day of Week Pass Yards/Att`, `Opponent Pass Yards/Att`, `Playoff Pass Yards/Att`, `Pass Atts`, `QB Yards/Att`, `Pass Yds`, Player, Team, Opp, `QB Pass Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB Yards/Att`[i]/mean(Team_Stats$`QB Yards/Att`))*(stats$`Pass Atts`[i]*(stats$`QB Pass Atts`[i]/mean(Team_Stats$`QB Pass Atts`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Pass Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Pass Yds`, "Pass Yds"),
         PercentChange=(Projection-`Pass Yds`)/`Pass Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Pass Att`)) %>% 
  select(`INT%`, `Home/Away INT%`, `vDiv INT%`, `Time of Game INT%`, `Surface INT%`, `Rest INT%`, `Stadium INT%`, `Month INT%`, `Day of Week INT%`, `Opponent INT%`, `Playoff INT%`, `Pass Atts`, `QB INT%`, Player, Team, Opp, `QB Pass Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB INT%`[i]/mean(Team_Stats$`QB INT%`))*(stats$`Pass Atts`[i]*(stats$`QB Pass Atts`[i]/mean(Team_Stats$`QB Pass Atts`)))
  stats$`Value`[i]=stats$Projection[i]-1
}
stats <- stats %>% 
  mutate(Prop="1+ INTs",
         PercentChange=(Projection-1)/1) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Pass Att`)) %>% 
  select(`Passing TD%`, `Home/Away Passing TD%`, `vDiv Passing TD%`, `Time of Game Passing TD%`, `Surface Passing TD%`, `Rest Passing TD%`, `Stadium Passing TD%`, `Month Passing TD%`, `Day of Week Passing TD%`, `Opponent Passing TD%`, `Playoff Passing TD%`, `Pass Atts`, `QB Pass TD%`, Player, Team, Opp, `QB Pass Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB Pass TD%`[i]/mean(Team_Stats$`QB Pass TD%`))*(stats$`Pass Atts`[i]*(stats$`QB Pass Atts`[i]/mean(Team_Stats$`QB Pass Atts`)))
  stats$`Value`[i]=stats$Projection[i]-1.5
}
stats <- stats %>% 
  mutate(Prop="2+ Pass TDs",
         PercentChange=(Projection-2)/2) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Recs`) & Position=="RB") %>% 
  select(`Rec%`, `Home/Away Rec%`, `vDiv Rec%`, `Time of Game Rec%`, `Surface Rec%`, `Rest Rec%`, `Stadium Rec%`, `Month Rec%`, `Day of Week Rec%`, `Opponent Rec%`, `Playoff Rec%`, Targets, `Recs`, `RB Rec%`, Player, Team, Opp, `RB Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`RB Rec%`[i]/mean(Team_Stats$`RB Rec%`))*(stats$`Targets`[i]*(sum(stats$`RB Targets`[i])/mean(Team_Stats$`RB Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$Recs[i]
}
stats <- stats %>% 
  mutate(Prop=paste(Recs, "Receptions"),
         PercentChange=(Projection-`Recs`)/`Recs`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Recs`) & Position=="WR") %>% 
  select(`Rec%`, `Home/Away Rec%`, `vDiv Rec%`, `Time of Game Rec%`, `Surface Rec%`, `Rest Rec%`, `Stadium Rec%`, `Month Rec%`, `Day of Week Rec%`, `Opponent Rec%`, `Playoff Rec%`, Targets, `Recs`, `WR Rec%`, Player, Team, Opp, `WR Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`WR Rec%`[i]/mean(Team_Stats$`WR Rec%`))*(stats$`Targets`[i]*(sum(stats$`WR Targets`[i])/mean(Team_Stats$`WR Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$Recs[i]
}
stats <- stats %>% 
  mutate(Prop=paste(Recs, "Receptions"),
         PercentChange=(Projection-`Recs`)/`Recs`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Recs`) & Position=="TE") %>% 
  select(`Rec%`, `Home/Away Rec%`, `vDiv Rec%`, `Time of Game Rec%`, `Surface Rec%`, `Rest Rec%`, `Stadium Rec%`, `Month Rec%`, `Day of Week Rec%`, `Opponent Rec%`, `Playoff Rec%`, Targets, `Recs`, `TE Rec%`, Player, Team, Opp, `TE Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`TE Rec%`[i]/mean(Team_Stats$`TE Rec%`))*(stats$`Targets`[i]*(sum(stats$`TE Targets`[i])/mean(Team_Stats$`TE Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$Recs[i]
}
stats <- stats %>% 
  mutate(Prop=paste(Recs, "Receptions"),
         PercentChange=(Projection-`Recs`)/`Recs`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rec Yds`) & Position=="RB") %>% 
  select(`Rec Yards/Tar`, `Home/Away Rec Yards/Tar`, `vDiv Rec Yards/Tar`, `Time of Game Rec Yards/Tar`, `Surface Rec Yards/Tar`, `Rest Rec Yards/Tar`, `Stadium Rec Yards/Tar`, `Month Rec Yards/Tar`, `Day of Week Rec Yards/Tar`, `Opponent Rec Yards/Tar`, `Playoff Rec Yards/Tar`, Targets, `Rec Yds`, `RB Rec Yards/Tar`, Player, Team, Opp, `RB Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`RB Rec Yards/Tar`[i]/mean(Team_Stats$`RB Rec Yards/Tar`))*(stats$`Targets`[i]*(sum(stats$`RB Targets`[i])/mean(Team_Stats$`RB Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rec Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rec Yds`, "Rec Yards"),
         PercentChange=(Projection-`Rec Yds`)/`Rec Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rec Yds`) & Position=="WR") %>% 
  select(`Rec Yards/Tar`, `Home/Away Rec Yards/Tar`, `vDiv Rec Yards/Tar`, `Time of Game Rec Yards/Tar`, `Surface Rec Yards/Tar`, `Rest Rec Yards/Tar`, `Stadium Rec Yards/Tar`, `Month Rec Yards/Tar`, `Day of Week Rec Yards/Tar`, `Opponent Rec Yards/Tar`, `Playoff Rec Yards/Tar`, Targets, `Rec Yds`, `WR Rec Yards/Tar`, Player, Team, Opp, `WR Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`WR Rec Yards/Tar`[i]/mean(Team_Stats$`WR Rec Yards/Tar`))*(stats$`Targets`[i]*(sum(stats$`WR Targets`[i])/mean(Team_Stats$`WR Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rec Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rec Yds`, "Rec Yards"),
         PercentChange=(Projection-`Rec Yds`)/`Rec Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rec Yds`) & Position=="TE") %>% 
  select(`Rec Yards/Tar`, `Home/Away Rec Yards/Tar`, `vDiv Rec Yards/Tar`, `Time of Game Rec Yards/Tar`, `Surface Rec Yards/Tar`, `Rest Rec Yards/Tar`, `Stadium Rec Yards/Tar`, `Month Rec Yards/Tar`, `Day of Week Rec Yards/Tar`, `Opponent Rec Yards/Tar`, `Playoff Rec Yards/Tar`, Targets, `Rec Yds`, `TE Rec Yards/Tar`, Player, Team, Opp, `TE Targets`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`TE Rec Yards/Tar`[i]/mean(Team_Stats$`TE Rec Yards/Tar`))*(stats$`Targets`[i]*(sum(stats$`TE Targets`[i])/mean(Team_Stats$`TE Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rec Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rec Yds`, "Rec Yards"),
         PercentChange=(Projection-`Rec Yds`)/`Rec Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush Yds`) & Position=="QB") %>% 
  select(`Rush Yards/Att`, `Home/Away Rush Yards/Att`, `vDiv Rush Yards/Att`, `Time of Game Rush Yards/Att`, `Surface Rush Yards/Att`, `Rest Rush Yards/Att`, `Stadium Rush Yards/Att`, `Month Rush Yards/Att`, `Day of Week Rush Yards/Att`, `Opponent Rush Yards/Att`, `Playoff Rush Yards/Att`, `Rush Att`, `Rush Yds`, `QB Rush Yards/Att`, Player, Team, Opp, `QB Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`QB Rush Yards/Att`[i]/mean(Team_Stats$`QB Rush Yards/Att`))*(stats$`Rush Att`[i]*(sum(stats$`QB Rush Atts`[i])/mean(Team_Stats$`QB Rush Atts`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rush Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rush Yds`, "Rush Yards"),
         PercentChange=(Projection-`Rush Yds`)/`Rush Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush Yds`) & Position=="RB") %>% 
  select(`Rush Yards/Att`, `Home/Away Rush Yards/Att`, `vDiv Rush Yards/Att`, `Time of Game Rush Yards/Att`, `Surface Rush Yards/Att`, `Rest Rush Yards/Att`, `Stadium Rush Yards/Att`, `Month Rush Yards/Att`, `Day of Week Rush Yards/Att`, `Opponent Rush Yards/Att`, `Playoff Rush Yards/Att`, `Rush Att`, `Rush Yds`, `RB Rush Yards/Att`, Player, Team, Opp, `RB Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`RB Rush Yards/Att`[i]/mean(Team_Stats$`RB Rush Yards/Att`))*(stats$`Rush Att`[i]*(sum(stats$`RB Rush Atts`[i])/mean(Team_Stats$`RB Rush Atts`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rush Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rush Yds`, "Rush Yards"),
         PercentChange=(Projection-`Rush Yds`)/`Rush Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush Yds`) & Position=="WR") %>% 
  select(`Rush Yards/Att`, `Home/Away Rush Yards/Att`, `vDiv Rush Yards/Att`, `Time of Game Rush Yards/Att`, `Surface Rush Yards/Att`, `Rest Rush Yards/Att`, `Stadium Rush Yards/Att`, `Month Rush Yards/Att`, `Day of Week Rush Yards/Att`, `Opponent Rush Yards/Att`, `Playoff Rush Yards/Att`, `Rush Att`, `Rush Yds`, `WR Rush Yards/Att`, Player, Team, Opp, `WR Rush Atts`)
if (nrow(stats)>0) {
  for (i in 1:nrow(stats)) {
    stats$`Projection`[i]=(sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`WR Rush Yards/Att`[i]/mean(Team_Stats$`WR Rush Yards/Att`))*(stats$`Rush Att`[i]*(sum(stats$`WR Rush Atts`[i])/mean(Team_Stats$`WR Rush Atts`)))
    stats$`Value`[i]=stats$Projection[i]-stats$`Rush Yds`[i]
  }
  stats <- stats %>% 
    mutate(Prop=paste(`Rush Yds`, "Rush Yards"),
           PercentChange=(Projection-`Rush Yds`)/`Rush Yds`) %>% 
    select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
} else {
  stats <- data.frame()
}
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush + Rec Yds`) & Position=="RB") %>% 
  select(`Rush Yards/Att`, `Home/Away Rush Yards/Att`, `vDiv Rush Yards/Att`, `Time of Game Rush Yards/Att`, `Surface Rush Yards/Att`, `Rest Rush Yards/Att`, `Stadium Rush Yards/Att`, `Month Rush Yards/Att`, `Day of Week Rush Yards/Att`, `Opponent Rush Yards/Att`, `Playoff Rush Yards/Att`, `Rush Att`, `RB Rush Yards/Att`, `Rec Yards/Tar`, `Home/Away Rec Yards/Tar`, `vDiv Rec Yards/Tar`, `Time of Game Rec Yards/Tar`, `Surface Rec Yards/Tar`, `Rest Rec Yards/Tar`, `Stadium Rec Yards/Tar`, `Month Rec Yards/Tar`, `Day of Week Rec Yards/Tar`, `Opponent Rec Yards/Tar`, `Playoff Rec Yards/Tar`, Targets, `RB Rec Yards/Tar`, `Rush + Rec Yds`, Player, Team, Opp, `RB Targets`, `RB Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$`Projection`[i]=((sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`RB Rush Yards/Att`[i]/mean(Team_Stats$`RB Rush Yards/Att`))*(stats$`Rush Att`[i]*(sum(stats$`RB Rush Atts`[i])/mean(Team_Stats$`RB Rush Atts`)))) + (sum(stats[i, 14:24], na.rm = TRUE)/(sum(!is.na(stats[i, 14:24]))))*(stats$`RB Rec Yards/Tar`[i]/mean(Team_Stats$`RB Rec Yards/Tar`))*(stats$`Targets`[i]*(sum(stats$`RB Targets`[i])/mean(Team_Stats$`RB Targets`)))
  stats$`Value`[i]=stats$Projection[i]-stats$`Rush + Rec Yds`[i]
}
stats <- stats %>% 
  mutate(Prop=paste(`Rush + Rec Yds`, "Rush + Rec Yards"),
         PercentChange=(Projection-`Rush + Rec Yds`)/`Rush + Rec Yds`) %>% 
  select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush + Rec Yds`) & Position=="WR") %>% 
  select(`Rush Yards/Att`, `Home/Away Rush Yards/Att`, `vDiv Rush Yards/Att`, `Time of Game Rush Yards/Att`, `Surface Rush Yards/Att`, `Rest Rush Yards/Att`, `Stadium Rush Yards/Att`, `Month Rush Yards/Att`, `Day of Week Rush Yards/Att`, `Opponent Rush Yards/Att`, `Playoff Rush Yards/Att`, `Rush Att`, `WR Rush Yards/Att`, `Rec Yards/Tar`, `Home/Away Rec Yards/Tar`, `vDiv Rec Yards/Tar`, `Time of Game Rec Yards/Tar`, `Surface Rec Yards/Tar`, `Rest Rec Yards/Tar`, `Stadium Rec Yards/Tar`, `Month Rec Yards/Tar`, `Day of Week Rec Yards/Tar`, `Opponent Rec Yards/Tar`, `Playoff Rec Yards/Tar`, Targets, `WR Rec Yards/Tar`, `Rush + Rec Yds`, Player, Team, Opp, `WR Targets`, `WR Rush Atts`)
if (nrow(stats)>0) {
  for (i in 1:nrow(stats)) {
    stats$`Projection`[i]=((sum(stats[i, 1:11], na.rm = TRUE)/(sum(!is.na(stats[i, 1:11]))))*(stats$`WR Rush Yards/Att`[i]/mean(Team_Stats$`WR Rush Yards/Att`))*(stats$`Rush Att`[i]*(sum(stats$`WR Rush Atts`[i])/mean(Team_Stats$`WR Rush Atts`)))) + (sum(stats[i, 14:24], na.rm = TRUE)/(sum(!is.na(stats[i, 14:24]))))*(stats$`WR Rec Yards/Tar`[i]/mean(Team_Stats$`WR Rec Yards/Tar`))*(stats$`Targets`[i]*(sum(stats$`WR Targets`[i])/mean(Team_Stats$`WR Targets`)))
    stats$`Value`[i]=stats$Projection[i]-stats$`Rush + Rec Yds`[i]
  }
  stats <- stats %>% 
    mutate(Prop=paste(`Rush + Rec Yds`, "Rush + Rec Yards"),
           PercentChange=(Projection-`Rush + Rec Yds`)/`Rush + Rec Yds`) %>% 
    select(Player, Team, Opp, Prop, Projection, Value, PercentChange)
} else {
  stats <- data.frame()
}
Bets <- rbind(Bets, stats) %>% 
  arrange(desc(PercentChange)) %>% 
  select(-PercentChange)
view(Bets)
TD_Bets <- data.frame()
stats <- filter(Players, Position=="QB") %>% 
  select(`TD%`, `Home/Away TD%`, `vDiv TD%`, `Time of Game TD%`, `Surface TD%`, `Rest TD%`, `Stadium TD%`, `Month TD%`, `Day of Week TD%`, `Opponent TD%`, `Playoff TD%`, `Rush Att`, Targets, `QB TD%`, Player, Team, Opp, `QB Targets`, `QB Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$Projection[i]=((sum(stats[i, 1:11], na.rm = TRUE)/sum(!is.na(stats[i, 1:11])))*(stats$`QB TD%`[i]/mean(Team_Stats$`QB TD%`)))*((stats$`Rush Att`[i] + stats$Targets[i])*(stats$`QB Rush Atts`[i]+stats$`QB Targets`[i])/mean(Team_Stats$`QB Rush Atts` + Team_Stats$`QB Targets`))
}
stats <- stats %>% 
  mutate(Prop="1+ TDs") %>% 
  select(Player, Team, Opp, Prop, Projection)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="RB") %>% 
  select(`TD%`, `Home/Away TD%`, `vDiv TD%`, `Time of Game TD%`, `Surface TD%`, `Rest TD%`, `Stadium TD%`, `Month TD%`, `Day of Week TD%`, `Opponent TD%`, `Playoff TD%`, `Rush Att`, Targets, `RB TD%`, Player, Team, Opp, `RB Targets`, `RB Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$Projection[i]=((sum(stats[i, 1:11], na.rm = TRUE)/sum(!is.na(stats[i, 1:11])))*(stats$`RB TD%`[i]/mean(Team_Stats$`RB TD%`)))*((stats$`Rush Att`[i] + stats$Targets[i])*(stats$`RB Rush Atts`[i]+stats$`RB Targets`[i])/mean(Team_Stats$`RB Rush Atts` + Team_Stats$`RB Targets`))
}
stats <- stats %>% 
  mutate(Prop="1+ TDs") %>% 
  select(Player, Team, Opp, Prop, Projection)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="WR") %>% 
  select(`TD%`, `Home/Away TD%`, `vDiv TD%`, `Time of Game TD%`, `Surface TD%`, `Rest TD%`, `Stadium TD%`, `Month TD%`, `Day of Week TD%`, `Opponent TD%`, `Playoff TD%`, `Rush Att`, Targets, `WR TD%`, Player, Team, Opp, `WR Targets`, `WR Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$Projection[i]=((sum(stats[i, 1:11], na.rm = TRUE)/sum(!is.na(stats[i, 1:11])))*(stats$`WR TD%`[i]/mean(Team_Stats$`WR TD%`)))*((stats$`Rush Att`[i] + stats$Targets[i])*(stats$`WR Rush Atts`[i]+stats$`WR Targets`[i])/mean(Team_Stats$`WR Rush Atts` + Team_Stats$`WR Targets`))
}
stats <- stats %>% 
  mutate(Prop="1+ TDs") %>% 
  select(Player, Team, Opp, Prop, Projection)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="TE") %>% 
  select(`TD%`, `Home/Away TD%`, `vDiv TD%`, `Time of Game TD%`, `Surface TD%`, `Rest TD%`, `Stadium TD%`, `Month TD%`, `Day of Week TD%`, `Opponent TD%`, `Playoff TD%`, `Rush Att`, Targets, `TE TD%`, Player, Team, Opp, `TE Targets`, `TE Rush Atts`)
for (i in 1:nrow(stats)) {
  stats$Projection[i]=((sum(stats[i, 1:11], na.rm = TRUE)/sum(!is.na(stats[i, 1:11])))*(stats$`TE TD%`[i]/mean(Team_Stats$`TE TD%`)))*((stats$`Rush Att`[i] + stats$Targets[i])*(stats$`TE Rush Atts`[i]+stats$`TE Targets`[i])/mean(Team_Stats$`TE Rush Atts` + Team_Stats$`TE Targets`))
}
stats <- stats %>% 
  mutate(Prop="1+ TDs") %>% 
  select(Player, Team, Opp, Prop, Projection)
TD_Bets <- rbind(TD_Bets, stats) %>% 
  arrange(desc(Projection))
view(TD_Bets)
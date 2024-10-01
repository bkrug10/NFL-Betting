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
                                TRUE~"Morning"))
Todays_Games <- filter(schedule, gameday==today())
Current_Week_Games <- filter(schedule, season==get_current_season() & week==get_current_week())
oldteam <- c("OAK", "SD", "STL")
newteam <- c("LV", "LAC", "LA")
schedule$home_team[schedule$home_team %in% oldteam] <- newteam[match(schedule$home_team, oldteam, nomatch = 0)]
schedule$away_team[schedule$away_team %in% oldteam] <- newteam[match(schedule$away_team, oldteam, nomatch = 0)]
if (Todays_Games$game_type[1]=="REG") {
  weekly_boxscores <- filter(weekly_boxscores, season_type=="REG")
} else {
  weekly_boxscores <- weekly_boxscores
}
for (i in 1:nrow(weekly_boxscores)) {
  stats <- filter(schedule, season==weekly_boxscores$season[i] & week==weekly_boxscores$week[i] & (away_team==weekly_boxscores$recent_team[i] | home_team==weekly_boxscores$recent_team[i]))
  if (stats$location=="Neutral") {
    weekly_boxscores$Home_Away[i]="Neutral"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
  } else if (stats$location!="Neutral" & stats$home_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Home"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
  } else if (stats$location!="Neutral" & stats$away_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Away"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$div_game
    weekly_boxscores$time_of_game[i]=stats$time_of_game
    weekly_boxscores$surface[i]=stats$surface
  }
}
weekly_boxscores <- weekly_boxscores %>% 
  mutate(Month=month(game_date),
         Weekday=weekdays(as.Date.character(game_date)),
         position=case_when(player_id=="00-0033357"~"TE",
                            TRUE~position))
Team_Stats <- data.frame(unique(weekly_boxscores$recent_team))
colnames(Team_Stats) <- "Team"
for (i in 1:nrow(Team_Stats)) {
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i])
  stats <- head(unique(data.frame(stats$game_date)),7)
  Team_Stats$Cutoff[i]=stats$stats.game_date[nrow(stats)]
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="QB")
  Team_Stats$`QB Passing Yards`[i]=sum(stats$passing_yards)/7
  Team_Stats$`QB Passing Attempts`[i]=sum(stats$attempts)/7
  Team_Stats$`QB Passing Completions`[i]=sum(stats$completions)/7
  Team_Stats$`QB Passing TDs`[i]=sum(stats$passing_tds)/7
  Team_Stats$`QB Passing INTs`[i]=sum(stats$interceptions)/7
  Team_Stats$`QB Rushing Yards`[i]=sum(stats$rushing_yards)/7
  Team_Stats$`QB TDs`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/7
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="RB")
  Team_Stats$`RB Rushing Yards`[i]=sum(stats$rushing_yards)/7
  Team_Stats$`RB Rushing Attempts`[i]=sum(stats$carries)/7
  Team_Stats$`RB Receiving Yards`[i]=sum(stats$receiving_yards)/7
  Team_Stats$`RB Receptions`[i]=sum(stats$receptions)/7
  Team_Stats$`RB TDs`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/7
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="WR")
  Team_Stats$`WR Rushing Yards`[i]=sum(stats$rushing_yards)/7
  Team_Stats$`WR Receiving Yards`[i]=sum(stats$receiving_yards)/7
  Team_Stats$`WR Receptions`[i]=sum(stats$receptions)/7
  Team_Stats$`WR TDs`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/7
  stats <- filter(weekly_boxscores, opponent_team==Team_Stats$Team[i] & game_date>=Team_Stats$Cutoff[i] & position=="TE")
  Team_Stats$`TE Receiving Yards`[i]=sum(stats$receiving_yards)/7
  Team_Stats$`TE Receptions`[i]=sum(stats$receptions)/7
  Team_Stats$`TE TDs`[i]=(sum(stats$rushing_tds) + sum(stats$receiving_tds))/7
}
Props <- read.csv("Football Sims/nfl-player-props-overview.csv", skip = 1) %>% 
  select(Player, Team, Opp, `X.1`, `X.5`, `X.9`, `X.13`, `X.17`, `X.21`, `X.25`, `X.29`, `X.33`)
colnames(Props) <- c("Player", "Team", "Opp", "Comps", "Pass Att", "Pass Yds", "Passing TDs", "Ints", "Recs", "Rush + Rec Yds", "Rec Yds", "Rush Yds")
oldnames <- c("De'Von Achane", "DJ Chark", "DeMario Douglas", "JaMycal Hasty", "Scotty Miller", "DJ Moore", "Drew Ogletree", "Joshua Palmer", "DJ Turner", "KaVontae Turpin", "Jeff Wilson")
newnames <- c("Devon Achane", "D.J. Chark", "Demario Douglas", "Jamycal Hasty", "Scott Miller", "D.J. Moore", "Andrew Ogletree", "Josh Palmer", "D.J. Turner", "Kavontae Turpin", "Jeffery Wilson")

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
    Players$DivGame[i]=stats$div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
  } else if (stats$location=="Neutral" & stats$home_team==Players$Team[i]) {
    Players$Home_Away[i]="Neutral"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
  } else if (stats$location!="Neutral" & stats$away_team==Players$Team[i]) {
    Players$Home_Away[i]="Away"
    Players$Opponent[i]=stats$home_team
    Players$DivGame[i]=stats$div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
  } else {
    Players$Home_Away[i]="Home"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$div_game
    Players$TimeOfGame[i]=stats$time_of_game
    Players$Surface[i]=stats$surface
  }
  stats <- filter(weekly_boxscores, player_display_name==Players$Player[i])
  if (nrow(stats)>0) {
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
  stats <- filter(Team_Stats, Team==Players$Opponent[i])
  Players[i,22:40]=stats[3:21]
}
for (i in 1:nrow(Players)) {
  if (!is.na(Players$Comps[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Last 7 Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`vOpp Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`vDivisonal Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Month Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Weekday Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Surface Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Comps`[i]=sum(stats$completions*(Players$`QB Passing Completions`[i]/mean(Team_Stats$`QB Passing Completions`))>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Time of Game Comps`[i]=NA
    }
  } else {
    Players$`Last 7 Comps`[i]=NA
    Players$`Last 7 Home/Away Comps`[i]=NA
    Players$`vOpp Comps`[i]=NA
    Players$`vDivisonal Comps`[i]=NA
    Players$`Month Comps`[i]=NA
    Players$`Weekday Comps`[i]=NA
    Players$`Surface Comps`[i]=NA
    Players$`Time of Game Comps`[i]=NA
  }
  if (!is.na(Players$`Pass Att`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Last 7 Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]),7)
    if (nrow(stats)>2) {
      Players$`vOpp Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`vOpp Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Month Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Weekday Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Surface Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Pass Att`[i]=sum(stats$attempts*(Players$`QB Passing Attempts`[i]/mean(Team_Stats$`QB Passing Attempts`))>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Time of Game Pass Att`[i]=NA
    }
  } else {
    Players$`Last 7 Pass Att`[i]=NA
    Players$`Last 7 Home/Away Pass Att`[i]=NA
    Players$`vOpp Pass Att`[i]=NA
    Players$`vDivisonal Pass Att`[i]=NA
    Players$`Month Pass Att`[i]=NA
    Players$`Weekday Pass Att`[i]=NA
    Players$`Surface Pass Att`[i]=NA
    Players$`Time of Game Pass Att`[i]=NA
  }
  if (!is.na(Players$`Pass Yds`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Month Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Pass Yds`[i]=sum(stats$passing_yards*(Players$`QB Passing Yards`[i]/mean(Team_Stats$`QB Passing Yards`))>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Pass Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Pass Yds`[i]=NA
    Players$`Last 7 Home/Away Pass Yds`[i]=NA
    Players$`vOpp Pass Yds`[i]=NA
    Players$`vDivisonal Pass Yds`[i]=NA
    Players$`Month Pass Yds`[i]=NA
    Players$`Weekday Pass Yds`[i]=NA
    Players$`Surface Pass Yds`[i]=NA
    Players$`Time of Game Pass Yds`[i]=NA
  }
  if (!is.na(Players$Recs[i]) & Players$Position[i]=="RB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vOpp Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vDivisonal Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Month Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Weekday Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Surface Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Recs`[i]=sum(stats$receptions*(Players$`RB Receptions`[i]/mean(Team_Stats$`RB Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Time of Game Recs`[i]=NA
    }
  } else if (!is.na(Players$Recs[i]) & Players$Position[i]=="TE") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vOpp Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vDivisonal Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Month Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Weekday Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Surface Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Recs`[i]=sum(stats$receptions*(Players$`TE Receptions`[i]/mean(Team_Stats$`TE Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Time of Game Recs`[i]=NA
    }
  } else if (!is.na(Players$Recs[i]) & Players$Position[i]=="WR") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vOpp Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vDivisonal Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Month Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Weekday Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Surface Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Recs`[i]=sum(stats$receptions*(Players$`WR Receptions`[i]/mean(Team_Stats$`WR Receptions`))>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Time of Game Recs`[i]=NA
    }
  } else {
    Players$`Last 7 Recs`[i]=NA
    Players$`Last 7 Home/Away Recs`[i]=NA
    Players$`vOpp Recs`[i]=NA
    Players$`vDivisonal Recs`[i]=NA
    Players$`Month Recs`[i]=NA
    Players$`Weekday Recs`[i]=NA
    Players$`Surface Recs`[i]=NA
    Players$`Time of Game Recs`[i]=NA
  }
  if (!is.na(Players$`Rush + Rec Yds`[i]) & Players$Position[i]=="RB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`RB Rushing Yards`[i] + Players$`RB Receiving Yards`[i])/mean(Team_Stats$`RB Rushing Yards` + Team_Stats$`RB Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rush + Rec Yds`[i]=NA
    }
  } else if (!is.na(Players$`Rush + Rec Yds`[i]) & Players$Position[i]=="WR") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rush + Rec Yds`[i]=sum((stats$rushing_yards + stats$receiving_yards)*((Players$`WR Rushing Yards`[i] + Players$`WR Receiving Yards`[i])/mean(Team_Stats$`WR Rushing Yards` + Team_Stats$`WR Receiving Yards`))>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rush + Rec Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rush + Rec Yds`[i]=NA
    Players$`Last 7 Home/Away Rush + Rec Yds`[i]=NA
    Players$`vOpp Rush + Rec Yds`[i]=NA
    Players$`vDivisonal Rush + Rec Yds`[i]=NA
    Players$`Month Rush + Rec Yds`[i]=NA
    Players$`Weekday Rush + Rec Yds`[i]=NA
    Players$`Surface Rush + Rec Yds`[i]=NA
    Players$`Time of Game Rush + Rec Yds`[i]=NA
  }
  if (!is.na(Players$`Rec Yds`[i]) & Players$Position[i]=="RB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rec Yds`[i]=sum(stats$receiving_yards*(Players$`RB Receiving Yards`[i]/mean(Team_Stats$`RB Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rec Yds`[i]=NA
    }
  } else if (!is.na(Players$`Rec Yds`[i]) & Players$Position[i]=="WR") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rec Yds`[i]=sum(stats$receiving_yards*(Players$`WR Receiving Yards`[i]/mean(Team_Stats$`WR Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rec Yds`[i]=NA
    }
  } else if (!is.na(Players$`Rec Yds`[i]) & Players$Position[i]=="TE") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rec Yds`[i]=sum(stats$receiving_yards*(Players$`TE Receiving Yards`[i]/mean(Team_Stats$`TE Receiving Yards`))>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rec Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rec Yds`[i]=NA
    Players$`Last 7 Home/Away Rec Yds`[i]=NA
    Players$`vOpp Rec Yds`[i]=NA
    Players$`vDivisonal Rec Yds`[i]=NA
    Players$`Month Rec Yds`[i]=NA
    Players$`Weekday Rec Yds`[i]=NA
    Players$`Surface Rec Yds`[i]=NA
    Players$`Time of Game Rec Yds`[i]=NA
  }
  if (!is.na(Players$`Rush Yds`[i]) & Players$Position[i]=="RB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rush Yds`[i]=sum(stats$rushing_yards*(Players$`RB Rushing Yards`[i]/mean(Team_Stats$`RB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rush Yds`[i]=NA
    }
  } else if (!is.na(Players$`Rush Yds`[i]) & Players$Position[i]=="WR") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rush Yds`[i]=sum(stats$rushing_yards*(Players$`WR Rushing Yards`[i]/mean(Team_Stats$`WR Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rush Yds`[i]=NA
    }
  } else if (!is.na(Players$`Rush Yds`[i]) & Players$Position[i]=="QB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Surface Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Rush Yds`[i]=sum(stats$rushing_yards*(Players$`QB Rushing Yards`[i]/mean(Team_Stats$`QB Rushing Yards`))>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Time of Game Rush Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rush Yds`[i]=NA
    Players$`Last 7 Home/Away Rush Yds`[i]=NA
    Players$`vOpp Rush Yds`[i]=NA
    Players$`vDivisonal Rush Yds`[i]=NA
    Players$`Month Rush Yds`[i]=NA
    Players$`Weekday Rush Yds`[i]=NA
    Players$`Surface Rush Yds`[i]=NA
    Players$`Time of Game Rush Yds`[i]=NA
  }
  if (Players$Position[i]=="QB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`vOpp TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`vDivisonal TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Month TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Weekday TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Surface TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`QB TDs`[i]/mean(Team_Stats$`QB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Time of Game TDs`[i]=NA
    }
  } else if (Players$Position[i]=="RB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`vOpp TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`vDivisonal TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Month TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Weekday TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Surface TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`RB TDs`[i]/mean(Team_Stats$`RB TDs`))>=1)/nrow(stats)
    } else {
      Players$`Time of Game TDs`[i]=NA
    }
  } else if (Players$Position[i]=="WR") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`vOpp TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`vDivisonal TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Month TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Weekday TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Surface TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`WR TDs`[i]/mean(Team_Stats$`WR TDs`))>=1)/nrow(stats)
    } else {
      Players$`Time of Game TDs`[i]=NA
    }
  } else if (Players$Position[i]=="TE") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`vOpp TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`vDivisonal TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Month TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Weekday TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Surface TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game TDs`[i]=sum((stats$rushing_tds + stats$receiving_tds)*(Players$`TE TDs`[i]/mean(Team_Stats$`TE TDs`))>=1)/nrow(stats)
    } else {
      Players$`Time of Game TDs`[i]=NA
    }
  }
  if (Players$Position[i]=="QB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`vOpp INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`vDivisonal INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Month INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Weekday INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Surface INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game INTs`[i]=sum(stats$interceptions*(Players$`QB Passing INTs`[i]/mean(Team_Stats$`QB Passing INTs`))>=1)/nrow(stats)
    } else {
      Players$`Time of Game INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Last 7 Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`vOpp Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`vDivisonal Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Month Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Weekday Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & surface==Players$Surface[i]), 7)
    if (nrow(stats)>2) {
      Players$`Surface Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Surface Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & time_of_game==Players$TimeOfGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`Time of Game Passing TDs`[i]=sum(stats$passing_tds*(Players$`QB Passing TDs`[i]/mean(Team_Stats$`QB Passing TDs`))>=2)/nrow(stats)
    } else {
      Players$`Time of Game Passing TDs`[i]=NA
    }
  } else {
    Players$`Last 7 INTs`[i]=NA
    Players$`Last 7 Home/Away INTs`[i]=NA
    Players$`vOpp INTs`[i]=NA
    Players$`vDivisonal INTs`[i]=NA
    Players$`Month INTs`[i]=NA
    Players$`Weekday INTs`[i]=NA
    Players$`Surface INTs`[i]=NA
    Players$`Time of Game INTs`[i]=NA
    Players$`Last 7 Passing TDs`[i]=NA
    Players$`Last 7 Home/Away Passing TDs`[i]=NA
    Players$`vOpp Passing TDs`[i]=NA
    Players$`vDivisonal Passing TDs`[i]=NA
    Players$`Month Passing TDs`[i]=NA
    Players$`Weekday Passing TDs`[i]=NA
    Players$`Surface Passing TDs`[i]=NA
    Players$`Time of Game Passing TDs`[i]=NA
  }
}
Bets <- data.frame()
stats <- filter(Players, Position=="QB" & !is.na(Comps)) 
stats <- stats %>% 
  mutate(Prop=paste(`Comps`, "Completions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Comps`, `Last 7 Home/Away Comps`, `vOpp Comps`, `vDivisonal Comps`, `Month Comps`, `Weekday Comps`, `Surface Comps`, `Time of Game Comps`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Pass Att`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Pass Att`, "Attempts")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Pass Att`, `Last 7 Home/Away Pass Att`, `vOpp Pass Att`, `vDivisonal Pass Att`, `Month Pass Att`, `Weekday Pass Att`, `Surface Pass Att`, `Time of Game Pass Att`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Pass Yds`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Pass Yds`, "Yards")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Pass Yds`, `Last 7 Home/Away Pass Yds`, `vOpp Pass Yds`, `vDivisonal Pass Yds`, `Month Pass Yds`, `Weekday Pass Yds`, `Surface Pass Yds`, `Time of Game Pass Yds`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Recs`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Recs`, "Receptions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Recs`, `Last 7 Home/Away Recs`, `vOpp Recs`, `vDivisonal Recs`, `Month Recs`, `Weekday Recs`, `Surface Recs`, `Time of Game Recs`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush + Rec Yds`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Rush + Rec Yds`, "Rush + Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush + Rec Yds`, `Last 7 Home/Away Rush + Rec Yds`, `vOpp Rush + Rec Yds`, `vDivisonal Rush + Rec Yds`, `Month Rush + Rec Yds`, `Weekday Rush + Rec Yds`, `Surface Rush + Rec Yds`, `Time of Game Rush + Rec Yds`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rec Yds`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Rec Yds`, "Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rec Yds`, `Last 7 Home/Away Rec Yds`, `vOpp Rec Yds`, `vDivisonal Rec Yds`, `Month Rec Yds`, `Weekday Rec Yds`, `Surface Rec Yds`, `Time of Game Rec Yds`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, !is.na(`Rush Yds`)) 
stats <- stats %>% 
  mutate(Prop=paste(`Rush Yds`, "Rush Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush Yds`, `Last 7 Home/Away Rush Yds`, `vOpp Rush Yds`, `vDivisonal Rush Yds`, `Month Rush Yds`, `Weekday Rush Yds`, `Surface Rush Yds`, `Time of Game Rush Yds`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB") 
stats <- stats %>% 
  mutate(Prop="1+ INT") %>% 
  select(Player, Team, Opp, Prop, `Last 7 INTs`, `Last 7 Home/Away INTs`, `vOpp INTs`, `vDivisonal INTs`, `Month INTs`, `Weekday INTs`, `Surface INTs`, `Time of Game INTs`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB") 
stats <- stats %>% 
  mutate(Prop="2+ Passing TDs") %>% 
  select(Player, Team, Opp, Prop, `Last 7 Passing TDs`, `Last 7 Home/Away Passing TDs`, `vOpp Passing TDs`, `vDivisonal Passing TDs`, `Month Passing TDs`, `Weekday Passing TDs`, `Surface Passing TDs`, `Time of Game Passing TDs`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
TD_Bets <- data.frame()
stats <- Players %>% 
  mutate(Prop=paste("1+", "TDs")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 TDs`, `Last 7 Home/Away TDs`, `vOpp TDs`, `vDivisonal TDs`, `Month TDs`, `Weekday TDs`, `Surface TDs`, `Time of Game TDs`)
for (i in 1:nrow(stats)) {
  stats$`Hit Rate`[i]=sum(stats[i, 5:12], na.rm = TRUE)/(sum(!is.na(stats[i, 5:12])))
}
stats <- stats %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
TD_Bets <- stats


Todays_Bets=Bets[Bets$Team %in% c(Todays_Games$away_team, Todays_Games$home_team), ]
Todays_TD_Bets=TD_Bets[TD_Bets$Team %in% c(Todays_Games$away_team, Todays_Games$home_team), ]
Todays_Bets <- Todays_Bets %>% 
  arrange(-`Hit Rate`)
Todays_TD_Bets <- Todays_TD_Bets %>% 
  arrange(-`Hit Rate`)
if (nrow(Todays_Games)==1) {
  Prop_Bets <- data.frame(unique(Todays_Bets$Player))
  colnames(Prop_Bets) <- "Player"
  for (i in 1:nrow(Prop_Bets)) {
    stats <- filter(Todays_Bets, Player==Prop_Bets$Player[i])
    Prop_Bets$Team[i]=stats$Team[1]
    Prop_Bets$Opp[i]=stats$Opp[1]
    Prop_Bets$Prop[i]=stats$Prop[1]
    Prop_Bets$`Hit Rate`[i]=stats$`Hit Rate`[1]
  }
  Bets_Today <- head(Prop_Bets, 3)
  TD_Bet <- head(Todays_TD_Bets, 1)
  Bets_Today <- rbind(Bets_Today, TD_Bet)
  view(Bets_Today)
} else if (nrow(Todays_Games)==2) {
  Prop_Bets <- data.frame(unique(Todays_Bets$Player))
  colnames(Prop_Bets) <- "Player"
  for (i in 1:nrow(Prop_Bets)) {
    stats <- filter(Todays_Bets, Player==Prop_Bets$Player[i])
    Prop_Bets$Team[i]=stats$Team[1]
    Prop_Bets$Opp[i]=stats$Opp[1]
    Prop_Bets$Prop[i]=stats$Prop[1]
    Prop_Bets$`Hit Rate`[i]=stats$`Hit Rate`[1]
  }
  stats=Prop_Bets[Prop_Bets$Team %in% c(Todays_Games$away_team[1], Todays_Games$home_team[1]), ]
  Bets_Today <- head(stats, 3)
  stats=Prop_Bets[Prop_Bets$Team %in% c(Todays_Games$away_team[2], Todays_Games$home_team[2]), ]
  stats <- head(stats, 3)
  Bets_Today <- rbind(Bets_Today, stats)
  view(Bets_Today)
} else if (nrow(Todays_Games)==3) {
  Prop_Bets <- data.frame(unique(Todays_Bets$Player))
  colnames(Prop_Bets) <- "Player"
  for (i in 1:nrow(Prop_Bets)) {
    stats <- filter(Todays_Bets, Player==Prop_Bets$Player[i])
    Prop_Bets$Team[i]=stats$Team[1]
    Prop_Bets$Opp[i]=stats$Opp[1]
    Prop_Bets$Prop[i]=stats$Prop[1]
    Prop_Bets$`Hit Rate`[i]=stats$`Hit Rate`[1]
  }
  stats=Prop_Bets[Prop_Bets$Team %in% c(Todays_Games$away_team[1], Todays_Games$home_team[1]), ]
  Bets_Today <- head(stats, 2)
  stats=Prop_Bets[Prop_Bets$Team %in% c(Todays_Games$away_team[2], Todays_Games$home_team[2]), ]
  stats <- head(stats, 2)
  Bets_Today <- rbind(Bets_Today, stats)
  stats=Prop_Bets[Prop_Bets$Team %in% c(Todays_Games$away_team[3], Todays_Games$home_team[3]), ]
  stats <- head(stats, 2)
  Bets_Today <- rbind(Bets_Today, stats)
  view(Bets_Today)
  stats=Todays_TD_Bets[Todays_TD_Bets$Team %in% c(Todays_Games$away_team[1], Todays_Games$home_team[1]), ]
  TD_Bets_Today <- head(stats, 1)
  stats=Todays_TD_Bets[Todays_TD_Bets$Team %in% c(Todays_Games$away_team[2], Todays_Games$home_team[2]), ]
  stats <- head(stats, 1)
  TD_Bets_Today <- rbind(Bets_Today, stats)
  stats=Todays_TD_Bets[Todays_TD_Bets$Team %in% c(Todays_Games$away_team[3], Todays_Games$home_team[3]), ]
  stats <- head(stats, 1)
  TD_Bets_Today <- rbind(Bets_Today, stats)
  view(TD_Bets_Today)
} else {
  view(Todays_Bets)
  view(Todays_TD_Bets)
}


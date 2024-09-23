library(tidyverse)
library(nflreadr)

weekly_boxscores <- load_player_stats(c(get_current_season()-13, get_current_season()-12, get_current_season()-11, get_current_season()-10, get_current_season()-9, get_current_season()-8, get_current_season()-7, get_current_season()-6, get_current_season()-5, get_current_season()-4, get_current_season()-3, get_current_season()-2, get_current_season()-1, get_current_season()), stat_type = "offense") %>% 
  arrange(-season, -week)
schedule <- load_schedules(c(year(today()-60)-14, year(today()-60)-13, year(today()-60)-12, year(today()-60)-11, year(today()-60)-10, year(today()-60)-9, year(today()-60)-8, year(today()-60)-7, year(today()-60)-6, year(today()-60)-5, year(today()-60)-4, year(today()-60)-3, year(today()-60)-2, year(today()-60)-1, year(today()-60)))
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
  } else if (stats$location!="Neutral" & stats$home_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Home"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$div_game
  } else if (stats$location!="Neutral" & stats$away_team==weekly_boxscores$recent_team[i]) {
    weekly_boxscores$Home_Away[i]="Away"
    weekly_boxscores$game_id[i]=stats$game_id
    weekly_boxscores$game_date[i]=stats$gameday
    weekly_boxscores$divisional_game[i]=stats$div_game
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
  } else if (stats$location=="Neutral" & stats$home_team==Players$Team[i]) {
    Players$Home_Away[i]="Neutral"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$div_game
  } else if (stats$location!="Neutral" & stats$away_team==Players$Team[i]) {
    Players$Home_Away[i]="Away"
    Players$Opponent[i]=stats$home_team
    Players$DivGame[i]=stats$div_game
  } else {
    Players$Home_Away[i]="Home"
    Players$Opponent[i]=stats$away_team
    Players$DivGame[i]=stats$div_game
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
}
for (i in 1:nrow(Players)) {
  if (!is.na(Players$Comps[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Last 7 Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`vOpp Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`vDivisonal Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Month Comps`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Comps`[i]=sum(stats$completions>Players$Comps[i])/nrow(stats)
    } else {
      Players$`Weekday Comps`[i]=NA
    }
  } else {
    Players$`Last 7 Comps`[i]=NA
    Players$`Last 7 Home/Away Comps`[i]=NA
    Players$`vOpp Comps`[i]=NA
    Players$`vDivisonal Comps`[i]=NA
    Players$`Month Comps`[i]=NA
    Players$`Weekday Comps`[i]=NA
  }
  if (!is.na(Players$`Pass Att`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Last 7 Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]),7)
    if (nrow(stats)>2) {
      Players$`vOpp Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`vOpp Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Month Pass Att`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Pass Att`[i]=sum(stats$attempts>Players$`Pass Att`[i])/nrow(stats)
    } else {
      Players$`Weekday Pass Att`[i]=NA
    }
  } else {
    Players$`Last 7 Pass Att`[i]=NA
    Players$`Last 7 Home/Away Pass Att`[i]=NA
    Players$`vOpp Pass Att`[i]=NA
    Players$`vDivisonal Pass Att`[i]=NA
    Players$`Month Pass Att`[i]=NA
    Players$`Weekday Pass Att`[i]=NA
  }
  if (!is.na(Players$`Pass Yds`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Month Pass Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Pass Yds`[i]=sum(stats$passing_yards>Players$`Pass Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Pass Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Pass Yds`[i]=NA
    Players$`Last 7 Home/Away Pass Yds`[i]=NA
    Players$`vOpp Pass Yds`[i]=NA
    Players$`vDivisonal Pass Yds`[i]=NA
    Players$`Month Pass Yds`[i]=NA
    Players$`Weekday Pass Yds`[i]=NA
  }
  if (!is.na(Players$Recs[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vOpp Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`vDivisonal Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Month Recs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Recs`[i]=sum(stats$receptions>Players$Recs[i])/nrow(stats)
    } else {
      Players$`Weekday Recs`[i]=NA
    }
  } else {
    Players$`Last 7 Recs`[i]=NA
    Players$`Last 7 Home/Away Recs`[i]=NA
    Players$`vOpp Recs`[i]=NA
    Players$`vDivisonal Recs`[i]=NA
    Players$`Month Recs`[i]=NA
    Players$`Weekday Recs`[i]=NA
  }
  if (!is.na(Players$`Rush + Rec Yds`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush + Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush + Rec Yds`[i]=sum(stats$rushing_yards + stats$receiving_yards>Players$`Rush + Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush + Rec Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rush + Rec Yds`[i]=NA
    Players$`Last 7 Home/Away Rush + Rec Yds`[i]=NA
    Players$`vOpp Rush + Rec Yds`[i]=NA
    Players$`vDivisonal Rush + Rec Yds`[i]=NA
    Players$`Month Rush + Rec Yds`[i]=NA
    Players$`Weekday Rush + Rec Yds`[i]=NA
  }
  if (!is.na(Players$`Rec Yds`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rec Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rec Yds`[i]=sum(stats$receiving_yards>Players$`Rec Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rec Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rec Yds`[i]=NA
    Players$`Last 7 Home/Away Rec Yds`[i]=NA
    Players$`vOpp Rec Yds`[i]=NA
    Players$`vDivisonal Rec Yds`[i]=NA
    Players$`Month Rec Yds`[i]=NA
    Players$`Weekday Rec Yds`[i]=NA
  }
  if (!is.na(Players$`Rush Yds`[i])) {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vOpp Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`vDivisonal Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Month Rush Yds`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Rush Yds`[i]=sum(stats$rushing_yards>Players$`Rush Yds`[i])/nrow(stats)
    } else {
      Players$`Weekday Rush Yds`[i]=NA
    }
  } else {
    Players$`Last 7 Rush Yds`[i]=NA
    Players$`Last 7 Home/Away Rush Yds`[i]=NA
    Players$`vOpp Rush Yds`[i]=NA
    Players$`vDivisonal Rush Yds`[i]=NA
    Players$`Month Rush Yds`[i]=NA
    Players$`Weekday Rush Yds`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
  if (nrow(stats)>2) {
    Players$`Last 7 TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`Last 7 TDs`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
  if (nrow(stats)>2) {
    Players$`Last 7 Home/Away TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`Last 7 Home/Away TDs`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
  if (nrow(stats)>2) {
    Players$`vOpp TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`vOpp TDs`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
  if (nrow(stats)>2) {
    Players$`vDivisonal TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`vDivisonal TDs`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
  if (nrow(stats)>2) {
    Players$`Month TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`Month TDs`[i]=NA
  }
  stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
  if (nrow(stats)>2) {
    Players$`Weekday TDs`[i]=sum(stats$rushing_tds + stats$receiving_tds>0)/nrow(stats)
  } else {
    Players$`Weekday TDs`[i]=NA
  }
  if (Players$Position[i]=="QB") {
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 INTs`[i]=sum(stats$interceptions>0)/nrow(stats)
    } else {
      Players$`Last 7 INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away INTs`[i]=sum(stats$interceptions>0)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp INTs`[i]=sum(stats$interceptions>0)/nrow(stats) 
    } else {
      Players$`vOpp INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal INTs`[i]=sum(stats$interceptions>0)/nrow(stats)
    } else {
      Players$`vDivisonal INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month INTs`[i]=sum(stats$interceptions>0)/nrow(stats)
    } else {
      Players$`Month INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday INTs`[i]=sum(stats$interceptions>0)/nrow(stats)
    } else {
      Players$`Weekday INTs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    } else {
      Players$`Last 7 Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Home_Away==Players$Home_Away[i]), 7)
    if (nrow(stats)>2) {
      Players$`Last 7 Home/Away Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    } else {
      Players$`Last 7 Home/Away Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & opponent_team==Players$Opponent[i]), 7)
    if (nrow(stats)>2) {
      Players$`vOpp Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    } else {
      Players$`vOpp Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & divisional_game==Players$DivGame[i]), 7)
    if (nrow(stats)>2) {
      Players$`vDivisonal Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    } else {
      Players$`vDivisonal Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Month==month(today())), 7)
    if (nrow(stats)>2) {
      Players$`Month Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats) 
    } else {
      Players$`Month Passing TDs`[i]=NA
    }
    stats <- head(filter(weekly_boxscores, player_id==Players$PlayerId[i] & Weekday==weekdays(today())), 7)
    if (nrow(stats)>2) {
      Players$`Weekday Passing TDs`[i]=sum(stats$passing_tds>1.5)/nrow(stats)
    } else {
      Players$`Weekday Passing TDs`[i]=NA
    }
  } else {
    Players$`Last 7 INTs`[i]=NA
    Players$`Last 7 Home/Away INTs`[i]=NA
    Players$`vOpp INTs`[i]=NA
    Players$`vDivisonal INTs`[i]=NA
    Players$`Month INTs`[i]=NA
    Players$`Weekday INTs`[i]=NA
    Players$`Last 7 Passing TDs`[i]=NA
    Players$`Last 7 Home/Away Passing TDs`[i]=NA
    Players$`vOpp Passing TDs`[i]=NA
    Players$`vDivisonal Passing TDs`[i]=NA
    Players$`Month Passing TDs`[i]=NA
    Players$`Weekday Passing TDs`[i]=NA
  }
  stats <- filter(Team_Stats, Team==Players$Opponent[i])
  Players[i,80:98]=stats[3:21]
}
Bets <- data.frame()
stats <- filter(Players, Position=="QB" & !is.na(Comps)) 
stats <- stats %>% 
  mutate(`Pass Comps+`=`QB Passing Completions`/mean(Team_Stats$`QB Passing Completions`),
         Prop=paste(`Comps`, "Completions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Comps`, `Last 7 Home/Away Comps`, `vOpp Comps`, `vDivisonal Comps`, `Month Comps`, `Weekday Comps`, `Pass Comps+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~`Last 7 Comps`*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.3*`Last 7 Comps` + .7*`Last 7 Home/Away Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Last 7 Home/Away Comps` + .5*`vOpp Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Last 7 Home/Away Comps` + .5*`vDivisonal Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Last 7 Home/Away Comps` + .5*`Month Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Last 7 Home/Away Comps` + .5*`Weekday Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .3*`vDivisonal Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .3*`Month Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .3*`Weekday Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .3*`Month Comps` + .4*`vDivisonal Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .3*`Weekday Comps` + .4*`vDivisonal Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Last 7 Home/Away Comps` + .4*`Month Comps` + .3*`Weekday Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .15*`Last 7 Home/Away Comps` + .25*`vDivisonal Comps` + .3*`vOpp Comps` + .2*`Month Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .15*`Last 7 Home/Away Comps` + .25*`vDivisonal Comps` + .3*`vOpp Comps` + .2*`Weekday Comps`)*`Pass Comps+`,
                              !is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .15*`Last 7 Home/Away Comps` + .3*`vDivisonal Comps` + .25*`Month Comps` + .2*`Weekday Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.3*`Last 7 Comps` + .7*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`vDivisonal Comps` + .5*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Month Comps` + .5*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Weekday Comps` + .5*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Month Comps` + .3*`vDivisonal Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .2*`Weekday Comps` + .3*`vDivisonal Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .3*`Month Comps` + .2*`Weekday Comps` + .4*`vOpp Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & !is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .3*`vOpp Comps` + .25*`vDivisonal Comps` + .2*`Month Comps` + .15*`Weekday Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.3*`Last 7 Comps` + .7*`vDivisonal Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .5*`vDivisonal Comps` + .333*`Month Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .5*`vDivisonal Comps` + .333*`Weekday Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & !is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.1*`Last 7 Comps` + .3*`Month Comps` + .2*`Weekday Comps` + .4*`vDivisonal Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & is.na(`Weekday Comps`)~(.3*`Last 7 Comps` + .7*`Month Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & !is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.167*`Last 7 Comps` + .333*`Weekday Comps` + .5*`Month Comps`)*`Pass Comps+`,
                              is.na(`Last 7 Home/Away Comps`) & is.na(`vOpp Comps`) & is.na(`vDivisonal Comps`) & is.na(`Month Comps`) & !is.na(`Weekday Comps`)~(.3*`Last 7 Comps` + .7*`Weekday Comps`)*`Pass Comps+`,
                              TRUE~(.066667*`Last 7 Comps` + .266667*`vOpp Comps` + .226667*`vDivisonal Comps` + .186667*`Month Comps` + .146667*`Weekday Comps` + .106667*`Last 7 Home/Away Comps`)*`Pass Comps+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB" & !is.na(`Pass Att`)) 
stats <- stats %>% 
  mutate(`Pass Atts+`=`QB Passing Attempts`/mean(Team_Stats$`QB Passing Attempts`),
         Prop=paste(`Pass Att`, "Attempts")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Pass Att`, `Last 7 Home/Away Pass Att`, `vOpp Pass Att`, `vDivisonal Pass Att`, `Month Pass Att`, `Weekday Pass Att`, `Pass Atts+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~`Last 7 Pass Att`*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.3*`Last 7 Pass Att` + .7*`Last 7 Home/Away Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Last 7 Home/Away Pass Att` + .5*`vOpp Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Last 7 Home/Away Pass Att` + .5*`vDivisonal Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Last 7 Home/Away Pass Att` + .5*`Month Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Last 7 Home/Away Pass Att` + .5*`Weekday Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .3*`vDivisonal Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .3*`Month Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .3*`Weekday Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .3*`Month Pass Att` + .4*`vDivisonal Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .3*`Weekday Pass Att` + .4*`vDivisonal Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Last 7 Home/Away Pass Att` + .4*`Month Pass Att` + .3*`Weekday Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .15*`Last 7 Home/Away Pass Att` + .25*`vDivisonal Pass Att` + .3*`vOpp Pass Att` + .2*`Month Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .15*`Last 7 Home/Away Pass Att` + .25*`vDivisonal Pass Att` + .3*`vOpp Pass Att` + .2*`Weekday Pass Att`)*`Pass Atts+`,
                              !is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .15*`Last 7 Home/Away Pass Att` + .3*`vDivisonal Pass Att` + .25*`Month Pass Att` + .2*`Weekday Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.3*`Last 7 Pass Att` + .7*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`vDivisonal Pass Att` + .5*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Month Pass Att` + .5*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Weekday Pass Att` + .5*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Month Pass Att` + .3*`vDivisonal Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .2*`Weekday Pass Att` + .3*`vDivisonal Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .3*`Month Pass Att` + .2*`Weekday Pass Att` + .4*`vOpp Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & !is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .3*`vOpp Pass Att` + .25*`vDivisonal Pass Att` + .2*`Month Pass Att` + .15*`Weekday Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.3*`Last 7 Pass Att` + .7*`vDivisonal Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .5*`vDivisonal Pass Att` + .333*`Month Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .5*`vDivisonal Pass Att` + .333*`Weekday Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & !is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.1*`Last 7 Pass Att` + .3*`Month Pass Att` + .2*`Weekday Pass Att` + .4*`vDivisonal Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & is.na(`Weekday Pass Att`)~(.3*`Last 7 Pass Att` + .7*`Month Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & !is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.167*`Last 7 Pass Att` + .333*`Weekday Pass Att` + .5*`Month Pass Att`)*`Pass Atts+`,
                              is.na(`Last 7 Home/Away Pass Att`) & is.na(`vOpp Pass Att`) & is.na(`vDivisonal Pass Att`) & is.na(`Month Pass Att`) & !is.na(`Weekday Pass Att`)~(.3*`Last 7 Pass Att` + .7*`Weekday Pass Att`)*`Pass Atts+`,
                              TRUE~(.066667*`Last 7 Pass Att` + .266667*`vOpp Pass Att` + .226667*`vDivisonal Pass Att` + .186667*`Month Pass Att` + .146667*`Weekday Pass Att` + .106667*`Last 7 Home/Away Pass Att`)*`Pass Atts+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB" & !is.na(`Pass Yds`)) 
stats <- stats %>% 
  mutate(`Passing Yds+`=`QB Passing Yards`/mean(Team_Stats$`QB Passing Yards`),
         Prop=paste(`Pass Yds`, "Yards")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Pass Yds`, `Last 7 Home/Away Pass Yds`, `vOpp Pass Yds`, `vDivisonal Pass Yds`, `Month Pass Yds`, `Weekday Pass Yds`, `Passing Yds+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~`Last 7 Pass Yds`*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.3*`Last 7 Pass Yds` + .7*`Last 7 Home/Away Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Last 7 Home/Away Pass Yds` + .5*`vOpp Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Last 7 Home/Away Pass Yds` + .5*`vDivisonal Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Last 7 Home/Away Pass Yds` + .5*`Month Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Last 7 Home/Away Pass Yds` + .5*`Weekday Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .3*`vDivisonal Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .3*`Month Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .3*`Weekday Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .3*`Month Pass Yds` + .4*`vDivisonal Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .3*`Weekday Pass Yds` + .4*`vDivisonal Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Last 7 Home/Away Pass Yds` + .4*`Month Pass Yds` + .3*`Weekday Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .15*`Last 7 Home/Away Pass Yds` + .25*`vDivisonal Pass Yds` + .3*`vOpp Pass Yds` + .2*`Month Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .15*`Last 7 Home/Away Pass Yds` + .25*`vDivisonal Pass Yds` + .3*`vOpp Pass Yds` + .2*`Weekday Pass Yds`)*`Passing Yds+`,
                              !is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .15*`Last 7 Home/Away Pass Yds` + .3*`vDivisonal Pass Yds` + .25*`Month Pass Yds` + .2*`Weekday Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.3*`Last 7 Pass Yds` + .7*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`vDivisonal Pass Yds` + .5*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Month Pass Yds` + .5*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Weekday Pass Yds` + .5*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Month Pass Yds` + .3*`vDivisonal Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .2*`Weekday Pass Yds` + .3*`vDivisonal Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .3*`Month Pass Yds` + .2*`Weekday Pass Yds` + .4*`vOpp Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & !is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .3*`vOpp Pass Yds` + .25*`vDivisonal Pass Yds` + .2*`Month Pass Yds` + .15*`Weekday Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.3*`Last 7 Pass Yds` + .7*`vDivisonal Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .5*`vDivisonal Pass Yds` + .333*`Month Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .5*`vDivisonal Pass Yds` + .333*`Weekday Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & !is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.1*`Last 7 Pass Yds` + .3*`Month Pass Yds` + .2*`Weekday Pass Yds` + .4*`vDivisonal Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & is.na(`Weekday Pass Yds`)~(.3*`Last 7 Pass Yds` + .7*`Month Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & !is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.167*`Last 7 Pass Yds` + .333*`Weekday Pass Yds` + .5*`Month Pass Yds`)*`Passing Yds+`,
                              is.na(`Last 7 Home/Away Pass Yds`) & is.na(`vOpp Pass Yds`) & is.na(`vDivisonal Pass Yds`) & is.na(`Month Pass Yds`) & !is.na(`Weekday Pass Yds`)~(.3*`Last 7 Pass Yds` + .7*`Weekday Pass Yds`)*`Passing Yds+`,
                              TRUE~(.066667*`Last 7 Pass Yds` + .266667*`vOpp Pass Yds` + .226667*`vDivisonal Pass Yds` + .186667*`Month Pass Yds` + .146667*`Weekday Pass Yds` + .106667*`Last 7 Home/Away Pass Yds`)*`Passing Yds+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="RB" & !is.na(`Recs`)) 
stats <- stats %>% 
  mutate(`Rec+`=`RB Receptions`/mean(Team_Stats$`RB Receptions`),
         Prop=paste(`Recs`, "Receptions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Recs`, `Last 7 Home/Away Recs`, `vOpp Recs`, `vDivisonal Recs`, `Month Recs`, `Weekday Recs`, `Rec+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~`Last 7 Recs`*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Last 7 Home/Away Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .4*`Month Recs` + .3*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .25*`Month Recs` + .2*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`vDivisonal Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Month Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Month Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Weekday Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`vOpp Recs` + .25*`vDivisonal Recs` + .2*`Month Recs` + .15*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Weekday Recs`)*`Rec+`,
                              TRUE~(.066667*`Last 7 Recs` + .266667*`vOpp Recs` + .226667*`vDivisonal Recs` + .186667*`Month Recs` + .146667*`Weekday Recs` + .106667*`Last 7 Home/Away Recs`)*`Rec+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="WR" & !is.na(`Recs`)) 
stats <- stats %>% 
  mutate(`Rec+`=`WR Receptions`/mean(Team_Stats$`WR Receptions`),
         Prop=paste(`Recs`, "Receptions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Recs`, `Last 7 Home/Away Recs`, `vOpp Recs`, `vDivisonal Recs`, `Month Recs`, `Weekday Recs`, `Rec+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~`Last 7 Recs`*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Last 7 Home/Away Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .4*`Month Recs` + .3*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .25*`Month Recs` + .2*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`vDivisonal Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Month Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Month Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Weekday Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`vOpp Recs` + .25*`vDivisonal Recs` + .2*`Month Recs` + .15*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Weekday Recs`)*`Rec+`,
                              TRUE~(.066667*`Last 7 Recs` + .266667*`vOpp Recs` + .226667*`vDivisonal Recs` + .186667*`Month Recs` + .146667*`Weekday Recs` + .106667*`Last 7 Home/Away Recs`)*`Rec+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="TE" & !is.na(`Recs`)) 
stats <- stats %>% 
  mutate(`Rec+`=`TE Receptions`/mean(Team_Stats$`TE Receptions`),
         Prop=paste(`Recs`, "Receptions")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Recs`, `Last 7 Home/Away Recs`, `vOpp Recs`, `vDivisonal Recs`, `Month Recs`, `Weekday Recs`, `Rec+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~`Last 7 Recs`*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Last 7 Home/Away Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Last 7 Home/Away Recs` + .5*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Month Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .3*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Last 7 Home/Away Recs` + .4*`Month Recs` + .3*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Month Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .25*`vDivisonal Recs` + .3*`vOpp Recs` + .2*`Weekday Recs`)*`Rec+`,
                              !is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .15*`Last 7 Home/Away Recs` + .3*`vDivisonal Recs` + .25*`Month Recs` + .2*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`vDivisonal Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Month Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Month Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .2*`Weekday Recs` + .3*`vDivisonal Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vOpp Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & !is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`vOpp Recs` + .25*`vDivisonal Recs` + .2*`Month Recs` + .15*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .5*`vDivisonal Recs` + .333*`Weekday Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & !is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.1*`Last 7 Recs` + .3*`Month Recs` + .2*`Weekday Recs` + .4*`vDivisonal Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & !is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.167*`Last 7 Recs` + .333*`Weekday Recs` + .5*`Month Recs`)*`Rec+`,
                              is.na(`Last 7 Home/Away Recs`) & is.na(`vOpp Recs`) & is.na(`vDivisonal Recs`) & is.na(`Month Recs`) & !is.na(`Weekday Recs`)~(.3*`Last 7 Recs` + .7*`Weekday Recs`)*`Rec+`,
                              TRUE~(.066667*`Last 7 Recs` + .266667*`vOpp Recs` + .226667*`vDivisonal Recs` + .186667*`Month Recs` + .146667*`Weekday Recs` + .106667*`Last 7 Home/Away Recs`)*`Rec+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="RB" & !is.na(`Rush + Rec Yds`)) 
stats <- stats %>% 
  mutate(`Rush + Rec Yards+`=(`RB Rushing Yards` + `RB Receiving Yards`)/mean(Team_Stats$`RB Rushing Yards`+ Team_Stats$`RB Receiving Yards`),
         Prop=paste(`Rush + Rec Yds`, "Rush + Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush + Rec Yds`, `Last 7 Home/Away Rush + Rec Yds`, `vOpp Rush + Rec Yds`, `vDivisonal Rush + Rec Yds`, `Month Rush + Rec Yds`, `Weekday Rush + Rec Yds`, `Rush + Rec Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~`Last 7 Rush + Rec Yds`*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Last 7 Home/Away Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .4*`Month Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .2*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .25*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`vDivisonal Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Month Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Month Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .2*`Month Rush + Rec Yds` + .15*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds` + .333*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds` + .5*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              TRUE~(.066667*`Last 7 Rush + Rec Yds` + .266667*`vOpp Rush + Rec Yds` + .226667*`vDivisonal Rush + Rec Yds` + .186667*`Month Rush + Rec Yds` + .146667*`Weekday Rush + Rec Yds` + .106667*`Last 7 Home/Away Rush + Rec Yds`)*`Rush + Rec Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="WR" & !is.na(`Rush + Rec Yds`)) 
stats <- stats %>% 
  mutate(`Rush + Rec Yards+`=(`WR Rushing Yards` + `WR Receiving Yards`)/mean(Team_Stats$`WR Rushing Yards`+ Team_Stats$`WR Receiving Yards`),
         Prop=paste(`Rush + Rec Yds`, "Rush + Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush + Rec Yds`, `Last 7 Home/Away Rush + Rec Yds`, `vOpp Rush + Rec Yds`, `vDivisonal Rush + Rec Yds`, `Month Rush + Rec Yds`, `Weekday Rush + Rec Yds`, `Rush + Rec Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~`Last 7 Rush + Rec Yds`*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Last 7 Home/Away Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Last 7 Home/Away Rush + Rec Yds` + .5*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Last 7 Home/Away Rush + Rec Yds` + .4*`Month Rush + Rec Yds` + .3*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .2*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .15*`Last 7 Home/Away Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .25*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`vDivisonal Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Month Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds` + .5*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Month Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .3*`vDivisonal Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .4*`vOpp Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & !is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`vOpp Rush + Rec Yds` + .25*`vDivisonal Rush + Rec Yds` + .2*`Month Rush + Rec Yds` + .15*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds` + .333*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .5*`vDivisonal Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & !is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.1*`Last 7 Rush + Rec Yds` + .3*`Month Rush + Rec Yds` + .2*`Weekday Rush + Rec Yds` + .4*`vDivisonal Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & !is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.167*`Last 7 Rush + Rec Yds` + .333*`Weekday Rush + Rec Yds` + .5*`Month Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              is.na(`Last 7 Home/Away Rush + Rec Yds`) & is.na(`vOpp Rush + Rec Yds`) & is.na(`vDivisonal Rush + Rec Yds`) & is.na(`Month Rush + Rec Yds`) & !is.na(`Weekday Rush + Rec Yds`)~(.3*`Last 7 Rush + Rec Yds` + .7*`Weekday Rush + Rec Yds`)*`Rush + Rec Yards+`,
                              TRUE~(.066667*`Last 7 Rush + Rec Yds` + .266667*`vOpp Rush + Rec Yds` + .226667*`vDivisonal Rush + Rec Yds` + .186667*`Month Rush + Rec Yds` + .146667*`Weekday Rush + Rec Yds` + .106667*`Last 7 Home/Away Rush + Rec Yds`)*`Rush + Rec Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="RB" & !is.na(`Rec Yds`)) 
stats <- stats %>% 
  mutate(`Rec Yards+`=`RB Receiving Yards`/mean(Team_Stats$`RB Receiving Yards`),
         Prop=paste(`Rec Yds`, "Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rec Yds`, `Last 7 Home/Away Rec Yds`, `vOpp Rec Yds`, `vDivisonal Rec Yds`, `Month Rec Yds`, `Weekday Rec Yds`, `Rec Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~`Last 7 Rec Yds`*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .4*`Month Rec Yds` + .3*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .25*`Month Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`vDivisonal Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Month Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Month Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Weekday Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`vOpp Rec Yds` + .25*`vDivisonal Rec Yds` + .2*`Month Rec Yds` + .15*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Weekday Rec Yds`)*`Rec Yards+`,
                              TRUE~(.066667*`Last 7 Rec Yds` + .266667*`vOpp Rec Yds` + .226667*`vDivisonal Rec Yds` + .186667*`Month Rec Yds` + .146667*`Weekday Rec Yds` + .106667*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="WR" & !is.na(`Rec Yds`)) 
stats <- stats %>% 
  mutate(`Rec Yards+`=`WR Receiving Yards`/mean(Team_Stats$`WR Receiving Yards`),
         Prop=paste(`Rec Yds`, "Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rec Yds`, `Last 7 Home/Away Rec Yds`, `vOpp Rec Yds`, `vDivisonal Rec Yds`, `Month Rec Yds`, `Weekday Rec Yds`, `Rec Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~`Last 7 Rec Yds`*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .4*`Month Rec Yds` + .3*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .25*`Month Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`vDivisonal Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Month Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Month Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Weekday Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`vOpp Rec Yds` + .25*`vDivisonal Rec Yds` + .2*`Month Rec Yds` + .15*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Weekday Rec Yds`)*`Rec Yards+`,
                              TRUE~(.066667*`Last 7 Rec Yds` + .266667*`vOpp Rec Yds` + .226667*`vDivisonal Rec Yds` + .186667*`Month Rec Yds` + .146667*`Weekday Rec Yds` + .106667*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="TE" & !is.na(`Rec Yds`)) 
stats <- stats %>% 
  mutate(`Rec Yards+`=`TE Receiving Yards`/mean(Team_Stats$`TE Receiving Yards`),
         Prop=paste(`Rec Yds`, "Rec Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rec Yds`, `Last 7 Home/Away Rec Yds`, `vOpp Rec Yds`, `vDivisonal Rec Yds`, `Month Rec Yds`, `Weekday Rec Yds`, `Rec Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~`Last 7 Rec Yds`*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Last 7 Home/Away Rec Yds` + .5*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Month Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .3*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Last 7 Home/Away Rec Yds` + .4*`Month Rec Yds` + .3*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Month Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .25*`vDivisonal Rec Yds` + .3*`vOpp Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              !is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .15*`Last 7 Home/Away Rec Yds` + .3*`vDivisonal Rec Yds` + .25*`Month Rec Yds` + .2*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`vDivisonal Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Month Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Month Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .2*`Weekday Rec Yds` + .3*`vDivisonal Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vOpp Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & !is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`vOpp Rec Yds` + .25*`vDivisonal Rec Yds` + .2*`Month Rec Yds` + .15*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .5*`vDivisonal Rec Yds` + .333*`Weekday Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & !is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.1*`Last 7 Rec Yds` + .3*`Month Rec Yds` + .2*`Weekday Rec Yds` + .4*`vDivisonal Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & !is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.167*`Last 7 Rec Yds` + .333*`Weekday Rec Yds` + .5*`Month Rec Yds`)*`Rec Yards+`,
                              is.na(`Last 7 Home/Away Rec Yds`) & is.na(`vOpp Rec Yds`) & is.na(`vDivisonal Rec Yds`) & is.na(`Month Rec Yds`) & !is.na(`Weekday Rec Yds`)~(.3*`Last 7 Rec Yds` + .7*`Weekday Rec Yds`)*`Rec Yards+`,
                              TRUE~(.066667*`Last 7 Rec Yds` + .266667*`vOpp Rec Yds` + .226667*`vDivisonal Rec Yds` + .186667*`Month Rec Yds` + .146667*`Weekday Rec Yds` + .106667*`Last 7 Home/Away Rec Yds`)*`Rec Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="RB" & !is.na(`Rush Yds`)) 
stats <- stats %>% 
  mutate(`Rush Yards+`=`RB Rushing Yards`/mean(Team_Stats$`RB Rushing Yards`),
         Prop=paste(`Rush Yds`, "Rush Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush Yds`, `Last 7 Home/Away Rush Yds`, `vOpp Rush Yds`, `vDivisonal Rush Yds`, `Month Rush Yds`, `Weekday Rush Yds`, `Rush Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~`Last 7 Rush Yds`*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .4*`Month Rush Yds` + .3*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .25*`Month Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`vDivisonal Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Month Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Month Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Weekday Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`vOpp Rush Yds` + .25*`vDivisonal Rush Yds` + .2*`Month Rush Yds` + .15*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Weekday Rush Yds`)*`Rush Yards+`,
                              TRUE~(.066667*`Last 7 Rush Yds` + .266667*`vOpp Rush Yds` + .226667*`vDivisonal Rush Yds` + .186667*`Month Rush Yds` + .146667*`Weekday Rush Yds` + .106667*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB" & !is.na(`Rush Yds`)) 
stats <- stats %>% 
  mutate(`Rush Yards+`=`QB Rushing Yards`/mean(Team_Stats$`QB Rushing Yards`),
         Prop=paste(`Rush Yds`, "Rush Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush Yds`, `Last 7 Home/Away Rush Yds`, `vOpp Rush Yds`, `vDivisonal Rush Yds`, `Month Rush Yds`, `Weekday Rush Yds`, `Rush Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~`Last 7 Rush Yds`*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .4*`Month Rush Yds` + .3*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .25*`Month Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`vDivisonal Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Month Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Month Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Weekday Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`vOpp Rush Yds` + .25*`vDivisonal Rush Yds` + .2*`Month Rush Yds` + .15*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Weekday Rush Yds`)*`Rush Yards+`,
                              TRUE~(.066667*`Last 7 Rush Yds` + .266667*`vOpp Rush Yds` + .226667*`vDivisonal Rush Yds` + .186667*`Month Rush Yds` + .146667*`Weekday Rush Yds` + .106667*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="WR" & !is.na(`Rush Yds`)) 
stats <- stats %>% 
  mutate(`Rush Yards+`=`WR Rushing Yards`/mean(Team_Stats$`WR Rushing Yards`),
         Prop=paste(`Rush Yds`, "Rush Yds")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 Rush Yds`, `Last 7 Home/Away Rush Yds`, `vOpp Rush Yds`, `vDivisonal Rush Yds`, `Month Rush Yds`, `Weekday Rush Yds`, `Rush Yards+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~`Last 7 Rush Yds`*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Last 7 Home/Away Rush Yds` + .5*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Month Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .3*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Last 7 Home/Away Rush Yds` + .4*`Month Rush Yds` + .3*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Month Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .25*`vDivisonal Rush Yds` + .3*`vOpp Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              !is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .15*`Last 7 Home/Away Rush Yds` + .3*`vDivisonal Rush Yds` + .25*`Month Rush Yds` + .2*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`vDivisonal Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Month Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Month Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .2*`Weekday Rush Yds` + .3*`vDivisonal Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vOpp Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & !is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`vOpp Rush Yds` + .25*`vDivisonal Rush Yds` + .2*`Month Rush Yds` + .15*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .5*`vDivisonal Rush Yds` + .333*`Weekday Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & !is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.1*`Last 7 Rush Yds` + .3*`Month Rush Yds` + .2*`Weekday Rush Yds` + .4*`vDivisonal Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & !is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.167*`Last 7 Rush Yds` + .333*`Weekday Rush Yds` + .5*`Month Rush Yds`)*`Rush Yards+`,
                              is.na(`Last 7 Home/Away Rush Yds`) & is.na(`vOpp Rush Yds`) & is.na(`vDivisonal Rush Yds`) & is.na(`Month Rush Yds`) & !is.na(`Weekday Rush Yds`)~(.3*`Last 7 Rush Yds` + .7*`Weekday Rush Yds`)*`Rush Yards+`,
                              TRUE~(.066667*`Last 7 Rush Yds` + .266667*`vOpp Rush Yds` + .226667*`vDivisonal Rush Yds` + .186667*`Month Rush Yds` + .146667*`Weekday Rush Yds` + .106667*`Last 7 Home/Away Rush Yds`)*`Rush Yards+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB") 
stats <- stats %>% 
  mutate(`INTs+`=`QB Passing INTs`/mean(Team_Stats$`QB Passing INTs`),
         Prop="1+ INT") %>% 
  select(Player, Team, Opp, Prop, `Last 7 INTs`, `Last 7 Home/Away INTs`, `vOpp INTs`, `vDivisonal INTs`, `Month INTs`, `Weekday INTs`, `INTs+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~`Last 7 INTs`*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.3*`Last 7 INTs` + .7*`Last 7 Home/Away INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Last 7 Home/Away INTs` + .5*`vOpp INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Last 7 Home/Away INTs` + .5*`vDivisonal INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Last 7 Home/Away INTs` + .5*`Month INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Last 7 Home/Away INTs` + .5*`Weekday INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .3*`vDivisonal INTs` + .4*`vOpp INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .3*`Month INTs` + .4*`vOpp INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .3*`Weekday INTs` + .4*`vOpp INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .3*`Month INTs` + .4*`vDivisonal INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .3*`Weekday INTs` + .4*`vDivisonal INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Last 7 Home/Away INTs` + .4*`Month INTs` + .3*`Weekday INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .15*`Last 7 Home/Away INTs` + .25*`vDivisonal INTs` + .3*`vOpp INTs` + .2*`Month INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .15*`Last 7 Home/Away INTs` + .25*`vDivisonal INTs` + .3*`vOpp INTs` + .2*`Weekday INTs`)*`INTs+`,
                              !is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .15*`Last 7 Home/Away INTs` + .3*`vDivisonal INTs` + .25*`Month INTs` + .2*`Weekday INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.3*`Last 7 INTs` + .7*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`vDivisonal INTs` + .5*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Month INTs` + .5*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Weekday INTs` + .5*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Month INTs` + .3*`vDivisonal INTs` + .4*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .2*`Weekday INTs` + .3*`vDivisonal INTs` + .4*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .3*`Month INTs` + .2*`Weekday INTs` + .4*`vOpp INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & !is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .3*`vOpp INTs` + .25*`vDivisonal INTs` + .2*`Month INTs` + .15*`Weekday INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.3*`Last 7 INTs` + .7*`vDivisonal INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .5*`vDivisonal INTs` + .333*`Month INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .5*`vDivisonal INTs` + .333*`Weekday INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & !is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.1*`Last 7 INTs` + .3*`Month INTs` + .2*`Weekday INTs` + .4*`vDivisonal INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & is.na(`Weekday INTs`)~(.3*`Last 7 INTs` + .7*`Month INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & !is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.167*`Last 7 INTs` + .333*`Weekday INTs` + .5*`Month INTs`)*`INTs+`,
                              is.na(`Last 7 Home/Away INTs`) & is.na(`vOpp INTs`) & is.na(`vDivisonal INTs`) & is.na(`Month INTs`) & !is.na(`Weekday INTs`)~(.3*`Last 7 INTs` + .7*`Weekday INTs`)*`INTs+`,
                              TRUE~(.066667*`Last 7 INTs` + .266667*`vOpp INTs` + .226667*`vDivisonal INTs` + .186667*`Month INTs` + .146667*`Weekday INTs` + .106667*`Last 7 Home/Away INTs`)*`INTs+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
stats <- filter(Players, Position=="QB") 
stats <- stats %>% 
  mutate(`Passing TDs+`=`QB Passing TDs`/mean(Team_Stats$`QB Passing TDs`),
         Prop="2+ Passing TDs") %>% 
  select(Player, Team, Opp, Prop, `Last 7 Passing TDs`, `Last 7 Home/Away Passing TDs`, `vOpp Passing TDs`, `vDivisonal Passing TDs`, `Month Passing TDs`, `Weekday Passing TDs`, `Passing TDs+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~`Last 7 Passing TDs`*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.3*`Last 7 Passing TDs` + .7*`Last 7 Home/Away Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Last 7 Home/Away Passing TDs` + .5*`vOpp Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Last 7 Home/Away Passing TDs` + .5*`vDivisonal Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Last 7 Home/Away Passing TDs` + .5*`Month Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Last 7 Home/Away Passing TDs` + .5*`Weekday Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .3*`vDivisonal Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .3*`Month Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .3*`Weekday Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .3*`Month Passing TDs` + .4*`vDivisonal Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .3*`Weekday Passing TDs` + .4*`vDivisonal Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Last 7 Home/Away Passing TDs` + .4*`Month Passing TDs` + .3*`Weekday Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .15*`Last 7 Home/Away Passing TDs` + .25*`vDivisonal Passing TDs` + .3*`vOpp Passing TDs` + .2*`Month Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .15*`Last 7 Home/Away Passing TDs` + .25*`vDivisonal Passing TDs` + .3*`vOpp Passing TDs` + .2*`Weekday Passing TDs`)*`Passing TDs+`,
                              !is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .15*`Last 7 Home/Away Passing TDs` + .3*`vDivisonal Passing TDs` + .25*`Month Passing TDs` + .2*`Weekday Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.3*`Last 7 Passing TDs` + .7*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`vDivisonal Passing TDs` + .5*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Month Passing TDs` + .5*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Weekday Passing TDs` + .5*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Month Passing TDs` + .3*`vDivisonal Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .2*`Weekday Passing TDs` + .3*`vDivisonal Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .3*`Month Passing TDs` + .2*`Weekday Passing TDs` + .4*`vOpp Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & !is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .3*`vOpp Passing TDs` + .25*`vDivisonal Passing TDs` + .2*`Month Passing TDs` + .15*`Weekday Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.3*`Last 7 Passing TDs` + .7*`vDivisonal Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .5*`vDivisonal Passing TDs` + .333*`Month Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .5*`vDivisonal Passing TDs` + .333*`Weekday Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & !is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.1*`Last 7 Passing TDs` + .3*`Month Passing TDs` + .2*`Weekday Passing TDs` + .4*`vDivisonal Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & is.na(`Weekday Passing TDs`)~(.3*`Last 7 Passing TDs` + .7*`Month Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & !is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.167*`Last 7 Passing TDs` + .333*`Weekday Passing TDs` + .5*`Month Passing TDs`)*`Passing TDs+`,
                              is.na(`Last 7 Home/Away Passing TDs`) & is.na(`vOpp Passing TDs`) & is.na(`vDivisonal Passing TDs`) & is.na(`Month Passing TDs`) & !is.na(`Weekday Passing TDs`)~(.3*`Last 7 Passing TDs` + .7*`Weekday Passing TDs`)*`Passing TDs+`,
                              TRUE~(.066667*`Last 7 Passing TDs` + .266667*`vOpp Passing TDs` + .226667*`vDivisonal Passing TDs` + .186667*`Month Passing TDs` + .146667*`Weekday Passing TDs` + .106667*`Last 7 Home/Away Passing TDs`)*`Passing TDs+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
Bets <- rbind(Bets, stats)
TD_Bets <- data.frame()
stats <- filter(Players, Position=="QB") 
stats <- stats %>% 
  mutate(`TD+`=`QB TDs`/mean(Team_Stats$`QB TDs`),
         Prop=paste("1+", "TDs")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 TDs`, `Last 7 Home/Away TDs`, `vOpp TDs`, `vDivisonal TDs`, `Month TDs`, `Weekday TDs`, `TD+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~`Last 7 TDs`*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Last 7 Home/Away TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .4*`Month TDs` + .3*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .25*`Month TDs` + .2*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`vDivisonal TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Month TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Month TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Weekday TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`vOpp TDs` + .25*`vDivisonal TDs` + .2*`Month TDs` + .15*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Weekday TDs`)*`TD+`,
                              TRUE~(.066667*`Last 7 TDs` + .266667*`vOpp TDs` + .226667*`vDivisonal TDs` + .186667*`Month TDs` + .146667*`Weekday TDs` + .106667*`Last 7 Home/Away TDs`)*`TD+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="RB") 
stats <- stats %>% 
  mutate(`TD+`=`RB TDs`/mean(Team_Stats$`RB TDs`),
         Prop=paste("1+", "TDs")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 TDs`, `Last 7 Home/Away TDs`, `vOpp TDs`, `vDivisonal TDs`, `Month TDs`, `Weekday TDs`, `TD+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~`Last 7 TDs`*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Last 7 Home/Away TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .4*`Month TDs` + .3*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .25*`Month TDs` + .2*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`vDivisonal TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Month TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Month TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Weekday TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`vOpp TDs` + .25*`vDivisonal TDs` + .2*`Month TDs` + .15*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Weekday TDs`)*`TD+`,
                              TRUE~(.066667*`Last 7 TDs` + .266667*`vOpp TDs` + .226667*`vDivisonal TDs` + .186667*`Month TDs` + .146667*`Weekday TDs` + .106667*`Last 7 Home/Away TDs`)*`TD+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="WR") 
stats <- stats %>% 
  mutate(`TD+`=`WR TDs`/mean(Team_Stats$`WR TDs`),
         Prop=paste("1+", "TDs")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 TDs`, `Last 7 Home/Away TDs`, `vOpp TDs`, `vDivisonal TDs`, `Month TDs`, `Weekday TDs`, `TD+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~`Last 7 TDs`*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Last 7 Home/Away TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .4*`Month TDs` + .3*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .25*`Month TDs` + .2*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`vDivisonal TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Month TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Month TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Weekday TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`vOpp TDs` + .25*`vDivisonal TDs` + .2*`Month TDs` + .15*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Weekday TDs`)*`TD+`,
                              TRUE~(.066667*`Last 7 TDs` + .266667*`vOpp TDs` + .226667*`vDivisonal TDs` + .186667*`Month TDs` + .146667*`Weekday TDs` + .106667*`Last 7 Home/Away TDs`)*`TD+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
TD_Bets <- rbind(TD_Bets, stats)
stats <- filter(Players, Position=="TE") 
stats <- stats %>% 
  mutate(`TD+`=`TE TDs`/mean(Team_Stats$`TE TDs`),
         Prop=paste("1+", "TDs")) %>% 
  select(Player, Team, Opp, Prop, `Last 7 TDs`, `Last 7 Home/Away TDs`, `vOpp TDs`, `vDivisonal TDs`, `Month TDs`, `Weekday TDs`, `TD+`)
stats <- stats %>% 
  mutate(`Hit Rate`=case_when(is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~`Last 7 TDs`*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Last 7 Home/Away TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Last 7 Home/Away TDs` + .5*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Month TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .3*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Last 7 Home/Away TDs` + .4*`Month TDs` + .3*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Month TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .25*`vDivisonal TDs` + .3*`vOpp TDs` + .2*`Weekday TDs`)*`TD+`,
                              !is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .15*`Last 7 Home/Away TDs` + .3*`vDivisonal TDs` + .25*`Month TDs` + .2*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`vDivisonal TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Month TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Month TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .2*`Weekday TDs` + .3*`vDivisonal TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vOpp TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & !is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`vOpp TDs` + .25*`vDivisonal TDs` + .2*`Month TDs` + .15*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .5*`vDivisonal TDs` + .333*`Weekday TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & !is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.1*`Last 7 TDs` + .3*`Month TDs` + .2*`Weekday TDs` + .4*`vDivisonal TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & !is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.167*`Last 7 TDs` + .333*`Weekday TDs` + .5*`Month TDs`)*`TD+`,
                              is.na(`Last 7 Home/Away TDs`) & is.na(`vOpp TDs`) & is.na(`vDivisonal TDs`) & is.na(`Month TDs`) & !is.na(`Weekday TDs`)~(.3*`Last 7 TDs` + .7*`Weekday TDs`)*`TD+`,
                              TRUE~(.066667*`Last 7 TDs` + .266667*`vOpp TDs` + .226667*`vDivisonal TDs` + .186667*`Month TDs` + .146667*`Weekday TDs` + .106667*`Last 7 Home/Away TDs`)*`TD+`)) %>% 
  select(Player, Team, Opp, Prop, `Hit Rate`)
TD_Bets <- rbind(TD_Bets, stats)

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


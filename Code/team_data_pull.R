library(tidyverse)
library(httr)
library(data.table)



url_pull <- function(url_type, season_type = "Regular+Season"){
  if (url_type == "Boxscore") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=20&PlayerOrTeam=T&Season=2022-23&SeasonType=',
                 season_type,'&Sorter=DATE', sep ="")
  } else if (url_type == "Adv_Boxscore") {
    url <- paste('https://stats.gleague.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type, '&ShotClockRange=&VsConference=&VsDivision=', sep='')
  } else if (url_type == "FourFactor") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type,'&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=', sep='')
  } else if (url_type == "Shooting") {
    url <-paste('https://stats.gleague.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                season_type,'&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=', sep='')
  } else if (url_type == "Defense") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Defense&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type,'&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=',sep='')
  } else if (url_type == "Lineups") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameID=&GameSegment=&GroupQuantity=5&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type,'&ShotClockRange=&TeamID=0&VsConference=&VsDivision=', sep='')
  } else if (url_type == "TwoWays") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type,'&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=1&VsConference=&VsDivision=&Weight=',sep='')
  } else if (url_type == "Scoring") {
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashteamstatscombined?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type ,'&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=',sep='')
  } else if (url_type == "Adv_Lineups"){
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameID=&GameSegment=&GroupQuantity=5',
                 '&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=',
                 season_type,
                 '&ShotClockRange=&TeamID=0&VsConference=&VsDivision=', sep = '')
  } else{
    print("Error: Choose a different URL Type")
  }

}


getDatafromWebsite <- function(url_link){
  res <- httr::GET(url = url_link)
  data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
  column_names <- data$headers %>% as.character()
  dt <- rbindlist(data$rowSet) %>% setnames(column_names)

  return(dt)
}

getDatafromWebsiteShooting <- function(url_link){
  res <- httr::GET(url = url_link)
  data <- httr::content(res) %>% .[['resultSets']] %>% .[[3]]
  column_names <- httr::content(res) %>% .[['resultSets']] %>% .[[2]]

  header_names <- column_names[[1]][["columnNames"]] %>% as.character()
  column_names <- column_names[[2]][["columnNames"]] %>% as.character()
  dt <- data.frame(t(sapply(data,c)))
  #This may need to be updated because of the list of the header names may be in different order
  dt <- dt %>%
    rename(TEAM_ID = X1, TEAM_NAME = X2,
           RA_FGM = X3, RA_FGA = X4, RA_FG_PCT = X5,
           PAINT_FGM = X6, PAINT_FGA = X7, PAINT_FG_PCT = X8,
           MIDRANGE_FGM = X9, MIDRANGE_FGA = X10, MIDRANGE_FG_PCT = X11,
           LC3_FGM = X12, LC3_FGA = X13, LC3_FG_PCT = X14,
           RC3_FGM = X15, RC3_FGA = X16, RC3_FG_PCT = X17,
           ATB3_FGM = X18, ATB3_FGA = X19, ATB3_FG_PCT = X20,
           BACK_FGM = X21, BACK_FGA = X22, BACK_FG_PCT = X23,
           CORNER3_FGM = X24, CORNER3_FGA = X25, CORNER3_FG_PCT = X26)

  return(dt)
}


#Functions to clean the raw data and filter based on team names

cleanBoxscoreData <- function(dframe, team_abbr = ""){
  if (team_abbr == "") {
  tmp <- dframe %>%
    #filter(TEAM_ABBREVIATION == team_abbr) %>%
    group_by(GAME_DATE) %>%
    mutate(HomeAway = ifelse(grepl('@', MATCHUP, fixed = TRUE), 'Away', 'Home'),
           FG_PCT = round(FG_PCT*100, 2),
           FG3_PCT = round(FG3_PCT*100, 2),
           FT_PCT = round(FT_PCT*100, 2),
           ) %>%
    select(`GAME Date`=GAME_DATE, MATCHUP, HomeAway,WL,`FG%` = FG_PCT,`3P%` = FG3_PCT,`FT%` = FT_PCT, `+/-` = PLUS_MINUS)
  } else{
    tmp <- dframe %>%
      filter(TEAM_ABBREVIATION == team_abbr) %>%
      group_by(GAME_DATE) %>%
      mutate(HomeAway = ifelse(grepl('@', MATCHUP, fixed = TRUE), 'Away', 'Home'),
             FG_PCT = round(FG_PCT*100, 2),
             FG3_PCT = round(FG3_PCT*100, 2),
             FT_PCT = round(FT_PCT*100, 2),
      ) %>%
      select(`GAME Date`=GAME_DATE, MATCHUP, HomeAway,WL,`FG%` = FG_PCT,`3P%` = FG3_PCT,`FT%` = FT_PCT, `+/-` = PLUS_MINUS)
  }

  return(tmp)
}

cleanAdvBoxscoreData <- function(dframe, team_abbr){
  tmp <- dframe %>%
    filter(TEAM_ABBREVIATION == team_abbr) %>%
    group_by(GAME_DATE) %>%
    mutate(HomeAway = ifelse(grepl('@', MATCHUP, fixed = TRUE), 'Away', 'Home'),
           GAME_DATE = gsub(GAME_DATE,pattern="T00:00:00",replacement="",fixed=T)
    ) %>%
    select(`Game Date`=GAME_DATE,`Matchup`=MATCHUP,WL,`OER`=E_OFF_RATING,`DER`=E_DEF_RATING,`NetER`=E_NET_RATING,`Pace\40`=PACE_PER40,POSS) %>%
    arrange(desc(`Game Date`))

  return(head(tmp, 5))
}

cleanLineupData <- function(dframe, team_abbr){
  tmp <- dframe %>%
    filter(TEAM_ABBREVIATION == team_abbr) %>%
    #group_by(GROUP_ID) %>%
    mutate(FG_PCT = round(FG_PCT*100, 2),
           FG3_PCT = round(FG3_PCT*100, 2),
           FT_PCT = round(FT_PCT*100, 2),
           PPP = round(PTS/(FGA + (.44*FTA) + TOV), 2) ) %>%
    select(`Lineup`=GROUP_NAME, GP, MIN,`PPP`=PPP,`+/-`=PLUS_MINUS) %>%
    arrange(desc(MIN), desc(PPP), desc(`+/-`)) %>%
    filter(MIN >= 3)

  return(head(tmp, 5))
}

cleanTwoWayData <- function(dframe,team_abbr){
  filtered_dframe <- dframe %>% filter(TEAM_ABBREVIATION == team_abbr)

  if (nrow(filtered_dframe) < 1 ){
    tmp <- "No Two-Way Players on Roster"
    tmp <- as.data.frame(tmp) %>% rename(Player = tmp)
  } else{
  tmp <- dframe %>%
    filter(TEAM_ABBREVIATION == team_abbr) %>%
    mutate(FG_PCT = as.numeric(FG_PCT),
           FG3_PCT = as.numeric(FG3_PCT),
           FT_PCT = as.numeric(FT_PCT),
    ) %>%
    select(`Player Name` = PLAYER_NAME,GP,MIN,PTS,`FG%` = FG_PCT,`3P%` = FG3_PCT,`FT%` = FT_PCT,AST,STL,BLK,`+/-` = PLUS_MINUS)
  }

  return(tmp)
}

cleanShootingZoneData <- function(dframe, team_name){
  rank_data <- dframe %>%
    select(TEAM_ID,TEAM_NAME,RA_FG_PCT,PAINT_FG_PCT,MIDRANGE_FG_PCT,CORNER3_FG_PCT,LC3_FG_PCT,RC3_FG_PCT,ATB3_FG_PCT,BACK_FG_PCT) %>%
    rowwise() %>%
    mutate(ALL_3PT_PCT = mean(c(CORNER3_FG_PCT,ATB3_FG_PCT), na.rm = TRUE)) %>%
    unnest(TEAM_ID,TEAM_NAME,RA_FG_PCT,PAINT_FG_PCT,MIDRANGE_FG_PCT,CORNER3_FG_PCT,LC3_FG_PCT,RC3_FG_PCT,ATB3_FG_PCT,BACK_FG_PCT) %>%
    pivot_longer(!c(TEAM_ID, TEAM_NAME), names_to = "Category", values_to = "Value") %>%
    unnest(c( TEAM_ID, TEAM_NAME, Value), keep_empty = TRUE)

  rank_data <- rank_data %>%
    arrange(Category, desc(Value)) %>%
    group_by(Category) %>%
    mutate(Rank = rank(desc(Value), ties.method = "min")) %>%
    mutate(
      `Shot Spectrum` = case_when(
        Category == "RA_FG_PCT" ~ "Restricted Area",
        Category == "PAINT_FG_PCT" ~ "In The Paint (Non-RA)",
        Category == "MIDRANGE_FG_PCT" ~ "Mid-Range",
        Category == "LC3_FG_PCT" ~ "Left Corner 3",
        Category == "RC3_FG_PCT" ~ "Right Corner 3",
        Category == "ATB3_FG_PCT" ~ "Above the Break 3",
        Category == "BACK_FG_PCT" ~ "Backcourt",
        Category == "CORNER3_FG_PCT" ~ "Corner 3",
        Category == "ALL_3PT_PCT" ~ "Overall 3"
      ) ) %>%
    ungroup() %>%
    select(TEAM_ID,TEAM_NAME,`Shot Spectrum`, Value, Rank) %>%
    filter(TEAM_NAME == team_name)

  tmp <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_ID,TEAM_NAME,RA_FG_PCT,PAINT_FG_PCT,MIDRANGE_FG_PCT,CORNER3_FG_PCT,LC3_FG_PCT,RC3_FG_PCT,ATB3_FG_PCT,BACK_FG_PCT) %>%
    rowwise() %>%
    mutate(ALL_3PT_PCT = mean(c(CORNER3_FG_PCT,ATB3_FG_PCT), na.rm = TRUE)) %>%
    unnest(TEAM_ID,TEAM_NAME,RA_FG_PCT,PAINT_FG_PCT,MIDRANGE_FG_PCT,CORNER3_FG_PCT,LC3_FG_PCT,RC3_FG_PCT,ATB3_FG_PCT,BACK_FG_PCT) %>%
    pivot_longer(!c(TEAM_ID, TEAM_NAME), names_to = "Category", values_to = "Value") %>%
    select(-TEAM_NAME) %>%
    filter(str_detect(Category, '_PCT')) %>%
    mutate(
           `Shot Spectrum` = case_when(
             Category == "RA_FG_PCT" ~ "Restricted Area",
             Category == "PAINT_FG_PCT" ~ "In The Paint (Non-RA)",
             Category == "MIDRANGE_FG_PCT" ~ "Mid-Range",
             Category == "LC3_FG_PCT" ~ "Left Corner 3",
             Category == "RC3_FG_PCT" ~ "Right Corner 3",
             Category == "ATB3_FG_PCT" ~ "Above the Break 3",
             Category == "BACK_FG_PCT" ~ "Backcourt",
             Category == "CORNER3_FG_PCT" ~ "Corner 3",
             Category == "ALL_3PT_PCT" ~ "Overall 3"
           ) )  %>%
    select(TEAM_ID,`Shot Spectrum` , Value) %>%
    unnest(c( TEAM_ID, Value), keep_empty = TRUE) %>%
    as.data.frame()


  raw_rank_data <- dframe %>%
    select(TEAM_ID, TEAM_NAME,RA_FGA,PAINT_FGA,MIDRANGE_FGA,CORNER3_FGA,LC3_FGA,RC3_FGA,ATB3_FGA,BACK_FGA) %>%
    rowwise() %>%
    mutate(ALL3_FGA = sum(c(CORNER3_FGA,ATB3_FGA), na.rm = TRUE)) %>%
    unnest(TEAM_ID, TEAM_NAME,RA_FGA,PAINT_FGA,MIDRANGE_FGA,CORNER3_FGA,LC3_FGA,RC3_FGA,ATB3_FGA,BACK_FGA) %>%
    pivot_longer(!c(TEAM_ID, TEAM_NAME), names_to = "Category", values_to = "Value") %>%
    unnest(c( TEAM_ID, TEAM_NAME, Value), keep_empty = TRUE)

  raw_rank_data <- raw_rank_data %>%
    arrange(Category, desc(Value)) %>%
    group_by(Category) %>%
    mutate(Rank = rank(desc(Value), ties.method = "min")) %>%
    mutate(
      `Shot Spectrum` = case_when(
        Category == "RA_FGA" ~ "Restricted Area",
        Category == "PAINT_FGA" ~ "In The Paint (Non-RA)",
        Category == "MIDRANGE_FGA" ~ "Mid-Range",
        Category == "LC3_FGA" ~ "Left Corner 3",
        Category == "RC3_FGA" ~ "Right Corner 3",
        Category == "ATB3_FGA" ~ "Above the Break 3",
        Category == "BACK_FGA" ~ "Backcourt",
        Category == "CORNER3_FGA" ~ "Corner 3",
        Category == "ALL3_FGA" ~ "Overall 3"
      ) ) %>%
    ungroup() %>%
    select(TEAM_ID,TEAM_NAME,`Shot Spectrum`, Value, Rank) %>%
    filter(TEAM_NAME == team_name)

  tmp2 <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(-c(RA_FG_PCT,PAINT_FG_PCT,MIDRANGE_FG_PCT,CORNER3_FG_PCT,LC3_FG_PCT,RC3_FG_PCT,ATB3_FG_PCT,BACK_FG_PCT)) %>%
    rowwise() %>%
    mutate(ALL3_FGA = sum(c(CORNER3_FGA,ATB3_FGA), na.rm = TRUE),
           ALL3_FGM = sum(c(CORNER3_FGM,ATB3_FGM), na.rm = TRUE)) %>%
    unnest() %>%
    unite(col='Restricted Area', c('RA_FGM', 'RA_FGA'), sep='/') %>%
    unite(col='In The Paint (Non-RA)', c('PAINT_FGM', 'PAINT_FGA'), sep='/') %>%
    unite(col='Mid-Range', c('MIDRANGE_FGM', 'MIDRANGE_FGA'), sep='/') %>%
    unite(col='Left Corner 3', c('LC3_FGM', 'LC3_FGA'), sep='/') %>%
    unite(col='Right Corner 3', c('RC3_FGM', 'RC3_FGA'), sep='/') %>%
    unite(col='Above the Break 3', c('ATB3_FGM', 'ATB3_FGA'), sep='/') %>%
    unite(col='Backcourt', c('BACK_FGM', 'BACK_FGA'), sep='/') %>%
    unite(col='Corner 3', c('CORNER3_FGM', 'CORNER3_FGA'), sep='/') %>%
    unite(col='Overall 3', c('ALL3_FGM', 'ALL3_FGA'), sep='/') %>%
    pivot_longer(!c(TEAM_ID, TEAM_NAME), names_to = "Shot Spectrum", values_to = "Raw Count (FGM/FGA)") %>%
    select(-TEAM_NAME) %>%
    select(`Shot Spectrum` , `Raw Count (FGM/FGA)`, TEAM_ID) %>%
    unnest(c( TEAM_ID), keep_empty = TRUE) %>%
    left_join(raw_rank_data %>% select(-Value),  by = c("Shot Spectrum", "TEAM_ID")) %>%
    rename(`Volume Rank` = Rank)

  tmp <- tmp %>% left_join(rank_data, by = c("Shot Spectrum", "TEAM_ID", "Value")) %>%
    left_join(tmp2, by = c("Shot Spectrum", "TEAM_ID") ) %>%
    select(-TEAM_ID, -TEAM_NAME.y, -TEAM_NAME.x)

  return(tmp)
}

cleanDefenseData <- function(dframe, team_name){
  tmp <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME,`Def Rating`=DEF_RATING,`DREB%`=DREB_PCT,STL,BLK,`Oppenent PTS (off TOV)`=OPP_PTS_OFF_TOV,
           `Opponent PTS (2nd Chance)`=OPP_PTS_2ND_CHANCE,
           `Opponent PTS (FB)`=OPP_PTS_FB,
           `Opponent PTS (Paint)`=OPP_PTS_PAINT) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Defense Metrics", values_to = "Value") %>%
    select(-TEAM_NAME)

  tmp2 <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME,`Def Rating`=DEF_RATING_RANK,`DREB%`=DREB_PCT_RANK, `STL`=STL_RANK,`BLK`=BLK_RANK,
           `Oppenent PTS (off TOV)`=OPP_PTS_OFF_TOV_RANK,
           `Opponent PTS (2nd Chance)`=OPP_PTS_2ND_CHANCE_RANK,
           `Opponent PTS (FB)`=OPP_PTS_FB_RANK,
           `Opponent PTS (Paint)`=OPP_PTS_PAINT_RANK) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Defense Metrics", values_to = "Rank") %>%
    select(-TEAM_NAME)

  tmp <- tmp %>% left_join(tmp2, by = "Defense Metrics")

  return(tmp)
}

cleanFourFactorData <- function(dframe, team_name){
  tmp <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME, `EFG%`=EFG_PCT,`FTA Rate`=FTA_RATE,`TOV%`=TM_TOV_PCT,`OREB%`=OREB_PCT) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Four Factor Metrics", values_to = "Value") %>%
    select(-TEAM_NAME)

  tmp2 <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME, `EFG%`=EFG_PCT_RANK,`FTA Rate`=FTA_RATE_RANK, `TOV%`=TM_TOV_PCT_RANK, `OREB%`=OREB_PCT_RANK) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Four Factor Metrics", values_to = "Rank") %>%
    select(-TEAM_NAME)

    tmp <- tmp %>% left_join(tmp2, by = "Four Factor Metrics")

  return(tmp)
}

cleanScoringData <- function(dframe, team_name){
  tmp <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME, `2PT Attempt` = PCT_FGA_2PT, `3PT Attempt` = PCT_FGA_3PT,
           `Paint PTS Made` = PCT_PTS_2PT,`Midrange PTS Made` = PCT_PTS_2PT_MR,`3PTS Made` = PCT_PTS_3PT,
           `Fast Break` = PCT_PTS_FB, `Free Throw` = PCT_PTS_FT,`Off TOV` = PCT_PTS_OFF_TOV,
           `In Paint` = PCT_PTS_PAINT,`Assisted 2PT` = PCT_AST_2PM, `Unassisted 2PT` = PCT_UAST_2PM,
           `Assisted 3PT` = PCT_AST_3PM, `Unassisted 3PT` = PCT_UAST_3PM
    ) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Scoring (% of Points)", values_to = "Value") %>%
    select(-TEAM_NAME)

  tmp2 <- dframe %>%
    filter(TEAM_NAME == team_name) %>%
    select(TEAM_NAME, `2PT Attempt` = PCT_FGA_2PT_RANK, `3PT Attempt` = PCT_FGA_3PT_RANK,
           `Paint PTS Made` = PCT_PTS_2PT_RANK,`Midrange PTS Made` = PCT_PTS_2PT_MR_RANK,`3PTS Made` = PCT_PTS_3PT_RANK,
           `Fast Break` = PCT_PTS_FB_RANK, `Free Throw` = PCT_PTS_FT_RANK,`Off TOV` = PCT_PTS_OFF_TOV_RANK,
           `In Paint` = PCT_PTS_PAINT_RANK,`Assisted 2PT` = PCT_AST_2PM_RANK, `Unassisted 2PT` = PCT_UAST_2PM_RANK,
           `Assisted 3PT` = PCT_AST_3PM_RANK, `Unassisted 3PT` = PCT_UAST_3PM_RANK) %>%
    pivot_longer(!c(TEAM_NAME), names_to = "Scoring (% of Points)", values_to = "Rank") %>%
    select(-TEAM_NAME)

  tmp <- tmp %>% left_join(tmp2, by = "Scoring (% of Points)")

  return(tmp)
}

createShootingFullScheduleData <- function(show_url_link, reg_url_link, team_name){
  tmp1 <- getDatafromWebsiteShooting(show_url_link) %>% unnest() %>% mutate(Season_type = "Showcase")
  tmp2 <- getDatafromWebsiteShooting(reg_url_link) %>% unnest() %>% mutate(Season_type = "Regular Season")

  tmp3 <- tmp1 %>%
    bind_rows(tmp2) %>%
    filter(TEAM_NAME == team_name) %>%
    summarise(across(c(RA_FGM,RA_FGA,PAINT_FGM, PAINT_FGA,MIDRANGE_FGM, MIDRANGE_FGA,
                       LC3_FGM,LC3_FGA,RC3_FGM, RC3_FGA, ATB3_FGM, ATB3_FGA, CORNER3_FGM, CORNER3_FGA),
                     ~ sum(.x, na.rm = TRUE)))

  return(tmp3)

}

createFullScheduleData <- function(showcase_data, reg_season_data){
  tmp1 <- showcase_data %>% unnest() %>% mutate(Season_type = "Showcase")
  tmp2 <- reg_season_data %>% unnest() %>% mutate(Season_type = "Regular Season")

  tmp3 <- tmp1 %>%
    bind_rows(tmp2)

  return(tmp3)
}


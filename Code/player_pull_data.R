library(tidyverse)
library(hoopR)
library(data.table)

getDatafromWebsite <- function(url_link){
  res <- httr::GET(url = url_link)
  data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
  column_names <- data$headers %>% as.character()
  dt <- rbindlist(data$rowSet) %>% setnames(column_names)

  return(dt)
}

combine_data <- nba_draftcombineplayeranthro(
  league_id = "00",
  season_year = most_recent_nba_season() - 1
)$Results %>%
  mutate(eff_height = (as.numeric(HEIGHT_WO_SHOES) + as.numeric(WINGSPAN))/2 )

player_profile_url <- 'https://stats.gleague.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2023-24&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='
getDatafromWebsite(player_profile_url)



player_profile_url <- function(season, season_type){
  if (season_type == 'Regular Season'){
    season_type <- 'Regular+Season'
  return(paste('https://stats.gleague.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=',season,
               '&SeasonSegment=&SeasonType=', season_type, '&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=',sep=''))
  } else {
      season_type <- "Showcase"
      return(paste('https://stats.gleague.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=',season,
                   '&SeasonSegment=&SeasonType=', season_type, '&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=',sep=''))

    }

}


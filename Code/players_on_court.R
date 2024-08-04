library(tidyverse)
library(data.table)

play_by_play_url <- function(gameId){
  return(paste('https://stats.gleague.nba.com/stats/playbyplayv2/?gameId=',gameId, '&startPeriod=0&endPeriod=14' ,sep=''))

}

advanced_boxscore_url <- function(gameId, start, end){
  return(paste('https://stats.gleague.nba.com/stats/boxscoretraditionalv2/?gameId=',gameId, '&startPeriod=0&endPeriod=14&startRange=', start, '&endRange=', end, '&rangeType=2' , sep=''))
}

calculate_time_at_period <- function(period){
  if (period > 5) {
    return( (720 * 4 + (period - 5) * (5 * 60)) * 10 )
  } else {
    return( (720 * (period - 1)) * 10 )
  }
}

split_subs<- function(df, tag){
  subs = df %>%
    select(tag, PERIOD, EVENTNUM)

  subs$SUB = tag

  subs <- subs %>%
    select(PLAYER_ID = tag, PERIOD, EVENTNUM, SUB)

  return(subs)
}

getDatafromWebsite <- function(url_link){
  res <- httr::GET(url = url_link)
  data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
  column_names <- data$headers %>% as.character()
  dt <- rbindlist(data$rowSet) %>% setnames(column_names)

  return(dt)
}

game_id <- "2022300450"
frame <- getDatafromWebsite(play_by_play_url(game_id))

substitutionsOnly <- frame %>%
  filter(EVENTMSGTYPE == 8) %>%
  select(PERIOD, EVENTNUM, PLAYER1_ID, PLAYER2_ID)

substitutionsOnly <- substitutionsOnly %>%
  rename(OUT = PLAYER1_ID, IN = PLAYER2_ID)

subs_in <- split_subs(substitutionsOnly, 'IN')
subs_out = split_subs(substitutionsOnly, 'OUT')

full_subs <- subs_in %>%
  bind_rows(subs_out)

first_event_of_period <- full_subs %>%
  group_by(PERIOD, PLAYER_ID) %>%
  top_n(1, -EVENTNUM) %>%
  ungroup()

players_subbed_in_at_each_period <- first_event_of_period %>%
  filter(SUB == 'IN') %>%
  select(PLAYER_ID, PERIOD, SUB)

periods <- c(unique(players_subbed_in_at_each_period$PERIOD))

frames <- c()

for (period in periods){
  low <- calculate_time_at_period(period) + 5
  high <- calculate_time_at_period(period + 1) - 5
  boxscore <- advanced_boxscore_url(game_id, low, high)

  boxscore_players <- getDatafromWebsite(boxscore) %>%
    select(PLAYER_NAME, PLAYER_ID, TEAM_ABBREVIATION)

  boxscore_players$PERIOD <- period

  players_subbed_in_at_period <- players_subbed_in_at_each_period %>%
    filter(PERIOD == period) %>%
    mutate(PERIOD = as.integer(PERIOD))

  joined_players <- boxscore_players %>%
    left_join(players_subbed_in_at_period, by = c('PLAYER_ID', 'PERIOD'))

  joined_players <- joined_players %>%
    filter(!is.na(SUB))

  frames <- frames %>%
    bind_rows(joined_players)

  #out <- pd.concat(frames)
  print(frames)
}

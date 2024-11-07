# Load necessary libraries
library(rvest)
library(httr)
library(dplyr)
library(magrittr)

# Define the URL of the page you want to scrape
assign_url <- "https://basketball.realgm.com/gleague/transactions/assignments/2024"
nba_player_url <- "https://stats.gleague.nba.com/stats/leaguedashplayerstatscombined?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="


# Fetch the webpage content
webpage <- read_html(url)

# Extract table data (example for extracting a stats table)
stats_table <- webpage %>%
  html_nodes("table") %>%  # Look for tables
  .[[1]] %>%  # Assuming it's the first table on the page
  html_table(fill = TRUE)  # Convert to a data frame

assignment_players <- stats_table %>%
  filter(DaysAssigned > 0) %>%
  unique()

reg_seaon_players <- getDatafromWebsite(nba_player_url)

list_of_assignment_players <- reg_seaon_players %>%
  select(PLAYER_NAME, TEAM_ID, TEAM_ABBREVIATION, GP, MIN, PTS) %>%
  filter(PLAYER_NAME %in% assignment_players$Player)


gleague_team_assign_count <- list_of_assignment_players %>%       # Using "data", filter out all rows with NAs in aa
  group_by(TEAM_ID, TEAM_ABBREVIATION) %>%          # Then, with the filtered data, group it by "bb"
  summarise(num_assign_players = n_distinct(PLAYER_NAME),
            avg_mins = round(mean(MIN, na.rm = TRUE), 2),
            avg_pts = round(mean(PTS, na.rm = TRUE), 2),
            avg_gp = round(mean(GP, na.rm = TRUE), 2) ) %>%   # Now summarise with unique elements per group
  ungroup()

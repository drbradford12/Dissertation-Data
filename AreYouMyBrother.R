library(tidyverse)
library(hoopR)
library(data.table)
library(ggcorrplot)
library(corrplot)
library(ggpubr)
library(factoextra)
library(ggpcp)

# Pulls the data from RealGM with assignments
source("Realgm_data_pull.R")

getDatafromWebsite <- function(url_link){
  res <- httr::GET(url = url_link)
  data <- httr::content(res) %>% .[['resultSets']] %>% .[[1]]
  column_names <- data$headers %>% as.character()
  dt <- rbindlist(data$rowSet) %>% setnames(column_names)

  return(dt)
}


player_profile_url <- 'https://stats.gleague.nba.com/stats/leaguedashplayerbiostats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2023-24&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

gleague_teams <- getDatafromWebsite(player_profile_url) %>%
  select(TEAM_ID, TEAM_ABBREVIATION) %>%
  unique()

nba_teams <- nba_teams() %>%
  select(team_id, team_abbreviation)

nba_g_map <- read.csv("GLeague_to_NBA_map.csv", header = TRUE)
nba_g_map_winners <- read.csv("GLeague_to_NBA_map_winners.csv", header = TRUE)


# Create the season boxscores into one table
list_of_seasons <- c('2013-14','2014-15','2015-16','2016-17','2017-18', '2018-19','2020-21','2021-22', '2022-23', '2023-24')

pull_boxscore <- function(season_type = "Regular+Season", season_year = "2023-24", league_id = "20"){
    url <- paste('https://stats.gleague.nba.com/stats/leaguedashteamstatscombined?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=',league_id,
                 '&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=',
                 season_year,'&SeasonSegment=&SeasonType=',season_type,
                 '&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=', sep ="")
    return(url)
}

final_data <- c()

for (i in list_of_seasons){
  tmp <- getDatafromWebsite(pull_boxscore(season_year = i)) %>%
    select(TEAM_ID, `W%` = W_PCT, ORTG = E_OFF_RATING, DRTG = E_DEF_RATING, NRTG = E_NET_RATING, AST_TO,
           `OREB%` = OREB_PCT, Pace = E_PACE, `TOV%` = TM_TOV_PCT, `eFG%` = EFG_PCT, `TS%` = TS_PCT) %>%
    #select(TEAM_ID, W_PCT:PTS) %>%
    mutate(season_year = i)

  final_data <- final_data %>%
    bind_rows(tmp)

}

season_data <- final_data %>%
  filter(season_year == "2023-24") %>%
  left_join(nba_g_map %>% select(-nba_team_id), by = c("TEAM_ID" = "gleague_team_id")) %>%
  filter(!is.na(gleague_team_abbreviation))

season_data_assign <- season_data %>%
  left_join(gleague_team_assign_count, by = c("TEAM_ID", "gleague_team_abbreviation"= "TEAM_ABBREVIATION")) %>%
  mutate(assign_avg_mins = ifelse(is.na(avg_mins), 0.0, avg_mins ),
         assign_avg_pts = ifelse(is.na(avg_pts), 0.0, avg_pts ),
         assign_avg_gp = ifelse(is.na(avg_gp), 0.0, avg_gp ),
         num_assign_players = ifelse(is.na(num_assign_players), 0, num_assign_players)
         ) %>%
  select(-c(avg_mins, avg_pts, avg_gp))

nba_final_data <- c()

for (i in list_of_seasons){
  tmp1 <- getDatafromWebsite(pull_boxscore(season_year = i, league_id = "00")) %>%
    select(TEAM_ID, `W%` = W_PCT, ORTG = E_OFF_RATING, DRTG = E_DEF_RATING, NRTG = E_NET_RATING, AST_TO,
           `OREB%` = OREB_PCT, Pace = E_PACE, `TOV%` = TM_TOV_PCT, `eFG%` = EFG_PCT, `TS%` = TS_PCT) %>%
    #select(TEAM_ID, W_PCT:PTS) %>%
    mutate(season_year = i)

  nba_final_data <- nba_final_data %>%
    bind_rows(tmp1)

}

nba_season_data <- nba_final_data %>%
  filter(season_year == "2023-24") %>%
  left_join(nba_g_map %>% select(-gleague_team_id), by = c("TEAM_ID" = "nba_team_id")) %>%
  filter(!is.na(gleague_team_abbreviation))



create_cluster_data <- function(dframe, num_clusters) {
   kprototype_df <- dframe %>%
  column_to_rownames("TEAM_ID") %>%
  dplyr::select(-c("season_year","nba_team_abbreviation", "gleague_team_abbreviation"))

   #correlation plot for mixture data type
   #change column names for more better labels visual
   for (i in 1:ncol(kprototype_df)){
     colnames(kprototype_df)[i] <- paste0("X", i-1)

   }

   team_clusters <- kmeans(kprototype_df, num_clusters)

   return(team_clusters)
 }

create_final_cluster_data <- function(cluster_dframe, og_dframe, num_clusters){
 fit_df <- factor(cluster_dframe$cluster, order =  TRUE,
                  levels = c(1:num_clusters))
 fit <- data.frame(og_dframe, fit_df) %>%
   mutate(clusters = factor(fit_df))

 return(fit)

 }

create_cluster_graphic <- function(cluster_dframe, og_frame, last_col_num){
return(fviz_cluster(cluster_dframe, data = og_frame[,-c(1, 12:last_col_num)],
              palette = c("darkgreen","darkorange","darkblue", "steelblue", "darkred"),
              geom = "point",
              ellipse.type = "convex",
              ggtheme = theme_bw()
 ) )

}


gleague_cluster_data <- create_final_cluster_data(create_cluster_data(season_data_assign, 4), season_data_assign, 4) %>%
  select(gl_team_id = TEAM_ID, season_year, nba_team_abbreviation, gleague_team_abbreviation,
         num_assign_players, assign_avg_mins, assign_avg_pts, assign_avg_gp,
         gl_clusters = clusters, gl_wpct = `W.`, gl_ORTG = ORTG, gl_DRTG = DRTG, gl_NRTG = NRTG,
         gl_AST_TO = AST_TO, gl_orebpct = `OREB.`, gl_Pace = Pace, gl_tovpct = `TOV.`, gl_efgpct = `eFG.`,
         gl_tspct = `TS.`)

rownames(gleague_cluster_data) <- NULL

nba_cluster_data <- create_final_cluster_data(create_cluster_data(nba_season_data, 4), nba_season_data, 4) %>%
  select(nba_team_id = TEAM_ID, season_year, nba_team_abbreviation, gleague_team_abbreviation,
         nba_clusters = clusters, nba_wpct = `W.`, nba_ORTG = ORTG, nba_DRTG = DRTG, nba_NRTG = NRTG,
         nba_AST_TO = AST_TO, nba_orebpct = `OREB.`, nba_Pace = Pace, nba_tovpct = `TOV.`, nba_efgpct = `eFG.`,
         nba_tspct = `TS.`)

rownames(nba_cluster_data) <- NULL

#create_cluster_graphic(create_cluster_data(season_data_assign, 4), season_data_assign, 15)
#create_cluster_graphic(create_cluster_data(nba_season_data, 4), nba_season_data, 14)


full_data_clusters <- gleague_cluster_data %>%
  full_join(nba_cluster_data) %>%
  mutate(nba_and_g_name = paste(nba_team_abbreviation, gleague_team_abbreviation, sep=", "),
         gl_clusters = ifelse(is.na(gl_clusters), 0, gl_clusters),
         num_assign_players = ifelse(is.na(num_assign_players), 0, num_assign_players),
         assign_avg_mins = ifelse(is.na(assign_avg_mins), 0.0, assign_avg_mins),
         assign_avg_pts = ifelse(is.na(assign_avg_pts), 0.0, assign_avg_pts),
         assign_avg_gp = ifelse(is.na(assign_avg_gp), 0.0, assign_avg_gp)
         )

full_data_clusters$nba_clusters <- as.numeric(levels(full_data_clusters$nba_clusters))[full_data_clusters$nba_clusters]

library(GGally)

ggpairs(full_data_clusters, columns = c(5:15,18:31))

full_data_clusters %>%
  arrange(nba_clusters) %>%
  pcp_select(c(21,9,5:8,10:19,22:31))  %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange(method="from-right") %>%
  ggplot(aes_pcp()) +
  geom_pcp_boxes(fill="grey80") +
  geom_pcp_boxes(boxwidth=0.1) +
  geom_pcp(aes(colour = as.character(nba_clusters) ), alpha = 1.5, axiswidth = c(0,0)) +
  scale_colour_manual(values=c("darkgreen","darkorange","darkblue", "steelblue")) +
  guides(colour=guide_legend(override.aes = list(alpha=1))) +
  geom_pcp_labels() +
  scale_x_discrete(expand = expansion(add=0.2)) +
  theme_bw() +
  theme(axis.text.x = element_text(face="bold", angle=90))


ggparcoord_full_data_clusters <- full_data_clusters %>%
  mutate(nba_clusters = as.factor(nba_clusters))

ggparcoord(ggparcoord_full_data_clusters, c(9,5:8,10:19,22:31), groupColumn = 21,  splineFactor = 4,
           scale = "uniminmax", scaleSummary = "uniminmax") +
    scale_colour_manual(values=c("darkgreen","darkorange","darkblue", "steelblue")) +
    theme_bw() +
    theme(axis.text.x = element_text(face="bold", angle=90))



  #scale_colour_gradient2("nba_clusters", mid="grey80", midpoint = 3, low="darkred", high="darkblue")

# graphic <- full_data_clusters %>% select(-c(gl_team_id, season_year, nba_team_id, nba_team_abbreviation, gleague_team_abbreviation)) %>%
#      pivot_longer(!c(nba_and_g_name), names_to = "cluster_version", values_to = "cluster_number")


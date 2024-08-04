library(tidyverse)
library(gt)
library(gtExtras)

# functions that will format the tables for the pdf
designSidebySide <- function(table1, table2, overall_title, table1_label, table2_label, team_color){
  tmp_table <- data.frame(Ltable = table1, Rtable = table2) %>%
    gt() %>%
    fmt_markdown(columns = everything()) %>%
    tab_options(column_labels.hidden = FALSE)%>%
    tab_header(
      title = md(paste("**", overall_title,"**", sep='') )) %>%
    cols_label(
      Ltable = md(paste("**", table1_label,"**", sep="") ),
      Rtable = md(paste("**", table2_label,"**", sep="") ) ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black"
    )

  return(tmp_table)
}

designSingleTable <- function(table, overall_title, table_label, team_color){
  tmp_table <- data.frame(Ltable = table) %>%
    gt() %>%
    fmt_markdown(columns = everything()) %>%
    tab_options(column_labels.hidden = FALSE)%>%
    tab_header(
      title = md(paste("**", overall_title,"**", sep='') )) %>%
    cols_label(
       Ltable = md(paste("**", table_label,"**", sep="") )) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black"
    )

  return(tmp_table)

}

designFourFactorsTable <-function(dframe, team_color){

  tmp_table <- dframe %>%
    gt() %>%
    cols_label(
      `Four Factor Metrics` = md("**Four Factor Metrics**"),
      Value = md("**Value**"),
      Rank = md("**Rank**")) %>%
    fmt_percent(
      columns = Value,
      rows = `Four Factor Metrics`=="FTA Rate" | `Four Factor Metrics`=="EFG%" | `Four Factor Metrics`=="TOV%" | `Four Factor Metrics`=="OREB%",
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_width(
      starts_with("Four Factor Metrics") ~ px(250),
      starts_with("Value") ~ px(125),
      starts_with("Rank") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Four Factor Metrics") | starts_with("Value") | starts_with("Rank")
    ) %>%
    tab_style(
      style = list(cell_text(style = "italic",weight = "bold"),cell_fill(color="gray88")),
      locations = cells_body(
        columns = starts_with("Rank")
      )
    ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(4)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank <= 5
      )
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank >= 24
      )
    )%>%
    as_raw_html()

  return(tmp_table)
}

designScoringTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    mutate(Value = as.numeric(Value),
           Rank = as.numeric(Rank)) %>%
    gt() %>%
    fmt_number(
      columns = starts_with("Rank"),
      decimals = 0
    ) %>%
    fmt_percent(
      columns = starts_with("Value"),
      decimals = 1
    ) %>%
    cols_width(
      starts_with("Scoring") ~ px(225),
      starts_with("Value") ~ px(75),
      starts_with("Rank") ~ px(75),

    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Scoring") | starts_with("Value") | starts_with("Rank")
    ) %>%
    tab_style(
      style = list(cell_text(style = "italic",weight = "bold"),cell_fill(color="gray88")),
      locations = cells_body(
        columns = starts_with("Rank")
      )
    ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(4)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank <= 5
      )
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank >= 26
      )
    )%>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("bottom"),
          color = "#000000",
          weight = px(3)
        )),
      locations = list(
        cells_body(
          columns = starts_with("Scoring") | starts_with("Value") | starts_with("Rank"),
          rows = c(2,5,9,11)
        ) )
    ) %>%
    as_raw_html()

  return(tmp_table)
}

designShotZoneTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    filter(`Shot Spectrum` != "Backcourt") %>%
    mutate(Value = as.numeric(Value)) %>%
    gt() %>%
    fmt_number(
      columns = c("Rank", "Volume Rank"),
      decimals = 0
    ) %>%
    fmt_percent(
      columns = starts_with("Value"),
      decimals = 1
    ) %>%
    cols_width(
      starts_with("Shot Spectrum") ~ px(150),
      starts_with("Raw") ~ px(100),
      starts_with("Volume") ~ px(75),
      starts_with("Value") ~ px(75),
      starts_with("Rank") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Shot Spectrum") | starts_with("Value") |  starts_with("Rank") | starts_with("Raw") | starts_with("Volume")
    )  %>%
    tab_style(
      style = list(cell_text(style = "italic",weight = "bold"),cell_fill(color="gray88")),
      locations = cells_body(
        columns = starts_with("Rank") | starts_with("Volume")
      )
    ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("bottom"),
          color = "#000000",
          weight = px(3)
        )),
      locations = list(
        cells_body(
          columns = starts_with("Shot Spectrum") | starts_with("Value") | starts_with("Raw") | starts_with("Rank") | starts_with("Volume"),
          rows = 3
        ) )
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank <= 5
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = `Raw Count (FGM/FGA)`,
        rows = `Volume Rank` <= 5
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank >= 26
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = `Raw Count (FGM/FGA)`,
        rows = `Volume Rank` >= 26
      )
    ) %>%
    as_raw_html()

  return(tmp_table)
}

designAdvBoxscoreTable <- function(dframe, team_color, table_type){
  tmp_table <- dframe %>%
    gt() %>%
    cols_label(
      `Adv. Boxscore Metrics` = md("**Adv. Boxscore Metrics**"),
      Value = md("**Value**"),
      Rank = md("**Rank**")) %>%
    fmt_number(
      columns = Value,
      rows = `Adv. Boxscore Metrics`=="Offensive Rating" |`Adv. Boxscore Metrics`=="Defensive Rating" | `Adv. Boxscore Metrics`=="Net Rating"| `Adv. Boxscore Metrics`=="Pace"| `Adv. Boxscore Metrics`=="Ast Ratio"   ,
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_width(
      starts_with("Adv. Boxscore Metrics") ~ px(250),
      starts_with("Value") ~ px(125),
      starts_with("Rank") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Adv. Boxscore Metrics") | starts_with("Value") | starts_with("Rank")
    ) %>%
    tab_style(
      style = list(cell_text(style = "italic",weight = "bold"), cell_fill(color="gray88")),
      locations = cells_body(
        columns = starts_with("Rank")
      )
    ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(4)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank <= 5
      )
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank >= 24
      )
    )

  if (table_type == "single"){
    tmp_table
  }else{
    tmp_table <- tmp_table %>%
      as_raw_html()
  }

  return(tmp_table)
}

designDefenseTable <-function(dframe, team_color){

  tmp_table <- dframe %>%
    gt() %>%
    cols_label(
      `Defense Metrics` = md("**Defense Metrics**"),
      Value = md("**Value**"),
      Rank = md("**Rank**")) %>%
    fmt_number(
      columns = Value,
      rows = `Defense Metrics`=="Def Rating" |`Defense Metrics`=="STL"|`Defense Metrics`=="BLK"|`Defense Metrics`=="Oppenent PTS (off TOV)"|`Defense Metrics`=="Opponent PTS (2nd Chance)"|`Defense Metrics`=="Opponent PTS (FB)"|`Defense Metrics`=="Opponent PTS (Paint)",
      decimals = 2,
      use_seps = FALSE
    ) %>%
    fmt_percent(
      columns = Value,
      rows = `Defense Metrics`=="DREB%",
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_width(
      starts_with("Defense Metrics") ~ px(250),
      starts_with("Value") ~ px(125),
      starts_with("Rank") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Defense Metrics") | starts_with("Value") | starts_with("Rank")
    ) %>%
    tab_style(
      style = list(cell_text(style = "italic",weight = "bold"),cell_fill(color="gray88")),
      locations = cells_body(
        columns = starts_with("Rank")
      )
    ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(4)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#fdae6b")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank <= 5
      )
    )%>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = Value,
        rows = Rank >= 24
      )
    )%>%
    as_raw_html()

  return(tmp_table)
}

designLineupTable <- function(dframe, team_abb, team_color){
  tmp_table <- dframe %>%
    gt() %>%
    gt::cols_width(
      starts_with("Lineup") ~ px(650),
      starts_with("MIN") ~ px(65),
      starts_with("GP") ~ px(50),
      starts_with("PPP") ~ px(65),
      starts_with("+/-") ~ px(50)
    ) %>%
    fmt_number(
      columns = c("PPP", "MIN"),
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Lineup") | starts_with("MIN") | starts_with("PPP")| starts_with("GP") | starts_with("+")
    ) %>%
    tab_header(
      title = md(paste("**NOTABLE ", team_abb," LINEUPS**", sep='') ) ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      )

  return(tmp_table)
}

designTwoWayTable <- function(dframe, team_abb, team_color){
  if (nrow(dframe) < 2 ){
    tmp_table <- dframe %>%
      gt() %>%
      tab_header(
        title = md(paste("**Two-Way ", team_abb, " Players**", sep='')) )%>%
      tab_options(
        heading.background.color = as.character(team_color), #"#1D42BA",
        column_labels.background.color = "gray88",
        column_labels.border.bottom.width = "3px",
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        table.width = px(400))
  } else{
  tmp_table <- dframe %>%
    gt() %>%
    gt::cols_width(
      starts_with("Player Name") ~ px(280),
      starts_with("MIN") ~ px(75),
      starts_with("GP") ~ px(50),
      starts_with("PTS") ~ px(75),
      starts_with("FG%") ~ px(75),
      starts_with("3P%") ~ px(75),
      starts_with("FT%") ~ px(75),
      starts_with("AST") ~ px(50),
      starts_with("BLK") ~ px(50),
      starts_with("STL") ~ px(50),
      starts_with("+/-") ~ px(50)
    ) %>%
    fmt_percent(
      columns = c("FG%","3P%","FT%"),
      decimals = 2,
      use_seps = FALSE
    ) %>%
    fmt_number(
      columns = c("MIN", "GP", "PTS"),
      decimals = 1,
      use_seps = FALSE
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste("**Two-Way ", team_abb, " Players**", sep='') ))%>%
    tab_options(
      heading.background.color = as.character(team_color), #"#1D42BA"
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      table.width = px(400))
  }

  return(tmp_table)
}

designAssignmentTable <- function(dframe, team_abb, team_color){
  if (nrow(dframe) < 2 ){
    tmp_table <- dframe %>%
      gt() %>%
      tab_header(
        title = md(paste("**Assignment ",team_abb, " Players**", sep='')) ) %>%
      tab_options(
        heading.background.color = as.character(team_color),
        column_labels.background.color = "gray88",
        column_labels.border.bottom.width = "3px",
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        table.width = px(400))
  } else{

  tmp_table <- dframe %>%
    #select(-AGE) %>%
    gt() %>%
    gt::cols_width(
      starts_with("PLAYER_NAME") ~ px(280),
      # starts_with("POS") ~ px(50),
      starts_with("MIN") ~ px(75),
      starts_with("PTS") ~ px(50),
      # starts_with("Assign") ~ px(100),
      # starts_with("DaysAssigned") ~ px(50),
      starts_with("GP") ~ px(50),
      starts_with("FG%") ~ px(75),
      starts_with("3P%") ~ px(75),
      starts_with("FT%") ~ px(75),
      starts_with("AST") ~ px(50),
      starts_with("BLK") ~ px(50),
      starts_with("STL") ~ px(50),
      starts_with("+/-") ~ px(50)
    )%>%
    fmt_percent(
      columns = c("FG%","3P%","FT%"),
      decimals = 2,
      use_seps = FALSE
    ) %>%
    fmt_number(
      columns = c("MIN", "GP", "PTS"),
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste("**Assignment ",team_abb, " Players**", sep='')))%>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      table.width = px(400))
  }

  return(tmp_table)
}

designAdvPeriodStatsTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    gt() %>%
    fmt_number(
      columns = starts_with("D") | starts_with("O") | starts_with("NET"),
      decimals = 2
    ) %>%
    cols_width(
      starts_with("period") ~ px(75),
      starts_with("D") ~ px(75),
      starts_with("O") ~ px(75),
      starts_with("NET") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    )  %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>%
    as_raw_html()

  return(tmp_table)
}

designTenBoxscorePer36Table <- function(dframe,team_color, title_label){
  tmp_table <- dframe %>%
    gt() %>%
    gt::cols_width(
      starts_with("Player Name") ~ px(280),
      starts_with("MIN") ~ px(50),
      starts_with("GP") ~ px(50),
      starts_with("PTS") ~ px(50),
      starts_with("FG%") ~ px(50),
      starts_with("3P%") ~ px(50),
      starts_with("FT%") ~ px(50),
      starts_with("OREB") ~ px(50),
      starts_with("DREB") ~ px(50),
      starts_with("AST") ~ px(50),
      starts_with("TOV") ~ px(50),
      starts_with("BLK") ~ px(50),
      starts_with("STL") ~ px(50),
      starts_with("PF") ~ px(50),
      starts_with("+/-") ~ px(50)
    ) %>%
    fmt_percent(
      columns = c("FG%","3P%","FT%"),
      decimals = 0,
      use_seps = FALSE
    ) %>%
    fmt_number(
      columns = c("MIN", "GP", "PTS"),
      decimals = 1,
      use_seps = FALSE
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    ) %>%
    tab_header(
      title = md(paste("**MCC", title_label,  "Stats**" , sep = ' ')) ) %>%
    tab_options(
      heading.background.color = as.character(team_color), #"#1D42BA"
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      table.width = px(400))

  return(tmp_table)
}

designSpecialLineupTable <- function(dframe, team_abb, team_color){
  tmp_table <- dframe %>%
    gt() %>%
    gt::cols_width(
      starts_with("Lineup") ~ px(500),
      starts_with("MIN") ~ px(50),
      starts_with("GP") ~ px(50),
      starts_with("PPP") ~ px(50),
      starts_with("+/-") ~ px(50),
      starts_with("DefRtg") ~ px(50),
      starts_with("PACE") ~ px(50)
      #starts_with("AST:TO") ~ px(50)

    ) %>%
    fmt_number(
      columns = c("PPP", "MIN"),
      decimals = 2,
      use_seps = FALSE
    ) %>%
    fmt_number(
      columns = c("DefRtg", "PACE", "+/-"),
      decimals = 1,
      use_seps = FALSE
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
      ) %>%
    tab_header(
      title = md(paste("**NOTABLE ",team_abb," LINEUPS**", sep='') ) ) %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    )

  return(tmp_table)
}

designPersonalFoulTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    gt() %>%
    fmt_number(
      columns = c("PF", "MIN"),
      decimals = 1
    ) %>%
    cols_width(
      starts_with("Player Name") ~ px(180),
      starts_with("PF") ~ px(75),
      starts_with("MIN") ~ px(75),
    )  %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>%
    as_raw_html()

  return(tmp_table)
}

designFourFactorsNoRankTable <-function(dframe, team_color){

  tmp_table <- dframe %>%
    gt() %>%
    cols_label(
      `Four Factor Metrics` = md("**Four Factor Metrics**"),
      Value = md("**Value**")) %>%
    fmt_percent(
      columns = Value,
      rows = `Four Factor Metrics`=="FTA Rate" | `Four Factor Metrics`=="EFG%" | `Four Factor Metrics`=="TOV%" | `Four Factor Metrics`=="OREB%",
      decimals = 2,
      use_seps = FALSE
    ) %>%
    cols_width(
      starts_with("Four Factor Metrics") ~ px(250),
      starts_with("Value") ~ px(125)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Four Factor Metrics") | starts_with("Value")
    )  %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>%
    as_raw_html()

  return(tmp_table)
}

designShotZoneNoRankTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    mutate(Value = as.numeric(Value)) %>%
    gt() %>%
    fmt_percent(
      columns = starts_with("Value"),
      decimals = 1
    ) %>%
    cols_width(
      starts_with("Shot Spectrum") ~ px(150),
      starts_with("Raw") ~ px(100),
      starts_with("Value") ~ px(75)
    ) %>%
    cols_align(
      align = c("center"),
      columns = starts_with("Shot Spectrum") | starts_with("Value") | starts_with("Raw")
    )  %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    )  %>%
    as_raw_html()

  return(tmp_table)
}

designPlayoffTable <- function(dframe, team_color){
  tmp_table <- dframe %>%
    gt() %>%
    fmt_number(
      columns = c("Games Back", "Clinched Playoff Birth"),
      decimals = 1
    ) %>%
    cols_width(
      starts_with("Team City") ~ px(180),
      starts_with("Team Name") ~ px(150),
      starts_with("Record") ~ px(100),
      starts_with("Games Back") ~ px(50),
      starts_with("Clinched Playoff Birth") ~ px(75),
      starts_with("Ahead At Half") ~ px(100),
      starts_with("Behind At Half") ~ px(100)
    ) %>%
    cols_align(
      align = c("center"),
      columns = everything()
    )  %>%
    tab_options(
      heading.background.color = as.character(team_color),
      column_labels.background.color = "gray88",
      column_labels.border.bottom.width = "3px",
      column_labels.border.bottom.color = "black",
      data_row.padding = px(5)
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#9ecae1")
      ),
      locations = cells_body(
        columns = c(`Team City`, `Team Name`, Record, `Games Back`) | starts_with("Clinched") | starts_with("Ahead") | starts_with("Behind"),
        rows = `Clinched Playoff Birth` == 1.0
      )
    )  %>%
    tab_style(
      style = list(
        cell_fill(color = "#FFCCCB")
      ),
      locations = cells_body(
        columns = everything(),
        rows = `Games Back` > 12
      )
    )  %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("bottom"),
          color = "#000000",
          weight = px(3)
        )),
      locations = list(
        cells_body(
          columns = everything(),
          rows = 6
        ) )
    )

  return(tmp_table)
}

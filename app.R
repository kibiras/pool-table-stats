library(shiny)
library(RMariaDB)
library(dplyr)
library(magrittr)
library(DBI)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),
                align="center",
                h1("Previous game statistics"),
                tableOutput("lastgame"),
                h1("Total statistics"),
                dataTableOutput("tbl")
)

server <- function(input, output, session) {
  table_name <- 'event3'
  conn <- dbConnect(
    drv = RMariaDB::MariaDB(), 
    username = 'seb',
    password = 'seb',
    dbname = 'seb',
    host = '127.0.0.1',
    port = '3306'
  ) 
  
  tbl(conn, table_name)  %>%
    filter(name %in% c("combinationEnd", "combinationStart")) %>%
    select(game_id, combination_id, status, time, name) %>%
    group_by(game_id, combination_id, name) %>%
    summarize(tries = n(), time = max(time, na.rm = TRUE), status) %>%
    summarise(start_time = min(time, na.rm = TRUE), end_time = max(time, na.rm = TRUE), tries) %>%
    mutate(duration = (end_time - start_time)) %>%
    group_by(game_id)   %>%
    mutate(points = ifelse(tries == 1, 2, ifelse(tries == 2, 1, 0)))   %>%
    summarise(time_in_minutes = sum(duration, na.rm = TRUE)/60, score = sum(points, na.rm = TRUE)) %>%
    mutate(time_in_minutes = round(time_in_minutes, 2)) %>%
    arrange(desc(score), time_in_minutes) %>%
    collect() %>%
    mutate(rank = row_number()) %>%
    select(rank, everything()) -> summary 

  output$tbl <- renderDataTable({
    summary 
  })
  output$lastgame <- renderTable({
    summary %>%
      filter(game_id == max(game_id))
  })
  
}

shinyApp(ui, server)
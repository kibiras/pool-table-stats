library(shiny)
library(RMariaDB)
library(dplyr)
library(magrittr)
library(DBI)
library(shinythemes)
library(dbplyr)

ui <- fluidPage(theme = shinytheme("cyborg"),
                align="center",
                h6("Previous game statistics"),
                tableOutput("lastgame"),
                h4("Total statistics"),
                dataTableOutput("tbl"),
                img(src='diagram.png', align = "center", height = "200px")
)

server <- function(input, output, session) {
  table_name <- 'event'
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
    summarize(tries = n(), time = max(time, na.rm = TRUE), status = sum(status)) %>%
    summarise(start_time = min(time, na.rm = TRUE), end_time = max(time, na.rm = TRUE), tries, status) %>%
    mutate(duration = (end_time - start_time)) %>%
    group_by(game_id) %>%
    mutate(points = ifelse(tries == 1 & status == 1, 2, ifelse(tries == 2 & status == 1, 1, 0)))   %>%
    summarise(time_in_minutes = sum(duration, na.rm = TRUE)/60, completed_shots = sum(status), score = sum(points, na.rm = TRUE)) %>%
    mutate(time_in_minutes = round(time_in_minutes, 2)) %>%
    arrange(desc(score), time_in_minutes) %>%
    collect() %>%
    mutate(rank = row_number()) %>%
    select(rank, everything()) -> summary 

  output$tbl <- renderDataTable(
    summary,
    options = list(pageLength = 10,
                   rowCallback = I(
                     'function(row, data) {
        $("td", row).each(function(i) {
            if (i != 0) return; 
            if (parseFloat(data[i]) <= 3)
                $(this).css("color", "green");
        });
    }')
    )
  )
  output$lastgame <- renderTable({
    summary %>%
      filter(game_id == max(game_id) - 1) %>%
      mutate(completed_shots = as.integer(completed_shots), score = as.integer(score))
  })
  
}

shinyApp(ui, server)
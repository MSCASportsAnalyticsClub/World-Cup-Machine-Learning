## ----Preamble, echo=FALSE------------------------------------------------
# Enter package in p_load()
# If package is not installed, p_load() will install and load the package
if(!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
  }
pacman::p_load(tidyverse, ggthemes, here, jsonlite)

options(stringsAsFactors = FALSE)

source("./src/01-event_logger.R")
source("./src/02-parse_possessions.R")

## Load Data ==========================

wc_data <- data_frame(GameID = character(),
                      event_id = character(),
                      type_id = integer(),
                      type_name = character(),
                      timestamp = character(),
                      duration = numeric(),
                      team_name = character(),
                      possession_team_name = character(),
                      play_pattern_name = character(),
                      pass_length = numeric(),
                      pass_height = character(),
                      pass_angle = numeric(),
                      goalkeeper_type_name = character(),
                      goalkeeper_outcome_name = character(),
                      loc_x = numeric(),
                      loc_y = numeric(),
                      shot_x = numeric(),
                      shot_y = numeric(),
                      shot_z = numeric(),
                      shot_body_part = character(),
                      shot_technique = character(),
                      shot_outcome = character(),
                      lead_possessor = logical(),
                      pass_sequence_label = factor(),
                      goals = character()
                      )



games <- sub("\\.json", "", list.files("./data/events"))
pk_exclude <- c(7524, 7566, 7581, 7582, 7585, 8652, 8657)
games_logged <- 0

for(game in games[!games %in% pk_exclude]){

  print(paste0("Game: ", game , " begin logging"))

  events_json <- fromJSON(paste0("data/events/", game, ".json"),
                          simplifyVector = FALSE)

  events      <- wc_log_events(events_json)
  possessions <- wc_parse_possessions(events)

  #assigning gameID to the output so that we can differentiate easily on the game
  possessions$GameID <- game

  wc_data <- wc_data %>%
    bind_rows(possessions)

  games_logged <- games_logged + 1
  print(paste0("Game: ", game , " logging complete", " - ", games_logged, " total games logged"))
}



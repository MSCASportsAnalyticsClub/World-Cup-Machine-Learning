## ----Preamble, echo=FALSE------------------------------------------------
# Enter package in p_load()
# If package is not installed, p_load() will install and load the package
if(!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
  }
pacman::p_load(tidyverse, ggthemes, here, jsonlite)

options(stringsAsFactors = FALSE)

# Set default ggplot theme to tufte
theme_set(ggthemes::theme_tufte())

## ----Data----------------------------------------------------------------

game_num <- "19725"
events_json <- fromJSON(paste0("data/events/", game_num, ".json"),
  simplifyVector = FALSE)

all_dat <- data_frame(event_id = character(),
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
                      goals = character(),
                      GameID = character())

games <- sub("\\.json", "", list.files("./data/events"))

pk_exclude <- c(7524, 7566, 7581, 7582, 7585, 8652, 8657)
games_logged <- 0

for( game in games[!games %in% pk_exclude]){

  print(paste0("Game: ", game , " begin logging"))

events_json <- fromJSON(paste0("data/events/", game, ".json"),
                          simplifyVector = FALSE)
## ----Extract-data--------------------------------------------------------
replace_na_empty <- function(x) {
  map_if(x, is.null, ~ NA)
}

event_id <- map_chr(events_json, ~ .x$id)

type_id <- map_int(events_json, ~ .x$type$id)

type_name <- map_chr(events_json, ~ .x$type$name)

timestamp <- map_chr(events_json, ~ .x$timestamp)

possession_team_name <- map_chr(events_json, ~ .x$possession_team$name)

team_name <- map_chr(events_json, ~ .x$team$name)

pass_length <- map(events_json, ~ .x$pass$length) %>%
  replace_na_empty() %>% unlist()

pass_height <- map(events_json, ~ .x$pass$height$name) %>%
  replace_na_empty() %>% unlist()

pass_angle <- map(events_json, ~ .x$pass$angle) %>%
  replace_na_empty() %>% unlist()

duration <- map(events_json, ~ .x$duration) %>%
  replace_na_empty() %>% unlist()

play_pattern_name <- map_chr(events_json, ~ .x$play_pattern$name)

goalkeeper_type_name <- map(events_json, ~ .x$goalkeeper$type$name) %>%
  replace_na_empty() %>% unlist()

goalkeeper_outcome_name <- map(events_json, ~ .x$goalkeeper$outcome$name) %>%
  replace_na_empty() %>% unlist()

locations <- map(events_json, ~ .x$location) %>%
  replace_na_empty()

loc_x <- map(locations, ~ .x[[1]]) %>%
  unlist()

loc_y <- map(locations, ~ ifelse(length(.x)>1, .x[[2]], NA)) %>%
  unlist()

shot_end_locs <- map(events_json, ~ .x$shot$end_location) %>%
  replace_na_empty()

shot_x <- map(shot_end_locs, ~ .x[[1]]) %>%
  unlist()

shot_y <- map(shot_end_locs, ~ ifelse(length(.x)>1,.x[[2]],NA)) %>%
  unlist()

shot_z <- map(shot_end_locs, ~ ifelse(length(.x)>2,.x[[3]],NA)) %>%
  unlist()

shot_body_part <- map(events_json, ~ .x$shot$body_part$name) %>%
  replace_na_empty() %>%
  unlist()

shot_technique <- map(events_json, ~ .x$shot$technique$name) %>%
  replace_na_empty() %>%
  unlist()

shot_outcome <- map(events_json, ~.x$shot$outcome$name) %>%
  replace_na_empty() %>%
  unlist()

## ----events-df-----------------------------------------------------------
events_df <- data.frame(
    event_id,
    type_id,
    type_name,
    timestamp,
    duration,
    team_name,
    possession_team_name,
    play_pattern_name,
    pass_length,
    pass_height,
    pass_angle,
    goalkeeper_type_name,
    goalkeeper_outcome_name,
    loc_x,
    loc_y,
    shot_x,
    shot_y,
    shot_z,
    shot_body_part,
    shot_technique,
    shot_outcome
  ) %>%
  #' Used to identify sequences; max `FALSE` value is the start of a sequence
  mutate(lead_possessor = possession_team_name == lead(possession_team_name)) %>%
  as_tibble()



## ------------------------------------------------------------------------
(shot_indexes <- which(str_detect(events_df$type_name, "Shot")))

## ----pass-sequences------------------------------------------------------
sequence_indexes <- imap(shot_indexes, function(x, indx) {
  start_index <- ifelse(x == shot_indexes[1], 1, shot_indexes[indx - 1] + 2)
  seq(start_index, shot_indexes[indx] + 1, 1)
  })

shots_split <- map(sequence_indexes, ~ events_df %>% slice(min(.x):max(.x)))

start_sequence <- map_int(shots_split, function(x) {
  x <- x %>% mutate(type_name_flag = ifelse(lag(type_name) == "Shot", "remove", "keep"))
  x <- x %>% filter(type_name_flag == "keep")
  #' Check if `FALSE` exist and return max index of `FALSE`; false indicates sequence has changed
  if (length(which(!x$lead_possessor)) >= 1) {
    as.integer(max(which(!x$lead_possessor)) + 2)
  } else {
      min(x$lead_possessor)
    }
  }
)

pass_sequences <- map2(shots_split, start_sequence, ~ .x %>% slice(.y:nrow(.x))) %>%
  map2(., 1:length(shots_split), ~ mutate(.x, pass_sequence_label = .y)) %>%
  bind_rows() %>%
  mutate(
    pass_sequence_label = factor(pass_sequence_label),
    type_name = ifelse(type_name == "Goal Keeper", goalkeeper_type_name, type_name)
  ) %>%
  group_by(pass_sequence_label) %>%
  # Need to account for shots that were block by someone
  # other than the goal keeper when identifying outcome of shot
  mutate(goals = case_when(
        str_detect(goalkeeper_outcome_name, "Goal Conceded|Penalty Conceded|No Touch|Touched In") ~ "goal conceded",
        str_detect(goalkeeper_type_name, "Shot Faced") & lag(type_name) == "Shot" ~ "missed shot",
        is.na(goalkeeper_outcome_name) & is.na(goalkeeper_type_name) ~ NA_character_,
        lag(type_name) != "Shot" | is.na(lag(type_name)) ~ NA_character_,
        TRUE ~ "saved"
  ))

pass_sequences$GameID <- game

all_dat <- all_dat %>%
  bind_rows(pass_sequences)

games_logged <- games_logged + 1
print(paste0("Game: ", game , " logging complete", " - ", games_logged, " total games logged"))

}








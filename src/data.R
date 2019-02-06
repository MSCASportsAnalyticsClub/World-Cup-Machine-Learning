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
events_json <- fromJSON("data/events/19725.json", simplifyVector = FALSE)

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
    goalkeeper_outcome_name
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




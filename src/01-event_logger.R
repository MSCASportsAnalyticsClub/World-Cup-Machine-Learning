#log events

wc_log_events <- function(events_json){

  num_of_events <- length(events_json)

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

  loc_y <- map(locations, ~ .x[[1]]) %>%
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

  ##========== events-df ==============
  events_df <- data_frame(
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
  mutate(lead_possessor = possession_team_name == lead(possession_team_name))

}



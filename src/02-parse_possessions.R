#02-parse_possessions.R
#' Taking an event data frame and parsing the pass sequences from it

wc_parse_possessions <- function(events_df){
  shot_indexes <- which(str_detect(events_df$type_name, "Shot"))

  ## pass-sequences ===================
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
    })

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
  return(pass_sequences)
}

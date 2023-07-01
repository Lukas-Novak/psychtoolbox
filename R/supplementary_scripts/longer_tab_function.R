longer_tab <- function(x) {
  # testing whether df contains results of the statistical tests
  if (summarize(x,
                contains_stat_tets_results = any(!is.na(across(contains("Group difference")))))$contains_stat_tets_results) {

    x %>%
      full_join(psd) %>%
      group_by(key) %>% # this group by has to be there because otherwise unwanted values might be filtered out
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      ungroup() %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(dups = duplicated(value))%>%
      filter(dups == FALSE) %>%
      select(!dups) %>%
      mutate_all(~(replace(., is.na(.), ""))) %>%
      mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
      group_by(key) %>%
      group_modify(~add_row(., .before = 1)) %>%
      ungroup() %>%
      mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
      mutate(value = if_else(is.na(value), key, value)) %>%
      mutate_all(~replace(., is.na(.), "")) %>%
      mutate(`n(%)` = paste0(as.numeric(n), " (",percent,")")) %>%
      mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
      select(-c("key","n","percent")) %>%
      relocate(`n(%)`, .after = value) %>%
      rename_with(~paste0(outcome.var," M(SD)"), ends_with(outcome.var)) %>%
      rename("variable" = "value",
             "n (%)" = `n(%)`)

  } else {
    x %>%
      mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
      group_by(key) %>%
      group_modify(~add_row(., .before = 1)) %>%
      ungroup() %>%
      mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
      mutate(value = if_else(is.na(value), key, value)) %>%
      mutate_all(~replace(., is.na(.), "")) %>%
      mutate(`n(%)` = paste0(as.numeric(n), " (",percent,")")) %>%
      mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
      select(-c("key","n","percent")) %>%
      relocate(`n(%)`, .after = value) %>%
      rename_with(~paste0(outcome.var," M(SD)"), ends_with(outcome.var)) %>%
      rename("variable" = "value",
             "n (%)" = `n(%)`)
  }
}

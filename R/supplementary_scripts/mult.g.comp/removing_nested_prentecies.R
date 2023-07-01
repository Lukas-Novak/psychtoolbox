removing_nested_prentecies <- function(x) {
  success <- FALSE
  while (!success) {
    x = x %>%
      mutate_all(~stringr::str_remove_all(., "\\)(?=.*\\))")) %>%
      mutate_all(~stringr::str_replace(., "\\((.*)\\(", "(\\1"))
    # check for success
    success <- x %>% reframe(across(everything(), ~stringr::str_count(., "\\(") >= 2)) %>% any(isTRUE(.),na.rm = T) == FALSE
  }
  return(x)
}

# desc.tab = function(groups, outcome.var, df) {
#   factors.dat = df %>% select(where(is.factor)) %>% names()
#   df %>% tidyr::drop_na(groups)
#   df %>% dplyr::mutate(across(paste0(factors.dat), ~paste(as.numeric(.), .))) %>%
#     tidyr::pivot_longer(groups,
#                         names_to = "key",
#                         values_to = "value") %>%
#     dplyr::group_by(key,value) %>%
#     dplyr::summarise(across(paste(outcome.var,sep = ","), list(mean=mean,
#                                                                sd=sd)),
#                      n = n()) %>%
#     mutate(percent = n / sum(n)*100) %>%
#     ungroup()
# }



# testing data
# dat = tibble(
#   eps = rnorm(300,0,h(x)),
#   Gender_prep = as.factor(rbinom(300, size = 1, prob = .30)),
#   Age = as.numeric(rnorm(n = 300, mean = 35, sd = 10)),
#   Work_years = as.numeric(rnorm(n = 300, mean = 50, sd = 15)),
#   Education_prep = as.factor(rbinom(n = 300, size = 2, prob = .5)),
#   Family_status = as.factor(case_when(Age > 20 ~ "Married",
#                                       Age > 15 ~ "In relationship",
#                                       Age < 15 ~ "Not in relationship")),
#   Education = recode_factor(Education_prep,
#                             "0" = "Basic schoool",
#                             "1" = "High school",
#                             "2" = "University"),
#   Gender = recode_factor(Gender_prep,
#                          "0"="Male",
#                          "1" = "Female")
# )

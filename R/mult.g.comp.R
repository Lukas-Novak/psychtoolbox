library(dplyr)

set.seed(54854)
dat = tibble(
  Age = as.numeric(rnorm(n = 300, mean = 35, sd = 10)),
  Education = as.factor(rbinom(n = 300, size = 2, prob = .5)),
  Family_status = as.factor(case_when(Age > 20 ~ "Married",
                                      Age > 15 ~ "In relationship",
                                      Age < 15 ~ "Not in relationship"))
)

#kruskal.test(dat$Age ~ dat$Family_status)
#kruskal.test(dat$Age ~ dat$Education)
# psychtoolbox::two.g.comp

tab = function(groups, outcome.var, df) {
  df %>% tidyr::drop_na(groups)
  df %>% tidyr::pivot_longer(groups,
                             names_to = "key",
                             values_to = "value") %>%
    dplyr::group_by(key,value) %>%
    dplyr::summarise(mean = round(mean(eval(parse(text = outcome.var)),  na.rm = T),digits = 2),
                     sd = round(sd(eval(parse(text = outcome.var)), na.rm = T),digits = 2),
                     n = n()) %>%
    mutate(percent = n / sum(n)*100)

}

tab(groups = c("Family_status", "Education"),
    outcome.var = c("Age"),
    df = dat)



desc.table = data.m %>%
  pivot_longer(c("Gender","Education", "Family_status"),
               names_to = "key", values_to = "value") %>%
  group_by(key,value) %>%
  summarise (mean = round(mean(IRI_PT, na.rm = T),digits = 2),
             sd = round(sd(IRI_PT, na.rm = T),digits = 2),
             n = n()) %>%
  mutate(percent = n / sum(n)*100)

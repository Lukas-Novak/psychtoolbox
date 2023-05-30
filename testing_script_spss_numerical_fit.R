data.t <- haven::read_sav("./DATA panel - očesané (1).sav") %>%
  mutate(across(starts_with(c("GSES_dich_Q4_perc","age4","gender","education4","economical5")), ~as.factor(.)))

lasy.log.reg(independent.var = "ZSPSQ_sum", dependent.var = "GSES_dich_Q4_perc",covariates = c("age4","gender","education4","economical5"),data = data.t)

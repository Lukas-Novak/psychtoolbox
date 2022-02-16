
library(dplyr)
library(broom)
library(tidyverse)

set.seed(54854)
x = rnorm(500,1,1)
b0 = 1 # intercept chosen at your choice
b1 = 1 # coef chosen at your choice
h = function(x) 1+.4*x # h performs heteroscedasticity function (here

dat = tibble(
  eps = rnorm(300,0,h(x)),
  Age = as.numeric(rnorm(n = 300, mean = 35, sd = 10)),
  Work_years = as.numeric(rnorm(n = 300, mean = 50, sd = 15)),
  Education_prep = as.factor(rbinom(n = 300, size = 2, prob = .5)),
  Family_status = as.factor(case_when(Age > 20 ~ "Married",
                                      Age > 15 ~ "In relationship",
                                      Age < 15 ~ "Not in relationship")),
  Education = recode_factor(Education_prep,
                            "0" = "Basic schoool",
                            "1" = "High school",
                            "2" = "University"
                            )
)

tab = function(groups, outcome.var, df) {
  df %>% tidyr::drop_na(groups)
  df %>% tidyr::pivot_longer(groups,
                             names_to = "key",
                             values_to = "value") %>%
    dplyr::group_by(key,value) %>%
    dplyr::summarise(across(paste(outcome.var,sep = ","), list(mean=mean,
                                                              sd=sd)),
                     n = n()) %>%
    mutate(percent = n / sum(n)*100)

}

# tab(groups = c("Family_status", "Education"),
#     outcome.var = c("Age","Work_years"),
#     df = dat)


dat2 = dat %>% tidyr::pivot_longer(c("Family_status", "Education"),
                                   names_to = "key",
                                   values_to = "value")

output.var <- c("Age","Work_years","eps")
groups <- c("Family_status", "Education")

# homogeneity testing
hom.t = dat2 %>%
  group_by(key) %>%
  summarise(across(paste0(output.var), ~fligner.test(., value)$p.value)) %>%
  pivot_longer(paste0(output.var),
               names_to = "names_continous_var",
               values_to = "p_val_homo")

# normality testing
norm.test = dat2 %>%
  group_by(key) %>%
  summarise(across(paste0(output.var), ~shapiro.test(.) %>% tidy))  %>%
  pivot_longer(paste0(output.var),
               names_to = "names_continous_var",
               values_to = "p_val_shapiro")

# non-parametric testing and merging homogeneity and normality
gen.tab.krus = dat2 %>%
  group_by(key) %>%
  summarise(across(paste0(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
  pivot_longer(paste0(output.var),
               names_to = "names_continous_var",
               values_to = "stat") %>%
  full_join(hom.t) %>%
  full_join(norm.test) %>%
  as.matrix() %>%
  as_tibble() %>%
  # removing variables with non-significant Kruscal-Wallis test
  mutate(stat.p.value = as.numeric(stat.p.value)) %>%
  mutate(p_val_homo = as.numeric(p_val_homo)) %>%
  # filter(stat.p.value < 0.05) %>% # there is need to turn on this after testing !!!!
  mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
         non_homo_normal = p_val_homo < 0.05)


{ # there starts sequence
d =  dat2 %>%
  group_by(key) %>%
  group_by(key)

if (any(gen.tab.krus$homo_non_normal == TRUE)) {
  non.norm.var=filter(.data = gen.tab.krus, homo_non_normal == TRUE)
  dunn.test.results = dat2 %>%
    group_by(key) %>%
    group_by(key) %>%
    summarise(across(paste0(output.var), ~rstatix::dunn_test(. ~value, data = d))) %>%
    as.matrix() %>%
    as_tibble() %>%
    select(-key)  %>%
    pivot_longer(cols = contains(c(
      ".key",
      "..y.",
      ".group",
      ".n1",
      ".n2",
      ".statistic",
      ".p",
      ".p.adj",
      ".p.adj.signif")),
      names_to = "names",
      values_to = "val") %>%
    mutate(names = str_replace(names,
                               paste0(output.var,collapse = "|"),"")) %>%
    pivot_wider(names_from = names,
                values_from = val,
                values_fn = list) %>%
    unnest(cols = c(.key, ..y., .group1, .group2,
                    .n1, .n2, .statistic, .p, .p.adj,
                    .p.adj.signif)) %>%
    rename("names_continous_var" = "..y.",
           "key" = ".key") %>%
    mutate(merged_cols = paste0(key,",",names_continous_var)) %>%
    # there is need to filter results which are not referring to proper results of the Dunn test
    filter(str_detect(merged_cols,
                      paste0(non.norm.var$key,",",non.norm.var$names_continous_var,collapse = "|")) &
             !duplicated(.statistic) & !duplicated(.p))
  }

  # Homoscedasticity
  if (any(gen.tab.krus$non_homo_normal == TRUE)) {
    non.homo.var=filter(.data = gen.tab.krus, non_homo_normal == TRUE)
    games.howell.test.results = dat2
    # There is need to calculate Games-Howell test



} else {
  DS = "ps"
}
  }

DS



aa=filter(.data = gen.tab.krus, homo_non_normal == FALSE)


dat2 %>%
  group_by(key) %>%
  group_by(key) %>%
  summarise(across(paste0(output.var), ~rstatix::dunn_test(. ~value, data = d))) %>%
  as.matrix() %>%
  as_tibble() %>%
  rownames_to_column() %>%
  mutate(random_number = runif(nrow(.))) %>%
  mutate(random_number = as.character(random_number)) %>%
  select(-key)  %>%
  pivot_longer(cols = contains(c(
    "random_number",
    ".key",
    "..y.",
    ".group",
    ".n1",
    ".n2",
    ".statistic",
    ".p",
    ".p.adj",
    ".p.adj.signif")),
    names_to = "names",
    values_to = "val") %>%
  mutate(names = str_replace(names,
                             paste0(output.var,collapse = "|"),
                             "")) %>%
  pivot_wider(names_from = names,
              values_from = val) %>%
  unnest() %>%
  rename("names_continous_var" = "..y.",
         "key" = ".key") %>%
  filter(key == aa$key & names_continous_var == aa$names_continous_var) %>% view()



y <- 1
data.frame(x = 1:5) %>%
  {if (y==1) filter(., x>3) else filter(., x<3)} %>%
  tail(1)




#
# mand below, there is need to explore, where are significnat differences between socio-demographic groups
# fligner.test(dat$Age, dat$Family_status)$p.value
# #..............................................................................................
# stat.tab.1=
#   data.to.exper %>%
#   group_by(key) %>%
#   do(., kruskal.test(.$UWES~.$value) %>% tidy) %>%
#   mutate(UWES = "UWES") %>%
#   mutate(firstrowforvar=T) %>%
#   select(key, UWES, statistic, parameter, p.value, firstrowforvar)
#
# table1.categorical.both <- desc.table %>%
#   group_by(key) %>%
#   # we join on firstrowforvar to make sure we don't duplicate the tests
#   mutate(firstrowforvar=row_number()==1) %>%
#   left_join(., stat.tab.1, by=c("key", "firstrowforvar")) %>%
#   # this is gross, but we don't want to repeat the variable names in our table
#   ungroup() %>%
#   mutate(Variables = ifelse(firstrowforvar==T, as.character(key), NA)) %>%
#   select(Variables, value, n, percent,mean,sd, statistic, parameter, p.value)
# #..............................................................................................
# # there are significat differences in: Education, Family status and work_position
# #..............................................................................................
# # removing results of Kruscal-Wallis test
# table1.categorical.both = table1.categorical.both %>%
#   select(Variables,value,n, percent,mean,sd) %>%
#   mutate(mean = str_replace(mean, "NaN", NA_character_)) %>%
#   janitor::remove_empty(which = c("rows")) %>%
#   filter(!is.na(value) & !is.na(mean)) # removing missing values
# #.............................................................................
# #.............................................................................
# # post hoc testing
# #.............................................................................
# #.............................................................................
# # Games-Howell for Education
# oa.edu=rstatix::games_howell_test(UWES ~ Education, data = data, detailed = T)
# # Dunn test
# dunn.test(data$UWES, data$Education, list = T, altp = T, method = "bonferroni", kw=T, label = T, table = T)
# # selecting significnat results from Games-Howell
# edu.join=oa.edu %>% filter(p.adj < 0.05) %>%
#   select(group1,group2,df,statistic,p.adj) %>%
#   rename(value=group1)
# #  Merging into table 1
# table.continuous =
#   full_join(table1.categorical.both, edu.join)
# # merging ES into table 1
# # extracting significant comparistons from the Post-hoc test
# edu.es.g=oa.edu %>%
#   filter(p.adj < 0.05) %>%
#   select(group1,group2)
# # extracting ES from VDA
# ES.edu=multiVDA(x = data$UWES, g = data$Education, statistic = "VDA", digits = 2) %>%
#   as.data.frame() %>%
#   separate(col = pairs.Comparison, sep = " - ", into = c("group1","group2")) %>%
#   filter(group1 == edu.es.g$group1 & group2 == edu.es.g$group2) %>%
#   select(group1,group2,pairs.VDA) %>%
#   rename(value = group1)
# #  Merging into table 1
# table.continuous =
#   full_join(table.continuous, ES.edu)
# #................................................
# # Family status
# #................................................
# # Games-Howell for Family status
# uwes.fs=rstatix::games_howell_test(UWES ~ Family_status, data = data, detailed = T)
# # Dunn test
# # du=FSA::dunnTest(UWES~ Family_status,data = data, method="bonferroni")
# # du=du$res %>% as_tibble()
# # after post hoc testing, the resutls were not significant
# #................................................
# # Work_position
# #................................................
# # Dunn test
# dunn.test(data$UWES, data$Work_position, list = T, altp = T, method = "bonferroni", kw=T, label = T, table = T)
# # Games-Howell for Work_position
# uwes.wp=rstatix::games_howell_test(UWES ~ Work_position, data = data, detailed = T)
# # selecting significnat results from Games-Howell
# uwes.join=uwes.wp %>% filter(p.adj < 0.05) %>%
#   select(group1,group2,df,statistic,p.adj) %>%
#   rename(value=group1)
# #  Merging into table 1
# table.continuous.2 =
#   full_join(table1.categorical.both, uwes.join)
# # merging ES into table 1
# # extracting significant comparistons from the Post-hoc test
# wp.es.g=uwes.wp %>%
#   filter(p.adj < 0.05) %>%
#   select(group1,group2)
# # extracting ES from VDA
# ES.wp=multiVDA(x = data$UWES, g = data$Work_position, statistic = "VDA", digits = 2) %>%
#   as.data.frame() %>%
#   separate(col = pairs.Comparison, sep = " - ", into = c("group1","group2")) %>%
#   filter(group1 == wp.es.g$group1 & group2 == wp.es.g$group2) %>%
#   select(group1,group2,pairs.VDA) %>%
#   rename(value = group1)
# #  Merging into table 1
# table.continuous.2 =
#   full_join(table.continuous.2, ES.wp)
#
# # removing empty rows and NaNs
# two.var.tab=full_join(table.continuous, table.continuous.2) %>%
#   mutate(mean = str_replace(mean, "NaN", NA_character_)) %>%
#   janitor::remove_empty(which = c("rows"))
#
# # removing duplicites
# two.var.tab = two.var.tab %>%
#   group_by(Variables, value) %>%
#   mutate(duplicate = n()) %>% # count number of duplicite cases
#   mutate(to.rm = ifelse(duplicate > 1 & is.na(group2),TRUE,FALSE)) %>%
#   filter(to.rm == FALSE) %>%
#   ungroup() %>%
#   select(!c("to.rm","duplicate"))
#
# # sorting working status - extracting positions from original data frame
# sort.bypos = data$Work_position %>% levels()  # selecting order based on which we want to sort variable
#
# # arranging
# two.var.tab = two.var.tab %>%
#   arrange(factor(value, levels = sort.bypos)) %>%
#   # removing duplicate variable names
#   mutate(to.rm2 = ifelse(duplicated(Variables) & !is.na(Variables),TRUE,FALSE)) %>%
#   mutate(Variables = ifelse(to.rm2 == TRUE,NA_character_, Variables)) %>%
#   select(!c("to.rm2"))
# #................................
# # formatting table
# #...............................
# two.var.tab = two.var.tab %>%
#   mutate("UWES_T: M(SD)" = paste0("",mean," (",sd,")"),
#          "n(%)" = paste0("",n," ","(",round(percent,digits = 0),"%)"),
#          Gr.dif.UWES.total = paste0("",group2,": ","x2(",round(df,digits = 0),")","=",round(statistic,digits = 2),
#                                     "", format_p(p.adj,stars_only = T),", A=",pairs.VDA), # there are stars only to save space
#          Gr.dif.UWES.total = str_replace(Gr.dif.UWES.total, pattern = "(?<=^NA:)( .*)", replacement = ""),
#          Gr.dif.UWES.total = str_replace(Gr.dif.UWES.total, pattern = "^NA:", replacement = "")) %>%
#   select(Variables,value,"n(%)","UWES_T: M(SD)",Gr.dif.UWES.total)
# desc.table.ded = data %>%
#   pivot_longer(c("Gender","Education", "Family_status", "Religiosity","Work_position"),
#                names_to = "key", values_to = "value") %>%
#   group_by(key,value) %>%
#   summarise (mean = round(mean(UWES_D, na.rm = T),digits = 2),
#              sd = round(sd(UWES_D, na.rm = T),digits = 2),
#              n = n()) %>%
#   mutate(percent = n / sum(n)*100)
#
#
# data.to.exper.ded = data %>%
#   pivot_longer(c("Gender","Education","Family_status","Religiosity","Work_position"),
#                names_to = "key", values_to = "value") %>%
#   group_by(key,value) %>%
#   summarise (mean = round(mean(UWES_D, na.rm = T),digits = 2),
#              sd = round(sd(UWES_D, na.rm = T),digits = 2),
#              UWES_D = as.numeric(UWES_D),
#              n = n()) %>%
#   mutate(percent = n / sum(n)*100)
#
# #..............................................................................................
# # By the command below, there is need to explore, where are significnat differences between socio-demographic groups
# #..............................................................................................
# stat.tab.ded=
#   data.to.exper.ded %>%
#   group_by(key) %>%
#   do(., kruskal.test(.$UWES_D~.$value) %>% tidy) %>%
#   mutate(UWES_D = "UWES_D") %>%
#   mutate(firstrowforvar=T) %>%
#   select(key, UWES_D, statistic, parameter, p.value, firstrowforvar)
#
# table1.categorical.both.ded <- desc.table.ded %>%
#   group_by(key) %>%
#   # we join on firstrowforvar to make sure we don't duplicate the tests
#   mutate(firstrowforvar=row_number()==1) %>%
#   left_join(., stat.tab.ded, by=c("key", "firstrowforvar")) %>%
#   # this is gross, but we don't want to repeat the variable names in our table
#   ungroup() %>%
#   mutate(Variables = ifelse(firstrowforvar==T, as.character(key), NA)) %>%
#   select(Variables, value, n, percent,mean,sd, statistic, parameter, p.value)
# #..............................................................................................
# # there are significat differences in: Education, Family status and work_position
# #..............................................................................................
# # removing results of Kruscal-Wallis test
# table1.categorical.both.ded = table1.categorical.both.ded %>%
#   select(Variables,value,n, percent,mean,sd) %>%
#   mutate(mean = str_replace(mean, "NaN", NA_character_)) %>%
#   janitor::remove_empty(which = c("rows")) %>%
#   filter(!is.na(value) & !is.na(mean)) # removing missing values
# #.............................................................................
# #.............................................................................
# # post hoc testing
# #.............................................................................
# #.............................................................................
# # Games-Howell for Education
# oa.edu.ded=rstatix::games_howell_test(UWES_D ~ Education, data = data, detailed = T) # all ns
# # Dunn test
# dunn.test(data$UWES_D, data$Education, list = T, altp = T, method = "bonferroni", kw=T, label = T, table = T)
# # selecting significnat results from Games-Howell
# edu.join.ded=oa.edu.ded %>% filter(p.adj < 0.05) %>%
#   select(group1,group2,df,statistic,p.adj) %>%
#   rename(value=group1)
# #  Merging into table 1
# table.continuous.ded =
#   full_join(table1.categorical.both.ded, edu.join.ded)
# # merging ES into table 1
# # extracting significant comparistons from the Post-hoc test
# edu.es.g.ded=oa.edu.ded %>%
#   filter(p.adj < 0.05) %>%
#   select(group1,group2)
# # ..........................this code is not run because when result is not significnat code can not be runned
# # extracting ES from VDA
# # ES.edu.ded=multiVDA(x = data$UWES_D, g = data$Education, statistic = "VDA", digits = 2) %>%
# #   as.data.frame() %>%
# #   separate(col = pairs.Comparison, sep = " - ", into = c("group1","group2")) %>%
# #   filter(group1 == edu.es.g.ded$group1 & group2 == edu.es.g.ded$group2) %>%
# #   select(group1,group2,pairs.VDA) %>%
# #   rename(value = group1)
# #  Merging into table 1
# # table.continuous =
# #   full_join(table.continuous, ES.edu.ded)
# #.........................
# #................................................
# # Family status
# #................................................
# # Work_position
# #................................................
# # Dunn test
# dunn.test(data$UWES_D, data$Work_position, list = T, altp = T, method = "bonferroni", kw=T, label = T, table = T)
# # Games-Howell for Work_position
# UWES_D.wp=rstatix::games_howell_test(UWES_D ~ Work_position, data = data, detailed = T)
# # selecting significnat results from Games-Howell
# UWES_D.join=UWES_D.wp %>% filter(p.adj < 0.05) %>%
#   select(group1,group2,df,statistic,p.adj) %>%
#   rename(value=group1)
# #  Merging into table 1
# table.continuous.2.ded =
#   full_join(table1.categorical.both.ded, UWES_D.join)
# # merging ES into table 1
# # extracting significant comparistons from the Post-hoc test
# wp.es.g.ded=UWES_D.wp %>%
#   filter(p.adj < 0.05) %>%
#   select(group1,group2)
# # extracting ES from VDA
# ES.wp.ded=multiVDA(x = data$UWES_D, g = data$Work_position, statistic = "VDA", digits = 2) %>%
#   as.data.frame() %>%
#   separate(col = pairs.Comparison, sep = " - ", into = c("group1","group2")) %>%
#   filter(group1 == wp.es.g.ded$group1 & group2 == wp.es.g.ded$group2) %>%
#   select(group1,group2,pairs.VDA) %>%
#   rename(value = group1)
# #  Merging into table 1
# table.continuous.2.ded =
#   full_join(table.continuous.2.ded, ES.wp.ded)
#
# # removing empty rows and NaNs
# two.var.tab.ded=full_join(table.continuous.ded, table.continuous.2.ded) %>%
#   mutate(mean = str_replace(mean, "NaN", NA_character_)) %>%
#   janitor::remove_empty(which = c("rows"))
#
# # removing duplicites
# two.var.tab.ded = two.var.tab.ded %>%
#   group_by(Variables, value) %>%
#   mutate(duplicate = n()) %>% # count number of duplicite cases
#   mutate(to.rm = ifelse(duplicate > 1 & is.na(group2),TRUE,FALSE)) %>%
#   filter(to.rm == FALSE) %>%
#   ungroup() %>%
#   select(!c("to.rm","duplicate"))
#
# # sorting working status - extracting positions from original data frame
# sort.bypos = data$Work_position %>% levels()  # selecting order based on which we want to sort variable
#
# # arranging
# two.var.tab.ded = two.var.tab.ded %>%
#   arrange(factor(value, levels = sort.bypos)) %>%
#   # removing duplicate variable names
#   mutate(to.rm2 = ifelse(duplicated(Variables) & !is.na(Variables),TRUE,FALSE)) %>%
#   mutate(Variables = ifelse(to.rm2 == TRUE,NA_character_, Variables)) %>%
#   select(!c("to.rm2"))
# #................................
# # formatting table
# #...............................
# two.var.tab.ded = two.var.tab.ded %>%
#   mutate("UWES_D: M(SD)" = paste0("",mean," (",sd,")"),
#          "n(%)" = paste0("",n," ","(",round(percent,digits = 0),"%)"),
#          Gr.dif.UWES_D.total = paste0("",group2,": ","x2(",round(df,digits = 0),")","=",round(statistic,digits = 2),
#                                       "", format_p(p.adj,stars_only = T),", A=",pairs.VDA), # there are stars only to save space
#          Gr.dif.UWES_D.total = str_replace(Gr.dif.UWES_D.total, pattern = "(?<=^NA:)( .*)", replacement = ""),
#          Gr.dif.UWES_D.total = str_replace(Gr.dif.UWES_D.total, pattern = "^NA:", replacement = "")) %>%
#   select(Variables,value,"n(%)","UWES_D: M(SD)",Gr.dif.UWES_D.total)

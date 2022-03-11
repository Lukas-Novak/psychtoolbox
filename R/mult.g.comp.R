library(dplyr)
library(broom)
library(tidyverse)
library(insight)

set.seed(54854)
x = rnorm(500,1,1)
b0 = 1 # intercept chosen at your choice
b1 = 1 # coef chosen at your choice
h = function(x) 1+.4*x # h performs heteroscedasticity function (here

dat = tibble(
  eps = rnorm(300,0,h(x)),
  Gender_prep = as.factor(rbinom(300, size = 1, prob = .30)),
  Age = as.numeric(rnorm(n = 300, mean = 35, sd = 10)),
  Work_years = as.numeric(rnorm(n = 300, mean = 50, sd = 15)),
  Education_prep = as.factor(rbinom(n = 300, size = 2, prob = .5)),
  Family_status = as.factor(case_when(Age > 20 ~ "Married",
                                      Age > 15 ~ "In relationship",
                                      Age < 15 ~ "Not in relationship")),
  Education = recode_factor(Education_prep,
                            "0" = "Basic schoool",
                            "1" = "High school",
                            "2" = "University"),
  Gender = recode_factor(Gender_prep,
                         "0"="Male",
                         "1" = "Female")
)

# b = desc.tab(groups = c("Family_status", "Education","Gender"),
#         outcome.var = c("Age","Work_years","eps"),
#         df = dat)

pokus.data = pokus.data %>%
  mutate(OASIS = rowSums(across(starts_with("OASIS_"))))

b = desc.tab(groups = c("Region", "Gender","Family_status","Education"),
             outcome.var = c("OASIS"),
             df = pokus.data)
#
# b

mult.g.comp = function(df,outcome.var,groups) {
  {
    desc.tab = function(groups, outcome.var, df) {
      factors.dat = df %>% select(where(is.factor)) %>% names()
      df %>% tidyr::drop_na(groups)
      df %>% dplyr::mutate(across(paste0(factors.dat), ~paste(as.numeric(.), .))) %>%
        tidyr::pivot_longer(groups,
                            names_to = "key",
                            values_to = "value") %>%
        dplyr::group_by(key,value) %>%
        dplyr::summarise(across(paste(outcome.var,sep = ","), list(mean=mean,
                                                                   sd=sd)),
                         n = n()) %>%
        mutate(percent = n / sum(n)*100) %>%
        ungroup()
    }

    b = desc.tab(groups, outcome.var, df)

    dat = df %>%
      select(ends_with(vctrs::vec_c(groups,outcome.var)))

    nam.ex = b %>%
      select(ends_with(c("_mean","_sd"))) %>%
      names()

    outcome.var = b %>%
      select(ends_with(c("_mean","_sd"))) %>%
      names() %>%
      str_replace_all(., "_mean", "") %>%
      str_replace_all(., "_sd", "") %>%
      unique()

    b = b %>%
      ungroup() %>%
      mutate(across(paste(nam.ex), ~as.numeric(.))) %>%
      mutate_if(is.numeric,round,2) %>%
      mutate(id = row_number()) %>%
      pivot_longer(names_to = "names", values_to = "val", all_of(nam.ex)) %>%
      mutate(variable = str_extract(names, paste0(outcome.var, collapse = "|"))) %>%
      group_by(id, variable) %>%
      mutate("M(sd)" = paste0("(", paste0(val, collapse = ';'), ")")) %>%
      ungroup() %>%
      select(!c(val,names)) %>%
      pivot_wider(names_from = variable, values_from = `M(sd)`, names_sep = "key", values_fn = list) %>%
      unnest(all_of(outcome.var)) %>%
      group_by(id) %>%
      mutate(dups = duplicated(id)) %>%
      filter(dups == FALSE) %>%
      ungroup() %>%
      select(!c(id,dups))
  }

  # odstaranit duplikáty v key prostřednictím funkce duplicate

  # selecting all groups and outome variables
  factors.dat = dat %>% select(where(is.factor)) %>% names()
  output.var = dat %>% select(where(is.numeric)) %>% names()

  dat.factors = dat %>%
    mutate(across(paste0(factors.dat), ~paste(as.numeric(.), .)))

  ## comparison for 2 groups
  ##.....................................................................................
  # if(dat %>% select_if(~ nlevels(.) == 2) %>% length() >= 1) {
  dat2.two.groups = dat %>%
    select_if(~ nlevels(.) == 2 | is.numeric(.)) %>%
    mutate(across(c(where(is.factor)), ~as.factor(paste(as.numeric(.), .)))) %>%
    tidyr::pivot_longer(cols = c(where(is.factor)),
                        names_to = "key",
                        values_to = "value")

  #   # homogeneity testing of the 2 groups
  hom.t.2groups = dat2.two.groups %>%
    group_by(key) %>%
    summarise(across(paste0(output.var), ~fligner.test(., value)$p.value)) %>%
    pivot_longer(paste0(output.var),
                 names_to = "names_continous_var",
                 values_to = "p_val_homo")

  # normality testing
  norm.test.2groups = dat2.two.groups %>%
    group_by(key) %>%
    summarise(across(paste0(output.var), ~shapiro.test(.) %>% tidy))  %>%
    pivot_longer(paste0(output.var),
                 names_to = "names_continous_var",
                 values_to = "p_val_shapiro")

  # non-parametric testing and merging homogeneity and normality
  gen.tab.krus.2groups = dat2.two.groups %>%
    group_by(key) %>%
    summarise(across(paste0(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
    pivot_longer(paste0(output.var),
                 names_to = "names_continous_var",
                 values_to = "stat") %>%
    full_join(hom.t.2groups) %>%
    full_join(norm.test.2groups) %>%
    as.matrix() %>%
    as_tibble() %>%
    # removing variables with non-significant Kruscal-Wallis test
    mutate(stat.p.value = as.numeric(stat.p.value)) %>%
    mutate(p_val_homo = as.numeric(p_val_homo)) %>%
    mutate(p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value)) %>%
    filter(stat.p.value < 0.05) %>%
    mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
           non_homo_normal = p_val_homo < 0.05)

  # there starts sequence
  d.2groups =  dat2.two.groups %>%
    group_by(key) %>%
    group_by(key)

  # Non-normality
  if (any(gen.tab.krus.2groups$homo_non_normal == TRUE)) {
    non.norm.var.wilc=filter(.data = gen.tab.krus.2groups, homo_non_normal == TRUE)
    wilcox.test.results = dat2.two.groups %>%
      group_by(key) %>%
      group_by(key) %>%
      summarise(across(paste0(output.var), ~rstatix::wilcox_test(. ~ value, data = d.2groups, p.adjust.method = "bonferroni"))) %>%
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
      mutate(merged_cols = paste0(key,",",names_continous_var),
             .p.adj = as.numeric(.p.adj),
             .p.adj = format_p(.p.adj),
             .statistic = as.numeric(.statistic),
             across(ends_with(".statistic"), ~round(., 2))) %>%
      # there is need to filter results which are not referring to proper results of the Dunn test
      filter(str_detect(merged_cols,
                        paste0(non.norm.var.wilc$key,",",non.norm.var.wilc$names_continous_var,collapse = "|")) &
               !duplicated(.statistic) & !duplicated(.p)) %>%
      mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                        str_extract(.group2, "^.{1}"),", ",
                                        "W = ", .statistic,", ", .p.adj))

    # Creating aggregated results to join into descriptive table
    aggregated.results.wilcox = wilcox.test.results %>%
      select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
      mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
      group_by(merged_cols,key,names_continous_var) %>%
      summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
      ungroup %>%
      select(key, `Group comparison`,names_continous_var) %>%
      mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
      pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
  }

  # Homoscedasticity
  if (any(gen.tab.krus.2groups$non_homo_normal == TRUE)) {
    non.homo.var.welch=filter(.data = gen.tab.krus.2groups, non_homo_normal == TRUE)
    Welch.test.results = dat2.two.groups %>%
      group_by(key) %>%
      group_by(key) %>%
      # There is need to calculate Games-Howell test
      summarise(across(paste0(output.var), ~rstatix::t_test(. ~value, var.equal = FALSE, data = d.2groups,
                                                            p.adjust.method = "bonferroni"))) %>%
      as.matrix() %>%
      as_tibble() %>%
      select(-key)  %>%
      pivot_longer(cols = contains(c(
        ".key",
        "..y.",
        ".group",
        ".n1",
        ".n2",
        ".conf.",
        ".se",
        ".statistic",
        ".df",
        ".p",
        ".method")),
        names_to = "names",
        values_to = "val") %>%
      mutate(names = str_replace(names,
                                 paste0(output.var,collapse = "|"),"")) %>%
      pivot_wider(names_from = names,
                  values_from = val,
                  values_fn = list) %>%
      unnest(cols = c(.key, ..y., .group1, .group2, .n1, .n2, .statistic, .df, .p)) %>%
      rename("names_continous_var" = "..y.",
             "key" = ".key") %>%
      mutate(merged_cols = paste0(key,",",names_continous_var),
             .df = as.numeric(.df),
             .p = format_p(.p),
             .statistic = as.numeric(.statistic),
             across(ends_with(c(".statistic",".df")), ~round(., 2))) %>%
      # there is need to filter results which are not referring to proper results of the Dunn test
      filter(str_detect(merged_cols,
                        paste0(non.homo.var.welch$key,",",non.homo.var.welch$names_continous_var,collapse = "|")) &
               !duplicated(.statistic) & !duplicated(.p)) %>%
      mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                        str_extract(.group2, "^.{1}"),", ",
                                        "t(",.df,")"," = ",.statistic,", ", .p))

    # Creating aggregated results to join into descriptive table
    aggregated.results.welch = Welch.test.results %>%
      select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
      mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
      group_by(merged_cols,key,names_continous_var) %>%
      summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
      ungroup %>%
      select(key, `Group comparison`,names_continous_var) %>%
      mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
      pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
  }
  {
    ##.....................................................................................
    # homogeneity testing of the more then 2 groups
    dat2 = dat %>%
      select_if(~ nlevels(.) > 2 | is.numeric(.)) %>%
      mutate(across(c(where(is.factor)), ~as.factor(paste(as.numeric(.), .)))) %>%
      tidyr::pivot_longer(cols = c(where(is.factor)),
                          names_to = "key",
                          values_to = "value")
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
      mutate(p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value)) %>%
      filter(stat.p.value < 0.05) %>% # there is need to turn on this after testing !!!!
      mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
             non_homo_normal = p_val_homo < 0.05)

    # there starts sequence
    d =  dat2 %>%
      group_by(key) %>%
      group_by(key)

    # Non-normality
    if (any(gen.tab.krus$homo_non_normal == TRUE)) {
      # this is for the further development with normal and homoscedastics data
      #.......................................................................
      # gen.tab.krus = gen.tab.krus %>%
      #   mutate(normal_dist = ifelse(homo_non_normal == FALSE & non_homo_normal == FALSE, TRUE, FALSE))
      # non.norm.var=filter(.data = gen.tab.krus, homo_non_normal == TRUE | normal_dist == TRUE)
      #.......................................................................
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
        mutate(merged_cols = paste0(key,",",names_continous_var),
               .p.adj = as.numeric(.p.adj),
               .p.adj = format_p(.p.adj),
               .statistic = as.numeric(.statistic),
               across(ends_with(".statistic"), ~round(., 2))) %>%
        # there is need to filter results which are not referring to proper results of the Dunn test
        filter(str_detect(merged_cols,
                          paste0(non.norm.var$key,",",non.norm.var$names_continous_var,collapse = "|")) &
                 !duplicated(.statistic) & !duplicated(.p)) %>%
        mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                          str_extract(.group2, "^.{1}"),", ",
                                          "z = ", .statistic,", ", .p.adj))

      # Creating aggregated results to join into descriptive table
      aggregated.results.dunn = dunn.test.results %>%
        select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
        mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
        group_by(merged_cols,key,names_continous_var) %>%
        summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
        ungroup %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    # estimation of the effect size from R package - Rcompanion
    #..................................................
    # Matrix = outer(A, B, FUN = "-") # A = X-variable, B = Y-variable
    # Diff = ifelse(Matrix == 0, 0.5, Matrix > 0)
    # VDA = signif(mean(Diff), digits = 2)
    #..................................................

    # Homoscedasticity
    if (any(gen.tab.krus$non_homo_normal == TRUE)) {
      non.homo.var=filter(.data = gen.tab.krus, non_homo_normal == TRUE) # this makes troubles | non_homo_normal == TRUE
      games.howell.test.results = dat2 %>%
        group_by(key) %>%
        group_by(key) %>%
        # There is need to calculate Games-Howell test
        summarise(across(paste0(output.var), ~rstatix::games_howell_test(. ~value, data = d, detailed = T))) %>%
        as.matrix() %>%
        as_tibble() %>%
        select(-key)  %>%
        pivot_longer(cols = contains(c(
          ".key",
          "..y.",
          ".group",
          ".n1",
          ".n2",
          ".estimate",
          ".conf.",
          ".se",
          ".statistic",
          ".df",
          ".p.",
          ".method")),
          names_to = "names",
          values_to = "val") %>%
        mutate(names = str_replace(names,
                                   paste0(output.var,collapse = "|"),"")) %>%
        pivot_wider(names_from = names,
                    values_from = val,
                    values_fn = list) %>%
        unnest(cols = c(.key, ..y., .group1, .group2, .n1, .n2, .estimate, .conf.low,
                        .conf.high, .se, .statistic, .df, .p.adj, .p.adj.signif,
                        .method)) %>%
        rename("names_continous_var" = "..y.",
               "key" = ".key") %>%
        mutate(merged_cols = paste0(key,",",names_continous_var),
               .p.adj = as.numeric(.p.adj),
               .df = as.numeric(.df),
               .p.adj = format_p(.p.adj),
               .statistic = as.numeric(.statistic),
               across(ends_with(c(".statistic",".df")), ~round(., 2))) %>%
        # there is need to filter results which are not referring to proper results of the Dunn test
        filter(str_detect(merged_cols,
                          paste0(non.homo.var$key,",",non.homo.var$names_continous_var,collapse = "|")) &
                 !duplicated(.statistic) & !duplicated(.p.adj)) %>%
        mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                          str_extract(.group2, "^.{1}"),", ",
                                          "t(",.df,")"," = ",.statistic,", ", .p.adj))

      # Creating aggregated results to join into descriptive table
      aggregated.results.games.howell = games.howell.test.results %>%
        select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
        mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
        group_by(merged_cols,key,names_continous_var) %>%
        summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
        ungroup %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    if(exists("aggregated.results.games.howell")) {
      psd = aggregated.results.games.howell %>%
        full_join(b)
    }

    if(exists("aggregated.results.dunn")) {
      psd = aggregated.results.dunn %>%
        full_join(b)
    }
    psd = psd %>%
      mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
      mutate_all(~replace(., is.na(.), "")) %>%
      mutate(key = ifelse(duplicated(key),"", key)) %>%
      mutate(n = as.numeric(n)) %>%
      mutate(across(ends_with(c("_mean","_sd","percent")), ~as.numeric(.)))
  }

  two.level.factors = dat %>%
    select_if(~ nlevels(.) == 2) %>% names()

  if(exists("aggregated.results.wilcox")) {
    comb.wilcox.pre = aggregated.results.wilcox %>%
      full_join(b) %>%
      filter(str_detect(key, paste0(two.level.factors,collapse = "|")))
  }

  if(exists("comb.wilcox.pre")) {
    comb.wilcox.pre.fin = comb.wilcox.pre %>%
      full_join(psd) %>%
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2)
  }

  if(exists("aggregated.results.welch")) {
    comb.welch.pre = aggregated.results.welch %>%
      full_join(b) %>%
      filter(str_detect(key, paste0(two.level.factors,collapse = "|")))
  }

  if(exists("comb.wilcox.pre")) {
    comb.welch.pre = comb.welch.pre %>%
      full_join(comb.wilcox.pre) %>%
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2)
  }

  if(exists("comb.wilcox.pre.fin")) {
    comb.wilcox.pre.fin = comb.wilcox.pre.fin %>%
      full_join(comb.welch.pre)
  }

  # there is problem causing absence of statistical results report in the table
  # if(exists("comb.wilcox.pre.fin")) {
  #   comb.tab.finished = comb.wilcox.pre.fin %>%
  #     relocate("key","value","n","percent")
  #   return(comb.tab.finished)
  # }
  # else {return(b)
  # }
  return(psd)
}

pokus.data = pokus.data %>%
  mutate(OASIS = rowSums(across(starts_with("OASIS_"))))

dd=mult.g.comp(groups = c("Region", "Gender","Family_status","Education"),
               outcome.var = c("OASIS"),
               df = pokus.data)

# dd

qqq = mult.g.comp(groups = c("Family_status", "Education","Gender"),
                  outcome.var = c("Age","Work_years","eps"),
                  df = dat)

qqq


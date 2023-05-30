# Documentation
#' Robust multi-group comparison
#'
#' @description This function allows to compare multiple groups in
#' multiple outcome variables with violated parametric assumptions.
#'
#' @param df data frame or tibble object
#' @param outcome.var continuous variable/s
#' @param groups grouping variable/s
#'
#' @return data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @keywords multiple-groups testing, Games-Howell test, Dunn-test
#'
#' @details
#' Currently, this function does not report effect size from post-hoc tests.
#'
#' ## Two group comparison
#' If there is less than three groups, the Welch test or the Wilcoxon
#' test depending on data distribution.
#'
#' ## Three and more groups comparison
#' If more than two groups are present in data, the Dunn test or Games-Howell
#' test is performed.
#'
#' @references
#' Welch, B. L. (1947). "The generalization of "Student's"
#' problem when several different population variances are involved".
#' Biometrika. 34 (1--2): 28--35.
#'
#' Wilcoxon, F., Individual Comparisons by Ranking Methods,
#' Biometrics Bulletin, Vol. 1, 1945, pp. 80--83. DOI:10.2307/3001968
#'
#' Dunn, O. J. (1961) Multiple comparisons among means.
#' Journal of the American Statistical Association. 56, 52--64.
#'
#' Games, P. A., Keselman, H. J., & Clinch, J. J.
#' Tests for homogeneity of variance in factorial designs.
#'  Psychological Bulletin, 86, 978--984
#'
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#' @importFrom broom tidy
#' @importFrom dplyr coalesce
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise_all
#' @importFrom dplyr distinct
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom stats t.test
#' @importFrom dplyr tibble
#' @importFrom stats fligner.test
#' @importFrom stats wilcox.test
#' @importFrom expss where
#' @importFrom dplyr across
#' @importFrom stringr str_replace
#' @importFrom tidyselect ends_with
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom stringr str_replace_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr across
#' @importFrom dplyr mutate_if
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom dplyr all_of
#' @importFrom stringr str_extract
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom dplyr select_if
#' @importFrom dplyr summarise
#' @importFrom dplyr full_join
#' @importFrom tidyr as_tibble
#' @importFrom dplyr contains
#' @importFrom dplyr rename
#' @importFrom insight format_p
#' @importFrom stringr str_detect
#' @importFrom dplyr starts_with
#' @importFrom dplyr mutate_all
#' @importFrom dplyr relocate
#' @importFrom vctrs vec_c
#'
#'
#' @examples
#' # data loading
#' tab.1=mult.g.comp(df = paq.validation.study, outcome.var = c("PAQ","G_DIF","G_DDF","G_EOT"),
#' groups = c("economical_status",
#'           "Gender",
#'           "education",
#'           "family_status"))
#' # printing the output
#' print(tab.1)
#' @export
#......................................................

mult.g.comp = function(df,outcome.var,groups) {
  {
    desc.tab = function(groups, outcome.var, df) {
      factors.dat = df %>% select(where(is.factor)) %>% names()
      df %>% drop_na(groups)
      df %>% mutate(across(paste0(factors.dat), ~paste(as.numeric(.), .))) %>%
        pivot_longer(groups,
                            names_to = "key",
                            values_to = "value") %>%
        group_by(key,value) %>%
        summarise(across(paste(outcome.var,sep = ","), list(mean=mean,
                                                                   sd=sd), na.rm = TRUE),
                         n = n()) %>%
        mutate(percent = n / sum(n)*100) %>%
        ungroup()
    }


    b = desc.tab(groups, outcome.var, df) %>%
      mutate(value = str_replace(value, "NA NA", "Missing"))

    if(sum(b$n <= 1) >= 1){
      stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
    }

    # filtering of NAs in data - this is causing problems when merging
    dat =df %>%
      select(ends_with(vctrs::vec_c(groups,outcome.var)))
    #filter(!if_any(c(paste(groups,sep = "|")), is.na))

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
    pivot_longer(cols = c(where(is.factor)),
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
        ".p")),
        names_to = "names",
        values_to = "val") %>%
      mutate(names = str_replace(names,
                                 paste0(output.var,collapse = "|"),"")) %>%
      pivot_wider(names_from = names,
                  values_from = val,
                  values_fn = list) %>%
      unnest(cols = c(".key","..y.", ".group1", ".group2",
                      ".n1",".n2",".statistic",".p")) %>%
      rename("names_continous_var" = "..y.",
             "key" = ".key") %>%
      mutate(merged_cols = paste0(key,",",names_continous_var),
             .p = as.numeric(.p),
             .p = format_p(.p),
             .statistic = as.numeric(.statistic),
             across(ends_with(".statistic"), ~round(., 2))) %>%
      # there is need to filter results which are not referring to proper results of the Dunn test
      filter(str_detect(merged_cols,
                        paste0(non.norm.var.wilc$key,",",non.norm.var.wilc$names_continous_var,collapse = "|"))) %>%
      distinct(.statistic, .p, .keep_all = T) %>%  # filtering duplicated values across the two columns
      mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                        str_extract(.group2, "^.{1}"),", ",
                                        "W = ", .statistic,", ", .p))

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
                        paste0(non.homo.var.welch$key,",",non.homo.var.welch$names_continous_var,collapse = "|"))) %>%
      distinct(.statistic, .p, .keep_all = T) %>%  # filtering duplicated values across the two columns
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
      pivot_longer(cols = c(where(is.factor)),
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
        summarise(across(paste0(output.var), ~rstatix::dunn_test(. ~value, data = d, detailed = T,
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
               .p.adj = as.numeric(.p.adj)) %>%
        filter(.p.adj < 0.05) %>%
        mutate(
          .p.adj = format_p(.p.adj),
          .statistic = as.numeric(.statistic),
          across(ends_with(".statistic"), ~round(., 2))) %>%
        # there is need to filter results which are not referring to proper results of the Dunn test
        filter(str_detect(merged_cols,
                          paste0(non.norm.var$key,",",non.norm.var$names_continous_var,collapse = "|"))) %>%
        distinct(.statistic, .p, .keep_all = T) %>%  # filtering duplicated values across the two columns
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
        mutate(.p.adj = as.numeric(.p.adj)) %>%
        filter(.p.adj < 0.05) %>%
        mutate(merged_cols = paste0(key,",",names_continous_var),
               .df = as.numeric(.df),
               .statistic = as.numeric(.statistic),
               .estimate = as.numeric(.estimate),
               across(ends_with(c(".statistic",".df")), ~round(., 2))) %>%
        # there is need to filter results which are not referring to proper results of the Dunn test
        filter(str_detect(merged_cols,
                          paste0(non.homo.var$key,",",non.homo.var$names_continous_var,collapse = "|"))) %>%
        distinct(.estimate, .statistic, .keep_all = T) %>%  # filtering duplicated values across the two columns
        mutate(.p.adj = format_p(.p.adj)) %>%
        mutate(.estimate = round(.estimate,digits = 2)) %>%
        mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ",
                                          str_extract(.group2, "^.{1}"),", ",
                                          "t(",.df,")"," = ",.estimate,", ", .p.adj))

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

    # https://stackoverflow.com/a/45515491/14041287
    coalesce_by_column <- function(df) {
      return(coalesce(!!! as.list(df)))
    }

    if(exists("aggregated.results.games.howell") & exists("aggregated.results.dunn")) {
      psd = aggregated.results.dunn %>%
        full_join(aggregated.results.games.howell) %>%
        group_by(key) %>%
        summarise_all(coalesce_by_column) %>%
        full_join(b)
    } else if (exists("aggregated.results.games.howell") & !exists("aggregated.results.dunn")) {
      psd = aggregated.results.games.howell %>%
        full_join(b)
    } else if (!exists("aggregated.results.games.howell") & exists("aggregated.results.dunn")) {
      psd = aggregated.results.dunn %>%
        full_join(b)
    } else if (!exists("aggregated.results.games.howell") & !exists("aggregated.results.dunn")) {
      psd = b
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
      group_by(key) %>% # this group by has to be there because otherwise unwanted values might be filtered out
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      ungroup() %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(dups = duplicated(value))%>%
      filter(dups == FALSE) %>%
      select(!dups) %>%
      mutate_all(~(replace(., is.na(.), "")))
  }

  if(exists("aggregated.results.welch")) {
    comb.welch.pre = aggregated.results.welch %>%
      full_join(b) %>%
      filter(str_detect(key, paste0(two.level.factors,collapse = "|")))
  }

  if(exists("aggregated.results.welch") & !exists("aggregated.results.wilcox")) {
    solo.welsh.fin = comb.welch.pre %>%
      full_join(psd) %>%
      group_by(key) %>%
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      ungroup() %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(dups = duplicated(value))%>%
      filter(dups == FALSE) %>%
      select(!dups) %>%
      mutate_all(~(replace(., is.na(.), "")))
  }

  if(exists("comb.wilcox.pre") & exists("comb.welch.pre")) {
    comb.welch.fin = comb.welch.pre %>%
      full_join(comb.wilcox.pre) %>%
      full_join(psd) %>%
      group_by(key) %>%
      filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
      ungroup() %>%
      mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
      mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(dups = duplicated(value))%>%
      filter(dups == FALSE) %>%
      select(!dups) %>%
      mutate_all(~(replace(., is.na(.), "")))
  }
  #.......................................
  if(exists("comb.wilcox.pre.fin") & exists("comb.welch.fin")) {
    comb.wilcox.pre.fin = comb.wilcox.pre.fin %>%
      full_join(comb.welch.fin) %>%
      mutate(across(contains("Group difference"), ~ifelse(is.na(.), "", .)))

    sort.names = comb.wilcox.pre.fin %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()

    comb.wilcox.pre.fin = comb.wilcox.pre.fin %>%
      relocate(all_of(sort.names)) %>%
      return(comb.welch.fin)
  } else
  {
    if(exists("comb.welch.fin")) {
      comb.welch.fin = comb.welch.fin %>%
        full_join(psd) %>%
        group_by(key) %>%
        filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
        ungroup() %>%
        mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
        mutate(across(ends_with("key"), ~ifelse(duplicated(.), "", .))) %>%
        mutate_if(is.numeric, round, 2) %>%
        mutate(dups = duplicated(value))%>%
        filter(dups == FALSE) %>%
        select(!dups) %>%
        mutate_all(~(replace(., is.na(.), "")))
    }

    if(exists("comb.wilcox.pre.fin")) {
      sort.names = comb.wilcox.pre.fin %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()
      wilcox.to.print =  comb.wilcox.pre.fin %>%
        relocate(all_of(sort.names))
      return(wilcox.to.print)
    }

    if(exists("solo.welsh.fin")) {
      sort.names = solo.welsh.fin %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()
      solo.welsh.to.print =  solo.welsh.fin %>%
        relocate(all_of(sort.names))
      return(solo.welsh.to.print)
    }
    # if(!exists("comb.welch.fin") & !exists("comb.wilcox.pre.fin")) {
    #   sort.names = comb.welch.fin %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()
    #   comb.welch.fin = psd %>%
    #     relocate(all_of(sort.names))
    #
    # }

    if (exists("comb.welch.fin")) {
      sort.names = comb.welch.fin %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()
      comb.welch.fin = comb.welch.fin %>%
        relocate(all_of(sort.names)) %>%
        return(comb.welch.fin)
    }

    if (exists("aggregated.results.games.howell") & !all(c("aggregated.results.wilcox","comb.wilcox.pre",
                                                           "aggregated.results.welch","comb.welch.pre",
                                                           "comb.wilcox.pre.fin","comb.welch.fin","solo.welsh.fin") %in% ls()))
    {
      sort.names = psd %>% select(ends_with(c("key","value","n","percent","Group difference"))) %>% names()
      psd = psd %>%
        relocate(all_of(sort.names)) %>%
        return(psd)
    }
  }
}




#
# library(dplyr)
# library(broom)
# library(tidyverse)
# library(insight)
#
# set.seed(54854)
# x = rnorm(500,1,1)
# b0 = 1 # intercept chosen at your choice
# b1 = 1 # coef chosen at your choice
# h = function(x) 1+.4*x # h performs heteroscedasticity function (here
#
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
#
#
# qqq = mult.g.comp(groups = c("Family_status", "Education","Gender"),
#                   outcome.var = c("Age","Work_years","eps"),
#                   df = dat)
#
# qqq %>% view()
#

# Documentation
#' Robust multi-group comparison
#'
#' @description This function allows to compare multiple groups in
#' multiple outcome variables with violated parametric assumptions.
#'
#' @param df data frame or tibble object
#' @param outcome.var continuous variable/s
#' @param groups grouping variable/s
#' @param desc_only print only descriptive statistics, default is FALSE
#' @param short_results prints only significance stars without numerical results, default is TRUE
#' @param remove_missings remove missing values from a table, default is FALSE
#' @param percent_decimals number of decimals used to round percenages, default is 2
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
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarize
# #' @importFrom dplyr reframe
#' @importFrom stringr str_extract
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom dplyr select_if
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom tidyr as_tibble
#' @importFrom dplyr contains
#' @importFrom dplyr rename
#' @importFrom insight format_p
#' @importFrom stringr str_detect
#' @importFrom dplyr starts_with
#' @importFrom dplyr mutate_all
#' @importFrom dplyr add_row
#' @importFrom dplyr relocate
#' @importFrom dplyr if_else
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

mult.g.comp = function(df,outcome.var,groups, desc_only = FALSE, short_results = TRUE, remove_missings = FALSE, percent_decimals = 2) {

  # ----------- Helper Functions (Unchanged) -----------
  desc.tab = function(groups, outcome.var, df) {
    factors.dat = df %>% select(where(is.factor)) %>% names()
    df %>% drop_na(groups)
    df %>% mutate(across(paste0(factors.dat), ~paste(as.numeric(.), .))) %>%
      pivot_longer(groups,
                   names_to = "key",
                   values_to = "value") %>%
      group_by(key,value) %>%
      summarise(across(all_of(outcome.var), list(mean=mean,
                                                 sd=sd), na.rm = TRUE),
                n = n()) %>%
      mutate(percent =  as.character(round(n / sum(n)*100, digits = percent_decimals))) %>%
      ungroup() %>%
      mutate_all(~str_replace_all(., "NA NA|NaN|NA", NA_character_))
  }

  remove_na_in_brackets <- function(x, var) {
    x = x %>%
      mutate(
        across(all_of(var), ~str_replace_all(., "\\(NA,\\)", "")),
        across(all_of(var), ~str_replace_all(., "NA+\\,", "")),
        across(all_of(var), ~str_replace_all(., "\\(NA\\)", "")),
        across(all_of(var), ~str_replace_all(., "NA", ""))
      )
  }

  longer_tab <- function(x) {
    # testing whether df contains results of the statistical tests
    if (summarize(x,
                  contains_stat_tets_results = any(!is.na(across(contains("Group difference")))))$contains_stat_tets_results) {


      # removing missing if desired
      if (remove_missings == TRUE) {
        x = x %>%
          filter(str_detect(value, "Missing", negate = TRUE))
      }

      x = x %>%
        remove_na_in_brackets(var = outcome.var) %>%
        mutate(value = replace_na(value, "Missing")) %>%
        group_by(key) %>% # this group by has to be there because otherwise unwanted values might be filtered out
        filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
        ungroup() %>%
        mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
        mutate_if(is.numeric, round, 2) %>%
        mutate_all(~(replace(., is.na(.), ""))) %>%
        mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
        group_by(key) %>%
        group_modify(~add_row(., .before = 1)) %>%
        ungroup() %>%
        mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
        mutate(value = if_else(is.na(value), key, value)) %>%
        mutate_all(~replace(., is.na(.), "")) %>%
        mutate(`n(%)` = paste0(as.numeric(n), "(",percent,")")) %>%
        mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
        select(-c("key","n","percent")) %>%
        rename_with(~paste0(outcome.var," M(SD)"), ends_with(outcome.var)) %>%
        rename("variable" = "value",
               "n(%)" = `n(%)`)
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
        rename_with(~paste0(outcome.var," M(SD)"), ends_with(outcome.var)) %>%
        rename("variable" = "value",
               "n(%)" = `n(%)`)
    }
  }

  removing_nested_prentecies <- function(x) {
    success <- FALSE
    while (!success) {
      x = x %>%
        mutate_all(~stringr::str_remove_all(., "\\)(?=.*\\))")) %>%
        mutate_all(~stringr::str_replace(., "\\((.*)\\(", "(\\1"))
      # check for success
      success <- x %>% summarise(across(everything(), ~stringr::str_count(., "\\(") >= 2)) %>% any(isTRUE(.),na.rm = T) == FALSE
    }
    return(x)
  }
  # ----------- End Helper Functions -----------


  # ----------- 1. Initial Data Preparation -----------
  descriptive_stats_raw = desc.tab(groups, outcome.var, df) %>%
    mutate(value = str_replace(value, "NA NA", "Missing"))

  if(sum(descriptive_stats_raw$n <= 1) >= 1 & desc_only == FALSE){
    stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
  }

  analysis_data = df %>%
    select(c(vctrs::vec_c(groups, outcome.var)))

  mean_sd_colnames = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names()

  # Re-assign outcome.var to only include variables present in the descriptive table
  outcome.var = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names() %>%
    str_replace_all(., "_mean", "") %>%
    str_replace_all(., "_sd", "") %>%
    unique()

  # Reshape descriptive stats to have "M (SD)" format
  descriptive_stats_wide = descriptive_stats_raw %>%
    ungroup() %>%
    mutate(across(all_of(mean_sd_colnames), ~as.numeric(.))) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "names", values_to = "val", all_of(mean_sd_colnames)) %>%
    mutate(variable = str_extract(names, paste0(outcome.var, collapse = "|"))) %>%
    mutate(val = ifelse(str_detect(names, "_sd"), paste0(" (",val,")"), val)) %>%
    group_by(id, variable) %>%
    mutate("M(sd)" = paste0(val, collapse = '')) %>%
    ungroup() %>%
    select(!c(val,names)) %>%
    pivot_wider(names_from = variable, values_from = `M(sd)`, names_sep = "key", values_fn = list) %>%
    unnest(all_of(outcome.var)) %>%
    group_by(id) %>%
    mutate(dups = duplicated(id)) %>%
    filter(dups == FALSE) %>%
    ungroup() %>%
    select(!c(id,dups))


  # ----------- 2. Handle 'desc_only' case -----------
  if (desc_only == TRUE) {
    final_descriptive_table <- descriptive_stats_wide  %>%
      remove_na_in_brackets(var = outcome.var) %>%
      mutate(value = replace_na(value, "Missing")) %>%
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
      rename_with(~paste0(outcome.var," M(SD)"), starts_with(outcome.var)) %>%
      rename("variable" = "value",
             "n(%)" = `n(%)`)

    if (remove_missings == TRUE) {
      final_descriptive_table = final_descriptive_table %>%
        filter(str_detect(variable, "Missing", negate = TRUE))
    }

    return(final_descriptive_table)

  } else {
    # ----------- 3. Main Analysis Block -----------

    # Get factor and numeric variable names from the analysis dataset
    factors.dat = analysis_data %>% select(where(is.factor)) %>% names()
    output.var = analysis_data %>% select(where(is.numeric)) %>% names()

    data_with_factors_as_strings = analysis_data %>%
      mutate(across(all_of(factors.dat), ~paste(as.numeric(.), .)))

    ## ----------- 3a. Two-Group Comparison -----------
    data_long_2_groups = analysis_data %>%
      select_if(~ nlevels(.) == 2 | is.numeric(.)) %>%
      mutate(across(c(where(is.factor)), ~ as.factor(str_replace_all(as.factor(paste(as.numeric(.), .)), "NA NA", NA_character_)))) %>%
      pivot_longer(cols = c(where(is.factor)),
                   names_to = "key",
                   values_to = "value")

    homogeneity_test_2_groups = data_long_2_groups %>%
      group_by(key) %>%
      summarise(across(all_of(output.var), ~fligner.test(., value)$p.value)) %>%
      pivot_longer(all_of(output.var),
                   names_to = "names_continous_var",
                   values_to = "p_val_homo")

    normality_test_2_groups = data_long_2_groups %>%
      group_by(key) %>%
      summarise(across(all_of(output.var), function(x) {
        n_samples = n()
        if (n_samples <= 5000) {
          message("Sample size is less than or equal to 5000, performing Shapiro test on all data for key: ", unique(key))
          shapiro.test(x) %>% tidy()
        } else {
          message("Sample size is greater than 5000, randomly sampling data for Shapiro test for key: ", unique(key))
          sampled_data = sample_n(cur_data(), size = min(n_samples, 5000), replace = FALSE)
          shapiro.test(sampled_data[[1]]) %>% tidy()
        }
      })) %>%
      pivot_longer(all_of(output.var),
                   names_to = "names_continous_var",
                   values_to = "p_val_shapiro")

    kruskal_summary_2_groups = data_long_2_groups %>%
      group_by(key) %>%
      summarise(across(all_of(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
      pivot_longer(all_of(output.var),
                   names_to = "names_continous_var",
                   values_to = "stat") %>%
      full_join(homogeneity_test_2_groups) %>%
      full_join(normality_test_2_groups) %>%
      as.matrix() %>%
      as_tibble() %>%
      mutate(stat.p.value = as.numeric(stat.p.value),
             p_val_homo = as.numeric(p_val_homo),
             p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value)) %>%
      filter(stat.p.value < 0.05) %>%
      mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
             non_homo_normal = p_val_homo < 0.05)

    grouped_data_2_groups =  data_long_2_groups %>%
      group_by(key)

    # Wilcoxon Test for non-normal, homoscedastic data
    if (any(kruskal_summary_2_groups$homo_non_normal == TRUE)) {
      vars_for_wilcox=filter(.data = kruskal_summary_2_groups, homo_non_normal == TRUE)
      results_wilcox = data_long_2_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~rstatix::wilcox_test(. ~ value, data = grouped_data_2_groups, p.adjust.method = "bonferroni"))) %>%
        as.matrix() %>% as_tibble() %>% select(-key)  %>%
        pivot_longer(cols = contains(c(".key","..y.",".group",".n1",".n2",".statistic",".p")),
                     names_to = "names", values_to = "val") %>%
        mutate(names = str_replace(names, paste0(output.var,collapse = "|"),"")) %>%
        pivot_wider(names_from = names, values_from = val, values_fn = list) %>%
        unnest(cols = c(".key","..y.", ".group1", ".group2", ".n1",".n2",".statistic",".p")) %>%
        rename("names_continous_var" = "..y.", "key" = ".key") %>%
        mutate(merged_cols = paste0(key,",",names_continous_var),
               .p = as.numeric(.p), .p_stars = format_p(.p, stars_only = TRUE), .p = format_p(.p),
               .statistic = as.numeric(.statistic), across(ends_with(".statistic"), ~round(., 2))) %>%
        filter(str_detect(merged_cols, paste0(vars_for_wilcox$key,",",vars_for_wilcox$names_continous_var,collapse = "|"))) %>%
        distinct(.statistic, .p, .keep_all = T)

      if (short_results == TRUE) {
        results_wilcox <- results_wilcox %>% mutate(results_agregated = paste0(.p))
      } else {
        results_wilcox = results_wilcox %>% mutate(results_agregated = paste0("W = ", .statistic,", ", .p))
      }

      aggregated_wilcox_results = results_wilcox %>%
        select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
        mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
        group_by(merged_cols,key,names_continous_var) %>%
        summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
        ungroup %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    # Welch's T-test for non-homoscedastic data
    if (any(kruskal_summary_2_groups$non_homo_normal == TRUE)) {
      vars_for_welch=filter(.data = kruskal_summary_2_groups, non_homo_normal == TRUE)
      results_welch = data_long_2_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~rstatix::t_test(. ~value, var.equal = FALSE, data = grouped_data_2_groups, p.adjust.method = "bonferroni"))) %>%
        as.matrix() %>% as_tibble() %>% select(-key)  %>%
        pivot_longer(cols = contains(c(".key","..y.",".group",".n1",".n2",".conf.",".se",".statistic",".df",".p",".method")),
                     names_to = "names", values_to = "val") %>%
        mutate(names = str_replace(names, paste0(output.var,collapse = "|"),"")) %>%
        pivot_wider(names_from = names, values_from = val, values_fn = list) %>%
        unnest(cols = c(.key, ..y., .group1, .group2, .n1, .n2, .statistic, .df, .p)) %>%
        rename("names_continous_var" = "..y.", "key" = ".key") %>%
        mutate(merged_cols = paste0(key,",",names_continous_var),
               .df = as.numeric(.df), .p_stars = format_p(.p, stars_only = TRUE), .p = format_p(.p),
               .statistic = as.numeric(.statistic), across(ends_with(c(".statistic",".df")), ~round(., 2))) %>%
        filter(str_detect(merged_cols, paste0(vars_for_welch$key,",",vars_for_welch$names_continous_var,collapse = "|"))) %>%
        distinct(.statistic, .p, .keep_all = T)

      if (short_results == TRUE) {
        results_welch <- results_welch %>% mutate(results_agregated = paste0(.p))
      } else {
        results_welch = results_welch %>%
          mutate(results_agregated = paste0(str_extract(.group1, "^.{1}"), " vs ", str_extract(.group2, "^.{1}"),", ", "t(",.df,")"," = ",.statistic,", ", .p))
      }

      aggregated_welch_results = results_welch %>%
        select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
        mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
        group_by(merged_cols,key,names_continous_var) %>%
        summarise("Group comparison" = paste(results_agregated, collapse = ", ")) %>%
        ungroup %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    # Combine Welch and Wilcox results if both exist
    if(exists("aggregated_welch_results") & exists("aggregated_wilcox_results")) {
      if (any(duplicated(full_join(aggregated_welch_results, aggregated_wilcox_results)$key))) {
        aggregated_wilcox_results <- full_join(aggregated_welch_results,
                                               aggregated_wilcox_results) %>%
          group_by(key) %>%
          tidyr::fill(everything(), .direction = 'updown') %>%
          ungroup() %>%
          filter(!duplicated(key))
        rm(aggregated_welch_results)
      } else {
        aggregated_wilcox_results <- full_join(aggregated_welch_results,
                                               aggregated_wilcox_results)
        rm(aggregated_welch_results)
        print("both Welsh and Wilcox are significant in some variables - merging into one object")
      }
    }


    ## ----------- 3b. Multi-Group (>2) Comparison -----------
    if (analysis_data %>% select_if(~ nlevels(.) > 2) %>% length() == 0) {
      print("It seems that groups you selected does not contain more than 2 levels, skipping multigroup analysis.....")
    } else if (analysis_data %>% select_if(~ nlevels(.) > 2) %>% length() > 0) {
      print("Groups you selected contains more than two groups, analysing........")

      data_long_multi_groups = analysis_data %>%
        select_if(~ nlevels(.) > 2 | is.numeric(.)) %>%
        mutate(across(c(where(is.factor)), ~ as.factor(str_replace_all(as.factor(paste(as.numeric(.), .)), "NA NA", NA_character_)))) %>%
        pivot_longer(cols = c(where(is.factor)),
                     names_to = "key",
                     values_to = "value")

      homogeneity_test_multi_groups = data_long_multi_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~fligner.test(., value)$p.value)) %>%
        pivot_longer(all_of(output.var),
                     names_to = "names_continous_var",
                     values_to = "p_val_homo")

      normality_test_multi_groups = data_long_multi_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~ {
          n_samples = n()
          if (n_samples <= 5000) {
            message("Sample size is less than or equal to 5000, performing Shapiro test on all data for key: ", unique(key))
            shapiro.test(.) %>% tidy()
          } else {
            message("Sample size is greater than 5000, randomly sampling data for Shapiro test for key: ", unique(key))
            sampled_data = sample_n(cur_data(), size = min(n_samples, 5000), replace = FALSE)
            shapiro.test(sampled_data[[1]]) %>% tidy()
          }
        })) %>%
        pivot_longer(all_of(output.var),
                     names_to = "names_continous_var",
                     values_to = "p_val_shapiro")

      kruskal_summary_multi_groups = data_long_multi_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
        pivot_longer(all_of(output.var),
                     names_to = "names_continous_var",
                     values_to = "stat") %>%
        full_join(homogeneity_test_multi_groups) %>%
        full_join(normality_test_multi_groups) %>%
        as.matrix() %>% as_tibble() %>%
        mutate(stat.p.value = as.numeric(stat.p.value),
               p_val_homo = as.numeric(p_val_homo),
               p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value)) %>%
        filter(stat.p.value < 0.05) %>%
        mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
               non_homo_normal = p_val_homo < 0.05)

      grouped_data_multi_groups = data_long_multi_groups %>%
        group_by(key)

      # Dunn's Test for non-normal, homoscedastic data
      if (any(kruskal_summary_multi_groups$homo_non_normal == TRUE)) {
        vars_for_dunn=filter(.data = kruskal_summary_multi_groups, homo_non_normal == TRUE)
        results_dunn = data_long_multi_groups %>%
          group_by(key) %>%
          summarise(across(all_of(output.var), ~rstatix::dunn_test(. ~value, data = grouped_data_multi_groups, detailed = T, p.adjust.method = "bonferroni"))) %>%
          as.matrix() %>% as_tibble() %>% select(-key)  %>%
          pivot_longer(cols = contains(c(".key","..y.",".group",".n1",".n2",".statistic",".p",".p.adj",".p.adj.signif")),
                       names_to = "names", values_to = "val") %>%
          mutate(names = str_replace(names, paste0(output.var,collapse = "|"),"")) %>%
          pivot_wider(names_from = names, values_from = val, values_fn = list) %>%
          unnest(cols = c(.key, ..y., .group1, .group2, .n1, .n2, .statistic, .p, .p.adj, .p.adj.signif)) %>%
          rename("names_continous_var" = "..y.", "key" = ".key") %>%
          mutate(merged_cols = paste0(key,",",names_continous_var), .p.adj = as.numeric(.p.adj)) %>%
          filter(.p.adj < 0.05) %>%
          mutate(.p.adj_stars = format_p(.p.adj, stars_only = T), .p.adj = format_p(.p.adj),
                 .statistic = as.numeric(.statistic), across(ends_with(".statistic"), ~round(., 2))) %>%
          filter(str_detect(merged_cols, paste0(vars_for_dunn$key,",",vars_for_dunn$names_continous_var,collapse = "|"))) %>%
          distinct(.statistic, .p, .keep_all = T) %>%
          left_join(kruskal_summary_multi_groups %>% rename(stat.p.val.kruskal = stat.p.value) %>% select(key,names_continous_var,stat.p.val.kruskal)) %>%
          mutate(stat.p.val.kruskal = replace(stat.p.val.kruskal, duplicated(stat.p.val.kruskal), ""),
                 stat.p.val.kruskal = format_p(as.numeric(stat.p.val.kruskal)))

        if (short_results == TRUE) {
          results_dunn <- results_dunn %>%
            mutate(results_agregated = paste0(stat.p.val.kruskal," ","(",str_extract(.group1, "^.{1}"),"-",str_extract(.group2, "^.{1}"),.p.adj_stars,")"))
        } else {
          results_dunn = results_dunn %>%
            mutate(results_agregated = paste0(stat.p.val.kruskal," ","(",str_extract(.group1, "^.{1}"), "-",str_extract(.group2, "^.{1}"),", ", "z = ", .statistic,", ", .p.adj,")"))
        }

        aggregated_dunn_results = results_dunn %>%
          select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
          mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
          group_by(merged_cols,key,names_continous_var) %>%
          summarise("Group comparison" = paste0(results_agregated, collapse = ",")) %>%
          ungroup %>%
          select(key, `Group comparison`,names_continous_var) %>%
          mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
          pivot_wider(names_from = names_continous_var, values_from = `Group comparison`) %>%
          mutate_all(~str_replace_all(., "\\,+[:blank:]", ","))

        if (short_results == TRUE) {
          aggregated_dunn_results <- aggregated_dunn_results %>% removing_nested_prentecies()
        }
      }

      # Games-Howell Test for non-homoscedastic data
      if (any(kruskal_summary_multi_groups$non_homo_normal == TRUE)) {
        vars_for_games_howell=filter(.data = kruskal_summary_multi_groups, non_homo_normal == TRUE)
        results_games_howell = data_long_multi_groups %>%
          group_by(key) %>%
          summarise(across(all_of(output.var), ~rstatix::games_howell_test(. ~value, data = grouped_data_multi_groups, detailed = T))) %>%
          as.matrix() %>% as_tibble() %>% select(-key)  %>%
          pivot_longer(cols = contains(c(".key","..y.",".group",".n1",".n2",".estimate",".conf.",".se",".statistic",".df",".p.",".method")),
                       names_to = "names", values_to = "val") %>%
          mutate(names = str_replace(names, paste0(output.var,collapse = "|"),"")) %>%
          pivot_wider(names_from = names, values_from = val, values_fn = list) %>%
          unnest(cols = c(.key, ..y., .group1, .group2, .n1, .n2, .estimate, .conf.low, .conf.high, .se, .statistic, .df, .p.adj, .p.adj.signif, .method)) %>%
          rename("names_continous_var" = "..y.", "key" = ".key") %>%
          mutate(.p.adj = as.numeric(.p.adj)) %>%
          filter(.p.adj < 0.05) %>%
          mutate(merged_cols = paste0(key,",",names_continous_var), .df = as.numeric(.df), .statistic = as.numeric(.statistic),
                 .estimate = as.numeric(.estimate), across(ends_with(c(".statistic",".df")), ~round(., 2))) %>%
          filter(str_detect(merged_cols, paste0(vars_for_games_howell$key,",",vars_for_games_howell$names_continous_var,collapse = "|"))) %>%
          distinct(.estimate, .statistic, .keep_all = T) %>%
          mutate(.p.adj_stars = format_p(.p.adj, stars_only = TRUE), .p.adj = format_p(.p.adj),
                 .estimate = round(.estimate,digits = 2)) %>%
          left_join(kruskal_summary_multi_groups %>% rename(stat.p.val.kruskal = stat.p.value) %>% select(key,names_continous_var,stat.p.val.kruskal)) %>%
          mutate(stat.p.val.kruskal = replace(stat.p.val.kruskal, duplicated(stat.p.val.kruskal), ""),
                 stat.p.val.kruskal = format_p(as.numeric(stat.p.val.kruskal)))

        if (short_results == TRUE) {
          results_games_howell <- results_games_howell %>%
            mutate(results_agregated = paste0(stat.p.val.kruskal," ","(",str_extract(.group1, "^.{1}"),"-",str_extract(.group2, "^.{1}"),.p.adj_stars,")"))
        } else {
          results_games_howell = results_games_howell %>%
            mutate(results_agregated = paste0(stat.p.val.kruskal," ",str_extract(.group1, "^.{1}"), "-",str_extract(.group2, "^.{1}"),", ","t(",.df,")"," = ",.estimate,", ", .p.adj))
        }

        aggregated_games_howell_results = results_games_howell %>%
          select(starts_with(c("key","names_cont","results_agre","merged_cols"))) %>%
          mutate(merged_cols = as.numeric(as.factor(merged_cols))) %>%
          group_by(merged_cols,key,names_continous_var) %>%
          summarise("Group comparison" = paste0(results_agregated, collapse = ", ")) %>%
          ungroup %>%
          select(key, `Group comparison`,names_continous_var) %>%
          mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
          pivot_wider(names_from = names_continous_var, values_from = `Group comparison`) %>%
          mutate_all(~str_replace_all(., "\\,+[:blank:]", ","))

        if (short_results == TRUE) {
          aggregated_games_howell_results <- aggregated_games_howell_results %>% removing_nested_prentecies()
        }
      }
    }


    # ----------- 4. Final Table Assembly -----------
    coalesce_by_column <- function(df) { return(coalesce(!!! as.list(df))) }

    # Combine multi-group results (Dunn and Games-Howell)
    if(exists("aggregated_games_howell_results") & exists("aggregated_dunn_results")) {
      results_multigroup_tests = aggregated_dunn_results %>%
        full_join(aggregated_games_howell_results) %>%
        group_by(key) %>%
        summarise_all(coalesce_by_column) %>%
        full_join(descriptive_stats_wide)
    } else if (exists("aggregated_games_howell_results") & !exists("aggregated_dunn_results")) {
      results_multigroup_tests = aggregated_games_howell_results %>%
        full_join(descriptive_stats_wide)
    } else if (!exists("aggregated_games_howell_results") & exists("aggregated_dunn_results")) {
      results_multigroup_tests = aggregated_dunn_results %>%
        full_join(descriptive_stats_wide)
    } else if (!exists("aggregated_games_howell_results") & !exists("aggregated_dunn_results")) {
      results_multigroup_tests = descriptive_stats_wide
    }

    results_multigroup_tests <- results_multigroup_tests  %>%
      mutate(
        across(ends_with("Group difference"), ~replace(., duplicated(.), "")),
        across(ends_with("Group difference"), ~replace(., is.na(.), ""))
      )

    # Combine with 2-group results
    two.level.factors = analysis_data %>% select_if(~ nlevels(.) == 2) %>% names()

    if(exists("aggregated_wilcox_results")) {
      table_with_wilcox = aggregated_wilcox_results %>%
        full_join(descriptive_stats_wide) %>%
        filter(str_detect(key, paste0(two.level.factors,collapse = "|")))
    }

    if(exists("table_with_wilcox")) {
      table_wilcox_and_multigroup = table_with_wilcox %>%
        full_join(results_multigroup_tests) %>%
        longer_tab()
    }

    if(exists("aggregated_welch_results")) {
      table_with_welch = aggregated_welch_results %>%
        full_join(descriptive_stats_wide) %>%
        filter(str_detect(key, paste0(two.level.factors,collapse = "|")))
    }

    if(exists("aggregated_welch_results") & !exists("aggregated_wilcox_results")) {
      table_welch_only_and_multigroup = table_with_welch %>%
        full_join(results_multigroup_tests) %>%
        longer_tab()
    }

    if(exists("table_with_wilcox") & exists("table_with_welch")) {
      table_both_2group_and_multigroup = table_with_welch %>%
        full_join(table_with_wilcox) %>%
        full_join(results_multigroup_tests) %>%
        longer_tab()
    }

    # ----------- 5. Select and Return Final Table -----------
    if(exists("table_wilcox_and_multigroup") & exists("table_both_2group_and_multigroup")) {
      # This block seems to have a bug where it modifies one table and returns another.
      # Preserving original logic as requested.
      table_wilcox_and_multigroup = table_wilcox_and_multigroup %>%
        full_join(table_both_2group_and_multigroup) %>%
        mutate(across(contains("Group difference"), ~ifelse(is.na(.), "", .)))

      sort.names = table_wilcox_and_multigroup %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()

      table_wilcox_and_multigroup = table_wilcox_and_multigroup %>%
        relocate(all_of(sort.names))
      return(table_both_2group_and_multigroup) # Original code returns this object

    } else {
      if(exists("table_both_2group_and_multigroup")) {
        table_both_2group_and_multigroup = table_both_2group_and_multigroup %>%
          full_join(results_multigroup_tests) %>%
          longer_tab()
      }

      if(exists("table_wilcox_and_multigroup")) {
        sort.names = table_wilcox_and_multigroup %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        table_to_return = table_wilcox_and_multigroup %>%
          relocate(all_of(sort.names))
        return(table_to_return)
      }

      if(exists("table_welch_only_and_multigroup")) {
        sort.names = table_welch_only_and_multigroup %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        table_to_return = table_welch_only_and_multigroup %>%
          relocate(all_of(sort.names))
        return(table_to_return)
      }

      if (exists("table_both_2group_and_multigroup")) {
        sort.names = table_both_2group_and_multigroup %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        table_both_2group_and_multigroup = table_both_2group_and_multigroup %>%
          relocate(all_of(sort.names))
        return(table_both_2group_and_multigroup)
      }

      # If only multi-group tests were significant (or no 2-group tests were run)
      if(!exists("aggregated_wilcox_results") & !exists("aggregated_welch_results")) {
        table_multigroup_only_formatted <- results_multigroup_tests %>%
          longer_tab()

        sort.names = table_multigroup_only_formatted %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        table_multigroup_only_formatted = table_multigroup_only_formatted %>%
          relocate(all_of(sort.names))
        return(table_multigroup_only_formatted)
      }

      if (exists("aggregated_games_howell_results") & !all(c("aggregated_wilcox_results","table_with_wilcox",
                                                             "aggregated_welch_results","table_with_welch",
                                                             "table_wilcox_and_multigroup","table_both_2group_and_multigroup","table_welch_only_and_multigroup") %in% ls()))
      {
        sort.names = results_multigroup_tests %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        results_multigroup_tests = results_multigroup_tests %>%
          relocate(all_of(sort.names))
        return(results_multigroup_tests)
      }
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
# results_table = mult.g.comp(groups = c("Family_status", "Education","Gender"),
#                   outcome.var = c("Age","Work_years","eps"),
#                   df = dat)
#
# results_table %>% view()
#
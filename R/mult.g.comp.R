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
#' Welch, B. L. (1947). "The generalization of 'Student's'"
#' problem when several different population variances are involved. Biometrika.
#' 34 (1--2): 28--35.
#'
#' Wilcoxon, F., Individual Comparisons by Ranking Methods,
#' Biometrics Bulletin, Vol. 1, 1945, pp. 80--83. DOI:10.2307/3001968
#'
#' Dunn, O. J. (1961) Multiple comparisons among means.
#' Journal of the American Statistical Association. 56, 52--64.
#'
#' Games, P. A., Keselman, H. J., & Clinch, J. J.
#' Tests for homogeneity of variance in factorial designs.
#' Psychological Bulletin, 86, 978--984
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
#' @importFrom dplyr mutate_if
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom dplyr all_of
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarize
#' @importFrom stringr str_extract
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @importFrom dplyr select_if
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

# ----------------------------------------------------------------------------------------
# INTERNAL HELPER FUNCTIONS --------------------------------------------------------------
# ----------------------------------------------------------------------------------------

#' @keywords internal
create_desc_table <- function(groups, outcome.var, df, percent_decimals) {
  factor_vars <- df %>% select(where(is.factor)) %>% names()
  df %>% drop_na(groups)
  df %>%
    mutate(across(all_of(factor_vars), ~ paste(as.numeric(.), .))) %>%
    pivot_longer(cols = all_of(groups),
                 names_to = "key",
                 values_to = "value") %>%
    group_by(key, value) %>%
    summarise(across(all_of(outcome.var), list(mean = mean, sd = sd), na.rm = TRUE),
              n = n()) %>%
    mutate(percent = as.character(round(n / sum(n) * 100, digits = percent_decimals))) %>%
    ungroup() %>%
    mutate_all(~ str_replace_all(., "NA NA|NaN|NA", NA_character_))
}

#' @keywords internal
remove_na_brackets <- function(x, var) {
  x %>%
    mutate(
      across(all_of(var), ~ str_replace_all(., "\\(NA,\\)", "")),
      across(all_of(var), ~ str_replace_all(., "NA+\\,", "")),
      across(all_of(var), ~ str_replace_all(., "\\(NA\\)", "")),
      across(all_of(var), ~ str_replace_all(., "NA", ""))
    )
}

#' @keywords internal
remove_nested_parentheses <- function(df) {
  success <- FALSE
  while (!success) {
    df <- df %>%
      mutate_all(~ stringr::str_remove_all(., "\\)(?=.*\\))")) %>%
      mutate_all(~ stringr::str_replace(., "\\((.*)\\(", "(\\1"))
    success <- df %>% summarise(across(everything(), ~ stringr::str_count(., "\\(") >= 2)) %>%
      any(isTRUE(.), na.rm = TRUE) == FALSE
  }
  df
}

#' @keywords internal
format_long_table <- function(x, outcome.var, remove_missings, percent_decimals) {
  if (summarise(x, contains_stat_results = any(!is.na(across(contains("Group difference")))))$contains_stat_results) {
    if (remove_missings) {
      x <- x %>% filter(str_detect(value, "Missing", negate = TRUE))
    }
    x <- x %>%
      remove_na_brackets(var = outcome.var) %>%
      mutate(value = replace_na(value, "Missing")) %>%
      group_by(key) %>%
      filter(!if_any(ends_with(outcome.var), duplicated)) %>%
      ungroup() %>%
      mutate(across(contains("Group difference"), ~ ifelse(duplicated(.), "", .))) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate_all(~ replace(., is.na(.), "")) %>%
      mutate(across(ends_with("Group difference"), ~ replace(., duplicated(.), ""))) %>%
      group_by(key) %>%
      group_modify(~ add_row(., .before = 1)) %>%
      ungroup() %>%
      mutate(across(ends_with("key"), ~ replace(., duplicated(.), NA_character_))) %>%
      mutate(value = if_else(is.na(value), key, value)) %>%
      mutate_all(~ replace(., is.na(.), "")) %>%
      mutate(`n(%)` = paste0(as.numeric(n), "(", percent, ")")) %>%
      mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
      select(-c("key", "n", "percent")) %>%
      rename_with(~ paste0(outcome.var, " M(SD)"), ends_with(outcome.var)) %>%
      rename(variable = value, `n(%)` = `n(%)`)
  } else {
    x <- x %>%
      mutate(across(ends_with("Group difference"), ~ replace(., duplicated(.), ""))) %>%
      group_by(key) %>%
      group_modify(~ add_row(., .before = 1)) %>%
      ungroup() %>%
      mutate(across(ends_with("key"), ~ replace(., duplicated(.), NA_character_))) %>%
      mutate(value = if_else(is.na(value), key, value)) %>%
      mutate_all(~ replace(., is.na(.), "")) %>%
      mutate(`n(%)` = paste0(as.numeric(n), " (", percent, ")")) %>%
      mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
      select(-c("key", "n", "percent")) %>%
      rename_with(~ paste0(outcome.var, " M(SD)"), ends_with(outcome.var)) %>%
      rename(variable = value, `n(%)` = `n(%)`)
  }
  x
}

# ----------------------------------------------------------------------------------------
# MAIN FUNCTION --------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

#' @export
mult.g.comp <- function(df, outcome.var, groups,
                        desc_only = FALSE, short_results = TRUE,
                        remove_missings = FALSE, percent_decimals = 2) {
  # ----------------------------------------------------------------
  # DESCRIPTIVE STATISTICS -----------------------------------------
  # ----------------------------------------------------------------
  desc_stats <- create_desc_table(groups, outcome.var, df, percent_decimals) %>%
    mutate(value = str_replace(value, "NA NA", "Missing"))

  if (sum(desc_stats$n <= 1) >= 1 & !desc_only) {
    stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
  }

  # ----------------------------------------------------------------
  # DATA FOR INFERENTIAL ANALYSIS ----------------------------------
  # ----------------------------------------------------------------
  analysis_data <- df %>% select(c(vctrs::vec_c(groups, outcome.var)))

  # recompute outcome.var in case order changed
  desc_num_cols <- desc_stats %>% select(ends_with(c("_mean", "_sd"))) %>% names()
  outcome.var <- desc_stats %>%
    select(ends_with(c("_mean", "_sd"))) %>%
    names() %>%
    str_replace_all("_mean", "") %>%
    str_replace_all("_sd", "") %>%
    unique()

  # MERGE MEANS AND SDs INTO M(SD)
  desc_stats <- desc_stats %>%
    ungroup() %>%
    mutate(across(all_of(desc_num_cols), ~ as.numeric(.))) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "names", values_to = "val", all_of(desc_num_cols)) %>%
    mutate(variable = str_extract(names, paste0(outcome.var, collapse = "|"))) %>%
    mutate(val = ifelse(str_detect(names, "_sd"), paste0(" (", val, ")"), val)) %>%
    group_by(id, variable) %>%
    mutate(`M(sd)` = paste0(val, collapse = "")) %>%
    ungroup() %>%
    select(-c(val, names)) %>%
    pivot_wider(names_from = variable, values_from = `M(sd)`, names_sep = "key", values_fn = list) %>%
    unnest(all_of(outcome.var)) %>%
    group_by(id) %>%
    filter(!duplicated(id)) %>%
    ungroup() %>%
    select(-c(id))

  # RETURN ONLY DESCRIPTIVES IF REQUESTED
  if (desc_only) {
    output_desc_only <- desc_stats %>%
      remove_na_brackets(var = outcome.var) %>%
      mutate(value = replace_na(value, "Missing")) %>%
      group_by(key) %>%
      group_modify(~ add_row(., .before = 1)) %>%
      ungroup() %>%
      mutate(across(ends_with("key"), ~ replace(., duplicated(.), NA_character_))) %>%
      mutate(value = if_else(is.na(value), key, value)) %>%
      mutate_all(~ replace(., is.na(.), "")) %>%
      mutate(`n(%)` = paste0(as.numeric(n), " (", percent, ")")) %>%
      mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
      select(-c("key", "n", "percent")) %>%
      relocate(`n(%)`, .after = value) %>%
      rename_with(~ paste0(outcome.var, " M(SD)"), starts_with(outcome.var)) %>%
      rename(variable = value, `n(%)` = `n(%)`)

    if (remove_missings) {
      output_desc_only <- output_desc_only %>% filter(!str_detect(variable, "Missing"))
    }
    return(output_desc_only)
  }

  # ----------------------------------------------------------------
  # HELPER FOR COALESCE AFTER MERGE --------------------------------
  # ----------------------------------------------------------------
  coalesce_by_column <- function(df) coalesce(!!!as.list(df))

  # ----------------------------------------------------------------
  # TWO‑GROUP ANALYSIS ---------------------------------------------
  # ----------------------------------------------------------------
  numeric_vars <- analysis_data %>% select(where(is.numeric)) %>% names()
  data_two_groups <- analysis_data %>%
    select_if(~ nlevels(.) == 2 | is.numeric(.)) %>%
    mutate(across(where(is.factor), ~ as.factor(str_replace_all(paste(as.numeric(.), .), "NA NA", NA_character_)))) %>%
    pivot_longer(cols = where(is.factor), names_to = "key", values_to = "value")

  homogeneity_2g <- data_two_groups %>%
    group_by(key) %>%
    summarise(across(all_of(numeric_vars), ~ fligner.test(., value)$p.value)) %>%
    pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "p_val_homo")

  normality_2g <- data_two_groups %>%
    group_by(key) %>%
    summarise(across(all_of(numeric_vars), function(x) {
      n_samples <- n()
      if (n_samples <= 5000) {
        shapiro.test(x) %>% tidy()
      } else {
        sampled_data <- sample_n(cur_data(), size = min(n_samples, 5000), replace = FALSE)
        shapiro.test(sampled_data[[1]]) %>% tidy()
      }
    })) %>%
    pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "p_val_shapiro")

  kw_2g <- data_two_groups %>%
    group_by(key) %>%
    summarise(across(all_of(numeric_vars), ~ kruskal.test(. ~ value) %>% tidy())) %>%
    pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "stat") %>%
    full_join(homogeneity_2g, by = c("key", "numeric_var")) %>%
    full_join(normality_2g, by = c("key", "numeric_var")) %>%
    mutate(stat.p.value = as.numeric(stat$p.value),
           p_val_homo = as.numeric(p_val_homo),
           p_val_shapiro.p.value = as.numeric(p_val_shapiro$p.value)) %>%
    filter(stat.p.value < 0.05) %>%
    mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
           non_homo_normal = p_val_homo < 0.05)

  aggregated_wilcox <- NULL
  aggregated_welch  <- NULL

  if (any(kw_2g$homo_non_normal)) {
    non_normal_vars <- filter(kw_2g, homo_non_normal)
    wilcox_results <- data_two_groups %>%
      group_by(key) %>%
      summarise(across(all_of(numeric_vars), ~ rstatix::wilcox_test(. ~ value, data = cur_group(), p.adjust.method = "bonferroni"))) %>%
      select(-key) %>%
      pivot_longer(cols = everything(), names_to = "name", values_to = "val") %>%
      mutate(name = str_replace(name, paste0(numeric_vars, collapse = "|"), "")) %>%
      pivot_wider(names_from = name, values_from = val, values_fn = list) %>%
      unnest(cols = everything()) %>%
      rename(numeric_var = ..y., key = .key) %>%
      mutate(merged = paste0(key, ",", numeric_var),
             .p = as.numeric(.p),
             .p_stars = format_p(.p, stars_only = TRUE),
             .p = format_p(.p),
             .statistic = round(as.numeric(.statistic), 2)) %>%
      filter(str_detect(merged, paste0(non_normal_vars$key, ",", non_normal_vars$numeric_var, collapse = "|"))) %>%
      distinct(.statistic, .p, .keep_all = TRUE)

    if (short_results) {
      wilcox_results <- wilcox_results %>% mutate(result = .p)
    } else {
      wilcox_results <- wilcox_results %>% mutate(result = paste0("W = ", .statistic, ", ", .p))
    }

    aggregated_wilcox <- wilcox_results %>%
      select(key, numeric_var, result) %>%
      mutate(numeric_var = paste0(numeric_var, " Group difference")) %>%
      pivot_wider(names_from = numeric_var, values_from = result)
  }

  if (any(kw_2g$non_homo_normal)) {
    hetero_vars <- filter(kw_2g, non_homo_normal)
    welch_results <- data_two_groups %>%
      group_by(key) %>%
      summarise(across(all_of(numeric_vars), ~ rstatix::t_test(. ~ value, var.equal = FALSE, data = cur_group(), p.adjust.method = "bonferroni"))) %>%
      select(-key) %>%
      pivot_longer(cols = everything(), names_to = "name", values_to = "val") %>%
      mutate(name = str_replace(name, paste0(numeric_vars, collapse = "|"), "")) %>%
      pivot_wider(names_from = name, values_from = val, values_fn = list) %>%
      unnest(cols = everything()) %>%
      rename(numeric_var = ..y., key = .key) %>%
      mutate(merged = paste0(key, ",", numeric_var),
             .df = as.numeric(.df),
             .p = as.numeric(.p),
             .p_stars = format_p(.p, stars_only = TRUE),
             .p = format_p(.p),
             .statistic = round(as.numeric(.statistic), 2)) %>%
      filter(str_detect(merged, paste0(hetero_vars$key, ",", hetero_vars$numeric_var, collapse = "|"))) %>%
      distinct(.statistic, .p, .keep_all = TRUE)

    if (short_results) {
      welch_results <- welch_results %>% mutate(result = .p)
    } else {
      welch_results <- welch_results %>%
        mutate(result = paste0(str_extract(.group1, "^.{1}"), " vs ",
                               str_extract(.group2, "^.{1}"),
                               ", t(", .df, ") = ", .statistic, ", ", .p))
    }

    aggregated_welch <- welch_results %>%
      select(key, numeric_var, result) %>%
      mutate(numeric_var = paste0(numeric_var, " Group difference")) %>%
      pivot_wider(names_from = numeric_var, values_from = result)
  }

  # ----------------------------------------------------------------
  # MULTI‑GROUP ANALYSIS (>2) --------------------------------------
  # ----------------------------------------------------------------
  aggregated_dunn <- NULL
  aggregated_gh   <- NULL
  if (analysis_data %>% select_if(~ nlevels(.) > 2) %>% ncol() > 0) {
    message("Groups you selected contain more than two levels, analysing...")
    data_multi <- analysis_data %>%
      select_if(~ nlevels(.) > 2 | is.numeric(.)) %>%
      mutate(across(where(is.factor), ~ as.factor(str_replace_all(paste(as.numeric(.), .), "NA NA", NA_character_)))) %>%
      pivot_longer(cols = where(is.factor), names_to = "key", values_to = "value")

    homogeneity_multi <- data_multi %>%
      group_by(key) %>%
      summarise(across(all_of(numeric_vars), ~ fligner.test(., value)$p.value)) %>%
      pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "p_val_homo")

    normality_multi <- data_multi %>%
      group_by(key) %>%
      summarise(across(all_of(numeric_vars), ~ {
        n_samples <- n()
        if (n_samples <= 5000) {
          shapiro.test(.) %>% tidy()
        } else {
          sampled_data <- sample_n(cur_data(), size = min(n_samples, 5000), replace = FALSE)
          shapiro.test(sampled_data[[1]]) %>% tidy()
        }
      })) %>%
      pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "p_val_shapiro")

    kw_multi <- data_multi %>%
      group_by(key) %>%
      summarise(across(all_of(numeric_vars), ~ kruskal.test(. ~ value) %>% tidy())) %>%
      pivot_longer(all_of(numeric_vars), names_to = "numeric_var", values_to = "stat") %>%
      full_join(homogeneity_multi, by = c("key", "numeric_var")) %>%
      full_join(normality_multi, by = c("key", "numeric_var")) %>%
      mutate(stat.p.value = as.numeric(stat$p.value),
             p_val_homo = as.numeric(p_val_homo),
             p_val_shapiro.p.value = as.numeric(p_val_shapiro$p.value)) %>%
      filter(stat.p.value < 0.05) %>%
      mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
             non_homo_normal = p_val_homo < 0.05)

    if (any(kw_multi$homo_non_normal)) {
      non_normal_multi <- filter(kw_multi, homo_non_normal)
      dunn_results <- data_multi %>%
        group_by(key) %>%
        summarise(across(all_of(numeric_vars), ~ rstatix::dunn_test(. ~ value, data = cur_group(), detailed = TRUE, p.adjust.method = "bonferroni"))) %>%
        select(-key) %>%
        pivot_longer(cols = everything(), names_to = "name", values_to = "val") %>%
        mutate(name = str_replace(name, paste0(numeric_vars, collapse = "|"), "")) %>%
        pivot_wider(names_from = name, values_from = val, values_fn = list) %>%
        unnest(cols = everything()) %>%
        rename(numeric_var = ..y., key = .key) %>%
        mutate(merged = paste0(key, ",", numeric_var), .p.adj = as.numeric(.p.adj)) %>%
        filter(.p.adj < 0.05) %>%
        mutate(
          .p.adj_stars = format_p(.p.adj, stars_only = TRUE),
          .p.adj = format_p(.p.adj),
          .statistic = round(as.numeric(.statistic), 2)
        ) %>%
        filter(str_detect(merged, paste0(non_normal_multi$key, ",", non_normal_multi$numeric_var, collapse = "|"))) %>%
        distinct(.statistic, .p, .keep_all = TRUE) %>%
        left_join(kw_multi %>% rename(kw_p = stat.p.value) %>% select(key, numeric_var, kw_p)) %>%
        mutate(kw_p = replace(kw_p, duplicated(kw_p), ""), kw_p = format_p(as.numeric(kw_p)))

      if (short_results) {
        dunn_results <- dunn_results %>%
          mutate(result = paste0(kw_p, " (", str_extract(.group1, "^.{1}"), "-", str_extract(.group2, "^.{1}"), .p.adj_stars, ")"))
      } else {
        dunn_results <- dunn_results %>%
          mutate(result = paste0(kw_p, " (", str_extract(.group1, "^.{1}"), "-", str_extract(.group2, "^.{1}"), ", z = ", .statistic, ", ", .p.adj, ")"))
      }

      aggregated_dunn <- dunn_results %>%
        select(key, numeric_var, result) %>%
        mutate(numeric_var = paste0(numeric_var, " Group difference")) %>%
        pivot_wider(names_from = numeric_var, values_from = result) %>%
        mutate_all(~ str_replace_all(., ",+[:blank:]", ","))

      if (short_results) aggregated_dunn <- remove_nested_parentheses(aggregated_dunn)
    }

    if (any(kw_multi$non_homo_normal)) {
      hetero_multi <- filter(kw_multi, non_homo_normal)
      gh_long <- data_multi %>%
        group_by(key) %>%
        reframe(across(all_of(numeric_vars), ~ rstatix::games_howell_test(.x ~ value, data = cur_data(), detailed = TRUE))) %>%
        pivot_longer(cols = all_of(numeric_vars), names_to = "numeric_var", values_to = "res") %>%
        unnest(res) %>%
        filter(p.adj < 0.05) %>%
        mutate(
          .p.adj_num   = as.numeric(p.adj),
          .p.adj_stars = format_p(.p.adj_num, stars_only = TRUE),
          .p.adj       = format_p(.p.adj_num),
          .estimate    = round(as.numeric(estimate), 2)
        ) %>%
        mutate(result = if (short_results) {
          paste0(.p.adj, .p.adj_stars)
        } else {
          paste0(str_extract(group1, "^.{1}"), "-", str_extract(group2, "^.{1}"),
                 ", t(", round(df, 2), ") = ", .estimate, ", ", .p.adj)
        }) %>%
        select(key, numeric_var, result)

      aggregated_gh <- gh_long %>%
        mutate(numeric_var = paste0(numeric_var, " Group difference")) %>%
        pivot_wider(names_from = numeric_var, values_from = result) %>%
        mutate_all(~ str_replace_all(., ",+[:blank:]", ","))

      if (short_results) aggregated_gh <- remove_nested_parentheses(aggregated_gh)
    }
  }

  # ----------------------------------------------------------------
  # MERGE ALL RESULTS AND FORMAT LONG TABLE ------------------------
  # ----------------------------------------------------------------
  merged_table <- desc_stats
  if (exists("aggregated_dunn") && !is.null(aggregated_dunn)) merged_table <- full_join(aggregated_dunn, merged_table, by = "key")
  if (exists("aggregated_gh")   && !is.null(aggregated_gh))   merged_table <- full_join(aggregated_gh,   merged_table, by = "key")
  if (!is.null(aggregated_wilcox)) merged_table <- full_join(aggregated_wilcox, merged_table, by = "key")
  if (!is.null(aggregated_welch))  merged_table <- full_join(aggregated_welch,  merged_table, by = "key")

  merged_table <- merged_table %>%
    group_by(key) %>%
    summarise_all(coalesce_by_column) %>%
    ungroup() %>%
    format_long_table(
      x                = .,
      outcome.var      = outcome.var,
      remove_missings  = remove_missings,
      percent_decimals = percent_decimals
    )

  merged_table
}

# -------------------------------------------------------------------------------------------------
# CODE EXAMPLES (UNCHANGED, FOR REFERENCE) ---------------------------------------------------------
# -------------------------------------------------------------------------------------------------
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
# qqq = mult.g.comp(groups = c("Family_status", "Education","Gender"),
#                   outcome.var = c("Age","Work_years","eps"),
#                   df = dat)
#
# qqq %>% view()
#
#
#
# # there are further usage examples kept exactly as in the original code ------------------------
# data_test <- readRDS("./data_for_testing.Rds")
# d <- data_test %>%
#   drop_na(c("Gender","Family_status","Education","Economical_status","Religiosity")) %>%
#   mult.g.comp(outcome.var = c("PANAS_N","PANAS_P","SMDS","PAQ"),
#               groups = c("Gender","Family_status","Education","Economical_status","Religiosity"), short_results = TRUE)
#
# d
# ds <- haven::read_sav("C:/Users/OUSHI/Downloads/Velká osamělost.sav") %>% as_factor()
# dq = ds %>%
#   #drop_na(c("Age_cat","economical_status","sex")) %>%
#   mult.g.comp(outcome.var = c("BMI","ODSIS_KOMPOZITNI","OASIS_KOMPOZITNI"),
#               groups = c("Gender","Family_status","Religiosity"), short_results = TRUE,desc_only = FALSE, remove_missings = FALSE, percent_decimals = 2)
#
#  dq %>% view()

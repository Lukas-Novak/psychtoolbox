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
#' @param show_non_significant_results if TRUE, p-values from non-significant tests are reported. Default is FALSE.
#' @param diagnostics if TRUE, prints a detailed diagnostic report for each test run. Default is FALSE.
#'
#' @return data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @keywords multiple-groups testing, Games-Howell test, Dunn-test, Yuen's test
#'
#' @details
#' This function automatically selects and performs appropriate statistical tests based on the number of groups and the underlying data assumptions.
#' For non-significant results, if `show_non_significant_results = TRUE`, the p-value of the performed test is reported. For multi-group comparisons, this is the Kruskal-Wallis test. For two-group comparisons, this is the p-value from the specific test chosen by the decision tree.
#'
#' ## Two group comparison
#' A decision tree based on normality (Shapiro-Wilk for N <= 5000, Anderson-Darling for N > 5000)
#' and homogeneity of variances (Fligner-Killeen) is used. Depending on the assumptions,
#' a Student's t-test, Welch's t-test, Wilcoxon test, or Yuen's test on trimmed means is performed.
#'
#' ## Three and more groups comparison
#' For comparisons involving three or more groups, a Kruskal-Wallis test is first performed. If significant, post-hoc tests (Dunn's test for equal variances or Games-Howell for unequal variances) are conducted.
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
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale, NJ: Erlbaum.
#'
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative science: a practical primer for t-tests and ANOVAs. Frontiers in psychology, 4, 863.
#'
#' Wilcox, R. R., & Tian, T. S. (2011). Measuring effect size: A robust heteroscedastic approach for two or more groups. Journal of Applied Statistics, 38(7), 1359-1368.
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
#' @importFrom stringr str_remove_all
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
#' @importFrom dplyr reframe
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
#' @importFrom rstatix dunn_test games_howell_test kruskal_effsize wilcox_effsize
#' @importFrom WRS2 yuen
#' @importFrom effectsize cohens_d
#' @importFrom nortest ad.test
#' @importFrom rlang sym
#' @importFrom stats kruskal.test shapiro.test var
#' @importFrom haven as_factor
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom stringr str_remove
#'
#' @examples
#' # data loading
#' tab.1=mult.g.comp(df = paq.validation.study, outcome.var = c("PAQ","G_DIF","G_DDF","G_EOT"),
#' groups = c("economical_status",
#'           "Gender",
#'           "education",
#           "family_status"))
#' # printing the output
#' print(tab.1)
#'
#' @export
#......................................................

mult.g.comp = function(df,outcome.var,groups, desc_only = FALSE, short_results = TRUE, remove_missings = FALSE, percent_decimals = 2, show_non_significant_results = FALSE, diagnostics = FALSE) {
  
  # ----------- Helper Functions -----------
  interpret_es <- function(es, cutoffs = c(0.2, 0.5, 0.8), labels = c("very small", "small", "medium", "large")) {
    abs_es <- abs(es)
    level <- findInterval(abs_es, cutoffs)
    return(labels[level + 1])
  }
  
  # This is the stable desc.tab function from your original code.
  desc.tab = function(groups, outcome.var, df) {
    # Coerce columns to factor to find them, but use the original df for processing
    factors.dat = df %>% mutate(across(all_of(groups), as.factor)) %>% select(where(is.factor)) %>% names()
    
    # Process the original dataframe
    df %>%
      drop_na(all_of(groups)) %>%
      # FIX 1: This is the key change. Apply the numeric prefixing ONLY to actual factor columns.
      # This prevents the error with `as.numeric()` on character columns.
      mutate(across(all_of(factors.dat), ~paste(as.numeric(.), haven::as_factor(.)))) %>%
      pivot_longer(cols = all_of(groups),
                   names_to = "key",
                   values_to = "group_category") %>%
      group_by(key, group_category) %>%
      summarise(
        across(
          all_of(outcome.var),
          list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))
        ),
        n = n(),
        .groups = "drop"
      ) %>%
      # Correctly calculating percentages within each group
      group_by(key) %>%
      mutate(percent = as.character(round(n / sum(n) * 100, digits = percent_decimals))) %>%
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
  
  # This is the stable and correct formatting function.
  longer_tab <- function(x, outcome.vars.stem) {
    if (any(str_detect(names(x), "Group difference"))) {
      x <- x %>%
        mutate(group_category = replace_na(group_category, "Missing")) %>%
        remove_na_in_brackets(var = outcome.vars.stem)
      
      if (remove_missings == TRUE) {
        x = x %>%
          filter(str_detect(group_category, "Missing", negate = TRUE))
      }
      
      x = x %>%
        mutate_if(is.numeric, round, 2) %>%
        mutate_all(~(replace(., is.na(.), ""))) %>%
        group_by(key) %>%
        mutate(across(contains("Group difference"), ~replace(., duplicated(.), ""))) %>%
        group_modify(~add_row(.x, .before = 1)) %>%
        ungroup() %>%
        mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
        mutate(group_category = if_else(is.na(group_category),
                                        key,
                                        paste0("  ", stringr::str_extract(group_category, "^[0-9]+"), ". ", stringr::str_remove(group_category, "^[0-9\\.]+ ")))) %>%
        mutate_all(~replace(., is.na(.), "")) %>%
        mutate(`n(%)` = paste0(as.numeric(n), "(",percent,")")) %>%
        mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
        select(-c("key","n","percent")) %>%
        rename_with(~paste0(outcome.vars.stem," M(SD)"), ends_with(outcome.vars.stem)) %>%
        rename("variable" = "group_category",
               "n(%)" = `n(%)`)
    } else {
      # Same logic for the desc_only case
      x %>%
        mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
        group_by(key) %>%
        group_modify(~add_row(.x, .before = 1)) %>%
        ungroup() %>%
        mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
        mutate(group_category = if_else(is.na(group_category),
                                        key,
                                        paste0("  ", stringr::str_extract(group_category, "^[0-9]+"), ". ", stringr::str_remove(group_category, "^[0-9\\.]+ ")))) %>%
        mutate_all(~replace(., is.na(.), "")) %>%
        mutate(`n(%)` = paste0(as.numeric(n), " (",percent,")")) %>%
        mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
        select(-c("key","n","percent")) %>%
        rename_with(~paste0(outcome.vars.stem," M(SD)"), ends_with(outcome.vars.stem)) %>%
        rename("variable" = "group_category",
               "n(%)" = `n(%)`)
    }
  }
  # ----------- End Helper Functions -----------
  
  
  # ----------- 1. Initial Data Preparation -----------
  descriptive_stats_raw = desc.tab(groups, outcome.var, df)
  
  if(sum(descriptive_stats_raw$n <= 1) >= 1 & desc_only == FALSE){
    stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
  }
  
  analysis_data = df %>%
    select(all_of(c(groups, outcome.var)))
  
  mean_sd_colnames = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names()
  
  outcome.var.stems = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names() %>%
    str_remove_all("_mean|_sd") %>%
    unique()
  
  descriptive_stats_wide = descriptive_stats_raw %>%
    ungroup() %>%
    mutate(across(all_of(mean_sd_colnames), ~as.numeric(.))) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(id = row_number()) %>%
    pivot_longer(names_to = "names", values_to = "val", all_of(mean_sd_colnames)) %>%
    mutate(variable = str_remove_all(names, "_mean|_sd")) %>%
    mutate(val = ifelse(str_detect(names, "_sd"), paste0(" (",val,")"), val)) %>%
    group_by(id, variable) %>%
    mutate("M(sd)" = paste0(val, collapse = '')) %>%
    ungroup() %>%
    select(!c(val,names)) %>%
    pivot_wider(names_from = variable, values_from = `M(sd)`, names_sep = "key", values_fn = list) %>%
    unnest(all_of(outcome.var.stems)) %>%
    group_by(id) %>%
    mutate(dups = duplicated(id)) %>%
    filter(dups == FALSE) %>%
    ungroup() %>%
    select(!c(id,dups))
  
  
  # ----------- 2. Handle 'desc_only' case -----------
  if (desc_only == TRUE) {
    final_descriptive_table <- descriptive_stats_wide  %>%
      mutate(group_category = replace_na(group_category, "Missing")) %>%
      remove_na_in_brackets(var = outcome.var.stems)
    
    if (remove_missings == TRUE) {
      final_descriptive_table = final_descriptive_table %>%
        filter(str_detect(group_category, "Missing", negate = TRUE))
    }
    
    final_descriptive_table <- final_descriptive_table %>%
      longer_tab(outcome.vars.stem = outcome.var.stems)
    
    return(final_descriptive_table)
    
  } else {
    # ----------- 3. Main Analysis Block -----------
    
    results_df <- tibble(key = character(), var = character(), result_string = character())
    two_group_tests_used <- c()
    multi_group_tests_used <- c()
    
    analysis_data_prefixed <- analysis_data %>%
      mutate(across(all_of(groups), ~paste(as.numeric(as.factor(.)), as.factor(.))))
    
    for (group_var in groups) {
      for (out_var in outcome.var.stems) {
        
        formula <- as.formula(paste0("`", out_var, "` ~ `", group_var, "`"))
        current_data <- analysis_data_prefixed %>% select(all_of(c(out_var, group_var))) %>% drop_na()
        
        if (nrow(current_data) < 2 || length(unique(current_data[[group_var]])) < 2) {
          results_df <- results_df %>% add_row(key = group_var, var = out_var, result_string = NA_character_)
          next
        }
        
        n_levels <- length(unique(current_data[[group_var]]))
        result_string <- NA_character_
        
        if(diagnostics) {
          cat("\n--- Running Diagnostics for:", out_var, "by", group_var, "---\n")
          cat("Levels:", n_levels, "\n")
        }
        
        if (n_levels == 2) {
          # ... (Two-group logic is stable and remains the same)
          shapiro_p_list <- current_data %>%
            group_by(!!rlang::sym(group_var)) %>%
            summarise(
              p = if(n() > 3 && n() < 5000) stats::shapiro.test(.data[[out_var]])$p.value else if(n() >= 5000) nortest::ad.test(.data[[out_var]])$p.value else 1,
              n = n()
            )
          shapiro_p <- min(shapiro_p_list$p)
          fligner_p <- stats::fligner.test(formula, data = current_data)$p.value
          test_name <- ""
          test_res <- NULL
          if(diagnostics) {
            normality_test_name <- if(any(shapiro_p_list$n >= 5000)) "Anderson-Darling" else "Shapiro-Wilk"
            cat(normality_test_name, "p-value (min):", shapiro_p, "\n")
            if (shapiro_p < 0.05) cat(" -> Assumption: Non-normal\n") else cat(" -> Assumption: Normal\n")
            cat("Fligner-Killeen p-value (Homogeneity):", fligner_p, "\n")
            if (fligner_p < 0.05) cat(" -> Assumption: Heteroscedastic\n") else cat(" -> Assumption: Homoscedastic\n")
          }
          if (shapiro_p >= 0.05 && fligner_p >= 0.05) {
            test_res <- stats::t.test(formula, data = current_data, var.equal = TRUE)
            test_name <- "Student's t-test"
          } else if (shapiro_p >= 0.05 && fligner_p < 0.05) {
            test_res <- stats::t.test(formula, data = current_data, var.equal = FALSE)
            test_name <- "Welch's t-test"
          } else if (shapiro_p < 0.05 && fligner_p >= 0.05) {
            test_res <- stats::wilcox.test(formula, data = current_data)
            test_name <- "Wilcoxon rank-sum test"
          } else {
            test_res <- WRS2::yuen(formula, data = current_data)
            test_name <- "Yuen's test on trimmed means"
          }
          if(diagnostics) cat("Decision:", test_name, "\n")
          p_val <- test_res$p.value
          es_val <- NA; es_type <- ""; es_citation <- ""; es_cutoffs <- c(); es_label <- ""
          if (test_name %in% c("Student's t-test", "Welch's t-test")) {
            es <- effectsize::cohens_d(formula, data = current_data, pooled_sd = (test_name == "Student's t-test"))
            es_val <- es$Cohens_d; es_type <- "Cohen's d"; es_label <- "d"; es_citation <- "(Cohen, 1988)"; es_cutoffs <- c(0.2, 0.5, 0.8)
          } else if (test_name == "Wilcoxon rank-sum test") {
            es <- rstatix::wilcox_effsize(formula, data = current_data); es_val <- es$effsize
            es_type <- "Rank-Biserial Correlation"; es_label <- "rbc"; es_citation <- "(Kerby, 2014)"; es_cutoffs <- c(0.1, 0.3, 0.5)
          } else if (test_name == "Yuen's test on trimmed means") {
            es <- WRS2::yuen.effect.ci(formula, data = current_data); es_val <- es$effsize
            es_type <- "Explanatory Measure"; es_label <- "ES"; es_citation <- "(Wilcox & Tian, 2011)"; es_cutoffs <- c(0.1, 0.3, 0.5)
          }
          if (diagnostics && !is.na(es_val)) {
            cat("Effect Size (", es_type, "): ", round(es_val, 3), "\n", sep = "")
            cat("  Interpretation Guide ", es_citation, ": ", es_cutoffs[1], " (small), ", es_cutoffs[2], " (medium), ", es_cutoffs[3], " (large)\n", sep = "")
            interpretation <- interpret_es(es_val, es_cutoffs)
            cat("  Interpretation of Current Result: A '", interpretation, "' effect was found.\n", sep = "")
          }
          if (!is.na(p_val) && (p_val < 0.05 || show_non_significant_results)) {
            two_group_tests_used <- c(two_group_tests_used, test_name)
            if (short_results) {
              result_string <- format_p(p_val)
            } else {
              es_string <- if(!is.na(es_val)) paste0(", ", es_label, " = ", round(es_val, 2)) else ""
              if(inherits(test_res, "htest") && !is.null(test_res$statistic) && 'W' %in% names(test_res$statistic)) {
                result_string <- paste0("W = ", round(test_res$statistic,2), ", ", format_p(p_val), es_string)
              } else if (inherits(test_res, "yuen")) {
                result_string <- paste0("tYuen(", round(test_res$df,2), ") = ", round(test_res$test,2), ", ", format_p(p_val), es_string)
              } else {
                result_string <- paste0("t(", round(test_res$parameter,2), ") = ", round(test_res$statistic,2), ", ", format_p(p_val), es_string)
              }
            }
          }
        } else { # n_levels > 2
          # --- Multi-Group Logic ---
          kw_test <- stats::kruskal.test(formula, data = current_data)
          if(diagnostics) cat("Kruskal-Wallis p-value:", kw_test$p.value, "\n")
          es_kw <- rstatix::kruskal_effsize(formula, data = current_data)
          es_kw_val <- es_kw$effsize
          if(diagnostics){
            es_cutoffs <- c(0.01, 0.06, 0.14)
            cat("Overall Effect Size (Epsilon-Squared): ", round(es_kw_val, 3), "\n", sep="")
            cat("  Interpretation Guide (Lakens, 2013): ", es_cutoffs[1], " (small), ", es_cutoffs[2], " (medium), ", es_cutoffs[3], " (large)\n", sep="")
            interpretation <- interpret_es(es_kw_val, es_cutoffs, labels = c("very small", "small", "medium", "large"))
            cat("  Interpretation of Current Result: A '", interpretation, "' overall effect was found.\n", sep="")
          }
          
          if (kw_test$p.value >= 0.05) {
            if (show_non_significant_results) {
              multi_group_tests_used <- c(multi_group_tests_used, "Kruskal-Wallis test")
              es_string <- paste0(", epsilon2 = ", round(es_kw_val, 2))
              result_string <- if(short_results) paste0("KW: ", format_p(kw_test$p.value)) else paste0("H(", kw_test$parameter, ") = ", round(kw_test$statistic, 2), ", ", format_p(kw_test$p.value), es_string)
            }
          } else {
            fligner_p <- stats::fligner.test(formula, data = current_data)$p.value
            posthoc_test_name <- if (fligner_p >= 0.05) "Dunn's Test" else "Games-Howell Test"
            if(diagnostics) {
              cat("Fligner-Killeen p-value (Homogeneity):", fligner_p, "\n")
              cat("Decision for post-hoc:", posthoc_test_name, "\n")
            }
            
            multi_group_tests_used <- c(multi_group_tests_used, "Kruskal-Wallis test", posthoc_test_name)
            posthoc_res <- if (fligner_p >= 0.05) rstatix::dunn_test(formula, data = current_data, p.adjust.method = "bonferroni") else rstatix::games_howell_test(formula, data = current_data)
            
            sig_pairs <- posthoc_res %>% filter(p.adj < 0.05)
            kw_es_string <- paste0(", epsilon2 = ", round(es_kw_val, 2))
            kw_stat_string <- paste0("H(", kw_test$parameter, ") = ", round(kw_test$statistic, 2), ", ", format_p(kw_test$p.value))
            
            if (nrow(sig_pairs) > 0) {
              if (short_results) {
                g1_clean <- stringr::str_remove(sig_pairs$group1, "^[0-9\\.]+ ")
                g2_clean <- stringr::str_remove(sig_pairs$group2, "^[0-9\\.]+ ")
                internal_string <- paste0(str_extract(g1_clean, "^.{1}"), "-", str_extract(g2_clean, "^.{1}"), format_p(sig_pairs$p.adj, stars_only = TRUE), collapse = ", ")
                posthoc_string <- paste0("(", internal_string, ")")
                result_string <- paste(format_p(kw_test$p.value), posthoc_string)
              } else {
                internal_strings <- sapply(1:nrow(sig_pairs), function(i) {
                  row <- sig_pairs[i, ]
                  pair_data <- current_data %>% filter(!!rlang::sym(group_var) %in% c(row$group1, row$group2))
                  es_string <- ""
                  if (posthoc_test_name == "Dunn's Test") {
                    es <- rstatix::wilcox_effsize(formula, data = pair_data); es_string <- paste0(", rbc = ", round(es$effsize, 2))
                  } else {
                    es <- effectsize::cohens_d(formula, data = pair_data); es_string <- paste0(", d = ", round(es$Cohens_d, 2))
                  }
                  stat_char <- if ("statistic" %in% names(row)) "z" else "t"
                  stat_val <- if ("statistic" %in% names(row)) row$statistic else row$estimate
                  
                  # This provides the "num vs num" format
                  group1_num <- stringr::str_extract(row$group1, "^[0-9]+")
                  group2_num <- stringr::str_extract(row$group2, "^[0-9]+")
                  
                  paste0(group1_num, " vs ", group2_num, ", ", stat_char, " = ", round(stat_val, 2), ", ", format_p(row$p.adj), es_string)
                })
                # This wraps the entire post-hoc string in one set of parentheses
                posthoc_string <- paste0("(", paste(internal_strings, collapse = "; "), ")")
                result_string <- paste(kw_stat_string, kw_es_string, posthoc_string)
              }
            } else {
              result_string <- if(short_results) format_p(kw_test$p.value) else paste0(kw_stat_string, kw_es_string)
            }
          }
        }
        results_df <- results_df %>% add_row(key = group_var, var = out_var, result_string = result_string)
      }
    }
    
    # ----------- 4. Final Table Assembly -----------
    if (nrow(results_df) > 0) {
      results_wide <- results_df %>%
        pivot_wider(names_from = var, values_from = result_string, names_glue = "{var} Group difference")
      
      # This is the stable join, which works by only matching the variable name.
      final_results_table <- full_join(descriptive_stats_wide, results_wide, by = "key")
      
    } else {
      final_results_table <- descriptive_stats_wide
    }
    
    table_to_return <- final_results_table %>%
      longer_tab(outcome.vars.stem = outcome.var.stems)
    
    sort.names = table_to_return %>% select(any_of(c("variable", "n(%)")), ends_with("Group difference")) %>% names()
    table_to_return = table_to_return %>% relocate(all_of(sort.names))
    
    if (!desc_only) {
      cat("\n--- Statistical Tests Used ---\n")
      if(length(two_group_tests_used) > 0){
        cat("For two-group comparisons: ", paste(unique(two_group_tests_used), collapse = ", "), ".\n", sep = "")
      }
      if(length(multi_group_tests_used) > 0){
        cat("For multi-group comparisons: ", paste(unique(multi_group_tests_used), collapse = ", "), ".\n", sep = "")
      }
    }
    
    return(table_to_return)
  }
}

#...............................................................................................................
# TESTING CODE
#...............................................................................................................
# # ----- Load Libraries -----
# library(dplyr)
# library(broom)
# library(tidyverse)
# library(insight)
# library(WRS2)
# library(effectsize)
#
# # ----- Data Simulation -----
# set.seed(455454)
# n <- 12000  # sample size
#
# # ----- Generate group variables ------------------------------------------
# Gender_prep    <- rbinom(n, 1, 0.50)  # 0 = Male, 1 = Female
# Education_prep <- sample(0:2, n, replace = TRUE,  # 0 = Basic, 1 = High school, 2 = University
#                          prob = c(.30, .40, .30))
#
# # ----- Define strong group effects for numeric variables -----------------
# Age <- rnorm(n,
#              mean = 18 +
#                Gender_prep * 8 +            # gender effect
#                Education_prep * 6,          # education effect
#              sd = 3)
#
# Work_years <- rnorm(n,
#                     mean = 1 +
#                       Gender_prep * 4 +
#                       Education_prep * 3 +
#                       0.20 * Age,           # logical link to age
#                     sd = 1)
#
# # eps: we add group shifts and also heteroskedasticity
# x <- rnorm(n, 1, 1)
# h <- function(x) 1 + .4 * x  # mild heteroskedasticity
#
# eps <- rnorm(n,
#              mean = -2 +
#                Gender_prep * 1.5 +
#                Education_prep * 1,
#              sd = h(x))
#
# # ----- Generate additional variables -----------------------------------
# # Variable 1: Group with 2 levels, non-normal and heteroscedastic
# Group2_prep <- rbinom(n, 1, 0.5)
# # Use a skewed distribution (chi-squared) to ensure non-normality
# Group2_value <- rchisq(n, df = 3) + (Group2_prep * 4) + rnorm(n, 0, sd = h(x)/2)
#
# # Variable 2: Group with 5 levels, non-normal and heteroscedastic
# Group5_prep <- sample(0:4, n, replace = TRUE)
# Group5_value <- rchisq(n, df = 3) + (Group5_prep * 2) + rnorm(n, 0, sd = h(x)/2)
#
# # ----- Compile the final data frame --------------------------------------
# dat <- tibble(
#   eps            = eps,
#   Gender_prep    = as.factor(Gender_prep),
#   Age            = Age,
#   Work_years     = Work_years,
#   Education_prep = as.factor(Education_prep),
#   Family_status  = case_when(
#     Age > 30 ~ "Married",
#     Age > 22 ~ "In relationship",
#     TRUE     ~ "Not in relationship") |> as.factor(),
#   Education = recode_factor(Education_prep,
#                             "0" = "Basic school",
#                             "1" = "High school",
#                             "2" = "University"),
#   Gender = recode_factor(Gender_prep,
#                          "0" = "Male",
#                          "1" = "Female"),
#   Group2_prep    = as.factor(Group2_prep),
#   Group5_prep    = as.factor(Group5_prep),
#   Group2_value   = Group2_value,
#   Group5_value   = Group5_value
# )
#
# # ----- Quick multivariate test -------------------------------------------
# qqq <- mult.g.comp(groups      = c("Family_status", "Education", "Gender", "Group2_prep", "Group5_prep"),
#                    outcome.var = c("Age", "Work_years", "eps", "Group2_value", "Group5_value"),
#                    short_results = F,
#                    show_non_significant_results = T,
#                    diagnostics = F,
#                    df          = dat)
#
# qqq %>% print(n = 500)
#
# dat %>% fligner.test(Group2_prep ~ Group2_prep)
#
# dat %>%
#   sample_n(1000) %>%
#   .$Group2_value %>%
#   shapiro.test()
#
#...............................................................................................................
# #
# library(dplyr)
# library(broom)
# library(tidyverse)
# library(insight)
#
# set.seed(54854)
# x = rnorm(500,1,1)
# b0 = 1 # intercept chosen at your choice
# b1 = 1 # coef chosen at your choice
# h = function(x) 1+.4*x # h performs heteroskedasticity function (here
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
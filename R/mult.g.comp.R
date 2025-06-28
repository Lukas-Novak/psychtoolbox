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
#' @param show_non_significant_results if TRUE, Kruskal-Wallis p-value is reported for non-significant group comparisons, default is FALSE
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
#' When `show_non_significant_results = TRUE` and `short_results = FALSE`, the Kruskal-Wallis
#' test is reported with an H statistic, degrees of freedom, and p-value.
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
#' @importFrom rstatix dunn_test games_howell_test
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

mult.g.comp = function(df,outcome.var,groups, desc_only = FALSE, short_results = TRUE, remove_missings = FALSE, percent_decimals = 2, show_non_significant_results = FALSE) {

  # ----------- Helper Functions -----------
  desc.tab = function(groups, outcome.var, df) {
    factors.dat = df %>% select(where(is.factor)) %>% names()
    df %>%
      drop_na(all_of(groups)) %>%
      mutate(across(all_of(factors.dat), ~paste(as.numeric(.), .))) %>%
      pivot_longer(cols = all_of(groups),
                   names_to = "key",
                   values_to = "value") %>%
      group_by(key,value) %>%
      summarise(
        across(
          all_of(outcome.var),
          list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))
        ),
        n = n(),
        .groups = "drop"
      ) %>%
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
    if (any(str_detect(names(x), "Group difference"))) {

      if (remove_missings == TRUE) {
        x = x %>%
          filter(str_detect(value, "Missing", negate = TRUE))
      }

      x = x %>%
        remove_na_in_brackets(var = outcome.var) %>%
        mutate(value = replace_na(value, "Missing")) %>%
        mutate_if(is.numeric, round, 2) %>%
        mutate_all(~(replace(., is.na(.), ""))) %>%
        group_by(key) %>%
        mutate(across(contains("Group difference"), ~replace(., duplicated(.), ""))) %>%
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
  # ----------- End Helper Functions -----------


  # ----------- 1. Initial Data Preparation -----------
  descriptive_stats_raw = desc.tab(groups, outcome.var, df) %>%
    mutate(value = str_replace(value, "NA NA", "Missing"))

  if(sum(descriptive_stats_raw$n <= 1) >= 1 & desc_only == FALSE){
    stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
  }

  analysis_data = df %>%
    select(all_of(c(groups, outcome.var)))

  mean_sd_colnames = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names()

  outcome.var = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names() %>%
    str_replace_all(., "_mean", "") %>%
    str_replace_all(., "_sd", "") %>%
    unique()

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
    # ----------- 3. Main Analysis Block (REWRITTEN) -----------

    results_df <- tibble(key = character(), var = character(), result_string = character())

    analysis_data_prefixed <- analysis_data %>%
      mutate(across(where(is.factor), ~paste(as.numeric(.), .)))

    for (group_var in groups) {
      for (out_var in outcome.var) {

        formula <- as.formula(paste0("`", out_var, "` ~ `", group_var, "`"))
        current_data <- analysis_data_prefixed %>% select(all_of(c(out_var, group_var))) %>% drop_na()

        n_levels <- length(unique(current_data[[group_var]]))

        kw_test <- kruskal.test(formula, data = current_data)

        result_string <- NA_character_

        if (kw_test$p.value >= 0.05) {
          if (show_non_significant_results) {
            result_string <- if(short_results) {
              paste0("KW: ", format_p(kw_test$p.value))
            } else {
              paste0("H(", kw_test$parameter, ") = ", round(kw_test$statistic, 2), ", ", format_p(kw_test$p.value))
            }
          }
        } else {
          if (n_levels == 2) {
            result_string <- format_p(kw_test$p.value)
          } else { # n_levels > 2
            fligner_p <- fligner.test(formula, data = current_data)$p.value

            posthoc_res <- if (fligner_p >= 0.05) {
              rstatix::dunn_test(formula, data = current_data, p.adjust.method = "bonferroni")
            } else {
              rstatix::games_howell_test(formula, data = current_data)
            }

            sig_pairs <- posthoc_res %>% filter(p.adj < 0.05)

            if (nrow(sig_pairs) > 0) {
              if (short_results) {
                internal_string <- paste0(str_extract(sig_pairs$group1, "^.{1}"), "-", str_extract(sig_pairs$group2, "^.{1}"), format_p(sig_pairs$p.adj, stars_only = TRUE), collapse = ", ")
                posthoc_string <- paste0("(", internal_string, ")")
              } else {
                stat_char <- if ("statistic" %in% names(sig_pairs)) "z" else "t"
                stat_val <- if ("statistic" %in% names(sig_pairs)) sig_pairs$statistic else sig_pairs$estimate
                internal_string <- paste0(str_extract(sig_pairs$group1, "^.{1}"), "-", str_extract(sig_pairs$group2, "^.{1}"), ", ", stat_char, " = ", round(stat_val, 2), ", ", format_p(sig_pairs$p.adj), collapse = "; ")
                posthoc_string <- paste0("(", internal_string, ")")
              }
              result_string <- paste(format_p(kw_test$p.value), posthoc_string)
            } else {
              result_string <- format_p(kw_test$p.value)
            }
          }
        }

        if (!is.na(result_string)) {
          results_df <- results_df %>%
            add_row(key = group_var, var = out_var, result_string = result_string)
        }
      }
    }

    # ----------- 4. Final Table Assembly -----------
    if (nrow(results_df) > 0) {
      results_wide <- results_df %>%
        pivot_wider(names_from = var, values_from = result_string, names_glue = "{var} Group difference")

      final_results_table <- full_join(descriptive_stats_wide, results_wide, by = "key")
    } else {
      final_results_table <- descriptive_stats_wide
    }

    # Apply final formatting and shaping for presentation.
    table_to_return <- final_results_table %>%
      longer_tab()

    sort.names = table_to_return %>% select(any_of(c("variable", "n(%)")), ends_with("Group difference")) %>% names()
    table_to_return = table_to_return %>%
      relocate(all_of(sort.names))

    return(table_to_return)
  }
}

library(dplyr)
library(broom)
library(tidyverse)
library(insight)

set.seed(455454)
n <- 5001                              # sample size

# ----- Generate group variables ------------------------------------------
Gender_prep    <- rbinom(n, 1, 0.50)                     # 0 = Male, 1 = Female
Education_prep <- sample(0:2, n, replace = TRUE,         # 0 = Basic, 1 = High school, 2 = University
                         prob = c(.30, .40, .30))

# ----- Define strong group effects for numeric variables -----------------
# Females are significantly older; higher education adds further years.
Age <- rnorm(n,
             mean = 18 +
               Gender_prep * 8 +            # gender effect
               Education_prep * 6,          # education effect
             sd = 3)

# Work_years depends on Age, Gender, and Education (all shift the mean significantly).
Work_years <- rnorm(n,
                    mean = 1 +
                      Gender_prep * 4 +
                      Education_prep * 3 +
                      0.20 * Age,           # logical link to age
                    sd = 1)

# eps: we add group shifts and also heteroskedasticity
x <- rnorm(n, 1, 1)
h <- function(x) 1 + .4 * x                      # mild heteroskedasticity

eps <- rnorm(n,
             mean = -2 +
               Gender_prep * 1.5 +
               Education_prep * 1,
             sd = h(x))

# ----- Compile the final data frame --------------------------------------
dat <- tibble(
  eps          = eps,
  Gender_prep  = as.factor(Gender_prep),
  Age          = Age,
  Work_years   = Work_years,
  Education_prep = as.factor(Education_prep),
  Family_status  = case_when(
    Age > 30 ~ "Married",
    Age > 22 ~ "In relationship",
    TRUE     ~ "Not in relationship") |> as.factor(),
  Education = recode_factor(Education_prep,
                            "0" = "Basic school",
                            "1" = "High school",
                            "2" = "University"),
  Gender = recode_factor(Gender_prep,
                         "0" = "Male",
                         "1" = "Female")
)

# ----- Quick multivariate test -------------------------------------------
qqq <- mult.g.comp(groups      = c("Family_status", "Education", "Gender"),
                   outcome.var = c("Age", "Work_years", "eps"),
                   short_results = TRUE,
                   df          = dat)

qqq

qqq %>% view()    # View the output – differences should be significant across both Gender and Education


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
  Education_prep = as.factor(rbinom(n = 300, size = 4, prob = .5)),
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


results_table = mult.g.comp(groups = c("Family_status", "Education","Gender"),
                  outcome.var = c("Age","Work_years","eps"),short_results = F, show_non_significant_results = T,
                  df = dat)

results_table %>% view()


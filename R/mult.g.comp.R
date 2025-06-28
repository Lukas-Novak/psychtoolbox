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

mult.g.comp = function(df,outcome.var,groups, desc_only = FALSE, short_results = TRUE, remove_missings = FALSE, percent_decimals = 2, show_non_significant_results = FALSE) {

  # ----------- Helper Functions -----------

  # Calculates descriptive statistics (mean, sd, n, percent) for each outcome variable,
  # grouped by the levels of each grouping variable. It pivots the data into a long format
  # to perform this calculation efficiently across all groups.
  desc.tab = function(groups, outcome.var, df) {
    factors.dat = df %>% select(where(is.factor)) %>% names()
    df %>%
      # Use all_of() for robust handling of the 'groups' character vector
      drop_na(all_of(groups)) %>%
      mutate(across(all_of(factors.dat), ~paste(as.numeric(.), .))) %>%
      pivot_longer(cols = all_of(groups),
                   names_to = "key",
                   values_to = "value") %>%
      group_by(key,value) %>%
      # Use modern lambda syntax `\(x)` inside across for passing arguments like `na.rm`
      summarise(
        across(
          all_of(outcome.var),
          list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE))
        ),
        n = n(),
        .groups = "drop" # Explicitly drop grouping after summarising
      ) %>%
      mutate(percent =  as.character(round(n / sum(n)*100, digits = percent_decimals))) %>%
      ungroup() %>%
      mutate_all(~str_replace_all(., "NA NA|NaN|NA", NA_character_))
  }

  # A simple string cleaning utility to remove specific NA patterns from the final output table.
  remove_na_in_brackets <- function(x, var) {
    x = x %>%
      mutate(
        across(all_of(var), ~str_replace_all(., "\\(NA,\\)", "")),
        across(all_of(var), ~str_replace_all(., "NA+\\,", "")),
        across(all_of(var), ~str_replace_all(., "\\(NA\\)", "")),
        across(all_of(var), ~str_replace_all(., "NA", ""))
      )
  }

  # This function performs the final formatting of the results table.
  # It reshapes the wide results data into a long, presentable format and adds
  # header rows for each grouping variable to improve readability.
  longer_tab <- function(x) {
    # Test if the data frame contains any statistical results before proceeding.
    if (summarize(x,
                  contains_stat_tets_results = any(!is.na(across(contains("Group difference")))))$contains_stat_tets_results) {

      if (remove_missings == TRUE) {
        x = x %>%
          filter(str_detect(value, "Missing", negate = TRUE))
      }

      x = x %>%
        remove_na_in_brackets(var = outcome.var) %>%
        mutate(value = replace_na(value, "Missing")) %>%
        group_by(key) %>%
        filter(!if_any(ends_with(paste0(outcome.var)), duplicated)) %>%
        ungroup() %>%
        mutate(across(contains("Group difference"), ~ifelse(duplicated(.), "", .))) %>%
        mutate_if(is.numeric, round, 2) %>%
        mutate_all(~(replace(., is.na(.), ""))) %>%
        mutate(across(ends_with("Group difference"), ~replace(., duplicated(.), ""))) %>%
        # Add a blank header row for each group
        group_by(key) %>%
        group_modify(~add_row(., .before = 1)) %>%
        ungroup() %>%
        # Clean up duplicated keys and populate the header row
        mutate(across(ends_with("key"), ~replace(., duplicated(.), NA_character_))) %>%
        mutate(value = if_else(is.na(value), key, value)) %>%
        mutate_all(~replace(., is.na(.), "")) %>%
        # Create the final 'n (%)' column
        mutate(`n(%)` = paste0(as.numeric(n), "(",percent,")")) %>%
        mutate(`n(%)` = ifelse(str_detect(`n(%)`, "NA"), "", `n(%)`)) %>%
        select(-c("key","n","percent")) %>%
        rename_with(~paste0(outcome.var," M(SD)"), ends_with(outcome.var)) %>%
        rename("variable" = "value",
               "n(%)" = `n(%)`)
    } else {
      # Formatting for tables with only descriptive statistics
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

  # Helper to clean up strings that may have multiple sets of parentheses
  # as a result of post-hoc test formatting.
  removing_nested_prentecies <- function(x) {
    success <- FALSE
    while (!success) {
      x = x %>%
        mutate_all(~stringr::str_remove_all(., "\\)(?=.*\\))")) %>%
        mutate_all(~stringr::str_replace(., "\\((.*)\\(", "(\\1"))
      success <- x %>% summarise(across(everything(), ~stringr::str_count(., "\\(") >= 2)) %>% any(isTRUE(.),na.rm = T) == FALSE
    }
    return(x)
  }
  # ----------- End Helper Functions -----------


  # ----------- 1. Initial Data Preparation -----------
  # Generate the primary descriptive statistics table.
  descriptive_stats_raw = desc.tab(groups, outcome.var, df) %>%
    mutate(value = str_replace(value, "NA NA", "Missing"))

  # Data validation: Ensure there are enough observations for statistical tests.
  if(sum(descriptive_stats_raw$n <= 1) >= 1 & desc_only == FALSE){
    stop("There is less than 1 observation in some factor level, please remove it or merge to another factor level")
  }

  # Select only the relevant columns for analysis, using all_of() for safety.
  analysis_data = df %>%
    select(all_of(c(groups, outcome.var)))

  # Identify the mean/sd columns for reshaping.
  mean_sd_colnames = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names()

  # Dynamically determine the outcome variables that are actually present in the data.
  outcome.var = descriptive_stats_raw %>%
    select(ends_with(c("_mean","_sd"))) %>%
    names() %>%
    str_replace_all(., "_mean", "") %>%
    str_replace_all(., "_sd", "") %>%
    unique()

  # Reshape descriptive stats from long format (var_mean, var_sd) to a wide format
  # with a single 'M (SD)' column for each outcome variable. This is the base table
  # to which statistical results will be joined.
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
  # If the user only wants descriptive statistics, format and return the table now.
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

    # Identify factor and numeric variables for the analysis loops.
    factors.dat = analysis_data %>% select(where(is.factor)) %>% names()
    output.var = analysis_data %>% select(where(is.numeric)) %>% names()

    data_with_factors_as_strings = analysis_data %>%
      mutate(across(all_of(factors.dat), ~paste(as.numeric(.), .)))

    ## ----------- 3a. Two-Group Comparison -----------
    # Isolate data for variables with exactly two factor levels.
    data_long_2_groups = analysis_data %>%
      select_if(~ nlevels(.) == 2 | is.numeric(.)) %>%
      mutate(across(c(where(is.factor)), ~ as.factor(str_replace_all(as.factor(paste(as.numeric(.), .)), "NA NA", NA_character_)))) %>%
      pivot_longer(cols = c(where(is.factor)),
                   names_to = "key",
                   values_to = "value")

    # Perform assumption checks (homogeneity of variances and normality).
    homogeneity_test_2_groups = data_long_2_groups %>%
      group_by(key) %>%
      summarise(across(all_of(output.var), ~fligner.test(., value)$p.value)) %>%
      pivot_longer(all_of(output.var),
                   names_to = "names_continous_var",
                   values_to = "p_val_homo")

    normality_test_2_groups = data_long_2_groups %>%
      group_by(key) %>%
      reframe(across(all_of(output.var), function(x) {
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

    # Run Kruskal-Wallis on all 2-group combinations to get an omnibus p-value.
    # Use reframe() as kruskal.test() %>% tidy() returns a 1-row data frame per group.
    kruskal_summary_2_groups_all = data_long_2_groups %>%
      group_by(key) %>%
      reframe(across(all_of(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
      pivot_longer(all_of(output.var),
                   names_to = "names_continous_var",
                   values_to = "stat") %>%
      full_join(homogeneity_test_2_groups, by = c("key", "names_continous_var")) %>%
      full_join(normality_test_2_groups, by = c("key", "names_continous_var")) %>%
      as.matrix() %>%
      as_tibble() %>%
      mutate(stat.p.value = as.numeric(stat.p.value),
             p_val_homo = as.numeric(p_val_homo),
             p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value))

    # If requested, filter for non-significant results (p >= 0.05) and format them.
    if (show_non_significant_results == TRUE) {
      non_sig_kw_2_groups <- kruskal_summary_2_groups_all %>%
        filter(stat.p.value >= 0.05)

      if (nrow(non_sig_kw_2_groups) > 0) {
        # Use a standard if/else for the scalar `short_results` condition.
        if (short_results == FALSE) {
          aggregated_kw_2_groups_results <- non_sig_kw_2_groups %>%
            mutate(`Group comparison` = paste0("H(", stat.parameter, ") = ", round(as.numeric(stat.statistic), 2), ", ", format_p(stat.p.value))) %>%
            mutate(names_continous_var = paste0(names_continous_var, " Group difference")) %>%
            select(key, names_continous_var, `Group comparison`) %>%
            pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
        } else {
          aggregated_kw_2_groups_results <- non_sig_kw_2_groups %>%
            mutate(`Group comparison` = paste0("KW: ", format_p(stat.p.value))) %>%
            mutate(names_continous_var = paste0(names_continous_var, " Group difference")) %>%
            select(key, names_continous_var, `Group comparison`) %>%
            pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
        }
      }
    }

    # Filter for *significant* results (p < 0.05) to determine which post-hoc tests are needed.
    kruskal_summary_2_groups = kruskal_summary_2_groups_all %>%
      filter(stat.p.value < 0.05) %>%
      mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
             non_homo_normal = p_val_homo < 0.05)

    grouped_data_2_groups =  data_long_2_groups %>%
      group_by(key)

    # Perform Wilcoxon Test for non-normal, homoscedastic data.
    if (any(kruskal_summary_2_groups$homo_non_normal == TRUE)) {
      vars_for_wilcox=filter(.data = kruskal_summary_2_groups, homo_non_normal == TRUE)
      # Use reframe() as rstatix tests can return multiple rows per group.
      results_wilcox = data_long_2_groups %>%
        group_by(key) %>%
        reframe(across(all_of(output.var), ~rstatix::wilcox_test(. ~ value, data = grouped_data_2_groups, p.adjust.method = "bonferroni"))) %>%
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
        summarise("Group comparison" = paste(results_agregated, collapse = ", "), .groups = "drop") %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    # Perform Welch's T-test for non-homoscedastic data.
    if (any(kruskal_summary_2_groups$non_homo_normal == TRUE)) {
      vars_for_welch=filter(.data = kruskal_summary_2_groups, non_homo_normal == TRUE)
      results_welch = data_long_2_groups %>%
        group_by(key) %>%
        reframe(across(all_of(output.var), ~rstatix::t_test(. ~value, var.equal = FALSE, data = grouped_data_2_groups, p.adjust.method = "bonferroni"))) %>%
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
        summarise("Group comparison" = paste(results_agregated, collapse = ", "), .groups = "drop") %>%
        select(key, `Group comparison`,names_continous_var) %>%
        mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
        pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
    }

    # Combine Welch and Wilcox results if both were performed.
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

    # Merge non-significant KW results with the significant post-hoc results.
    # `bind_rows` is appropriate because the results are for mutually exclusive variable combinations.
    if(exists("aggregated_kw_2_groups_results")) {
      if (exists("aggregated_wilcox_results")) {
        aggregated_wilcox_results <- dplyr::bind_rows(aggregated_wilcox_results, aggregated_kw_2_groups_results)
      } else {
        aggregated_wilcox_results <- aggregated_kw_2_groups_results
      }
    }


    ## ----------- 3b. Multi-Group (>2) Comparison -----------
    # Check if any variables have more than two factor levels.
    if (analysis_data %>% select_if(~ nlevels(.) > 2) %>% length() == 0) {
      print("It seems that groups you selected does not contain more than 2 levels, skipping multigroup analysis.....")
    } else if (analysis_data %>% select_if(~ nlevels(.) > 2) %>% length() > 0) {
      print("Groups you selected contains more than two groups, analysing........")

      # Prepare data, similar to the 2-group case.
      data_long_multi_groups = analysis_data %>%
        select_if(~ nlevels(.) > 2 | is.numeric(.)) %>%
        mutate(across(c(where(is.factor)), ~ as.factor(str_replace_all(as.factor(paste(as.numeric(.), .)), "NA NA", NA_character_)))) %>%
        pivot_longer(cols = c(where(is.factor)),
                     names_to = "key",
                     values_to = "value")

      # Assumption checks.
      homogeneity_test_multi_groups = data_long_multi_groups %>%
        group_by(key) %>%
        summarise(across(all_of(output.var), ~fligner.test(., value)$p.value)) %>%
        pivot_longer(all_of(output.var),
                     names_to = "names_continous_var",
                     values_to = "p_val_homo")

      normality_test_multi_groups = data_long_multi_groups %>%
        group_by(key) %>%
        reframe(across(all_of(output.var), ~ {
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

      # Omnibus Kruskal-Wallis test.
      kruskal_summary_multi_groups_all = data_long_multi_groups %>%
        group_by(key) %>%
        reframe(across(all_of(output.var), ~kruskal.test(. ~ value) %>% tidy)) %>%
        pivot_longer(all_of(output.var),
                     names_to = "names_continous_var",
                     values_to = "stat") %>%
        full_join(homogeneity_test_multi_groups, by = c("key", "names_continous_var")) %>%
        full_join(normality_test_multi_groups, by = c("key", "names_continous_var")) %>%
        as.matrix() %>% as_tibble() %>%
        mutate(stat.p.value = as.numeric(stat.p.value),
               p_val_homo = as.numeric(p_val_homo),
               p_val_shapiro.p.value = as.numeric(p_val_shapiro.p.value))

      # Format non-significant results if requested.
      if (show_non_significant_results == TRUE) {
        non_sig_kw_multi_groups <- kruskal_summary_multi_groups_all %>%
          filter(stat.p.value >= 0.05)

        if (nrow(non_sig_kw_multi_groups) > 0) {
          if (short_results == FALSE) {
            aggregated_kw_multi_groups_results <- non_sig_kw_multi_groups %>%
              mutate(`Group comparison` = paste0("H(", stat.parameter, ") = ", round(as.numeric(stat.statistic), 2), ", ", format_p(stat.p.value))) %>%
              mutate(names_continous_var = paste0(names_continous_var, " Group difference")) %>%
              select(key, names_continous_var, `Group comparison`) %>%
              pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
          } else {
            aggregated_kw_multi_groups_results <- non_sig_kw_multi_groups %>%
              mutate(`Group comparison` = paste0("KW: ", format_p(stat.p.value))) %>%
              mutate(names_continous_var = paste0(names_continous_var, " Group difference")) %>%
              select(key, names_continous_var, `Group comparison`) %>%
              pivot_wider(names_from = names_continous_var, values_from = `Group comparison`)
          }
        }
      }

      # Filter for significant results to determine post-hoc tests.
      kruskal_summary_multi_groups = kruskal_summary_multi_groups_all %>%
        filter(stat.p.value < 0.05) %>%
        mutate(homo_non_normal = p_val_homo > 0.05 & p_val_shapiro.p.value < 0.05,
               non_homo_normal = p_val_homo < 0.05)

      grouped_data_multi_groups = data_long_multi_groups %>%
        group_by(key)

      # Perform Dunn's Test for non-normal, homoscedastic data.
      if (any(kruskal_summary_multi_groups$homo_non_normal == TRUE)) {
        vars_for_dunn=filter(.data = kruskal_summary_multi_groups, homo_non_normal == TRUE)
        results_dunn = data_long_multi_groups %>%
          group_by(key) %>%
          reframe(across(all_of(output.var), ~rstatix::dunn_test(. ~value, data = grouped_data_multi_groups, detailed = T, p.adjust.method = "bonferroni"))) %>%
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
          summarise("Group comparison" = paste0(results_agregated, collapse = ","), .groups = "drop") %>%
          select(key, `Group comparison`,names_continous_var) %>%
          mutate(names_continous_var = paste0(names_continous_var," Group difference")) %>%
          pivot_wider(names_from = names_continous_var, values_from = `Group comparison`) %>%
          mutate_all(~str_replace_all(., "\\,+[:blank:]", ","))

        if (short_results == TRUE) {
          aggregated_dunn_results <- aggregated_dunn_results %>% removing_nested_prentecies()
        }
      }

      # Perform Games-Howell Test for non-homoscedastic data.
      if (any(kruskal_summary_multi_groups$non_homo_normal == TRUE)) {
        vars_for_games_howell=filter(.data = kruskal_summary_multi_groups, non_homo_normal == TRUE)
        results_games_howell = data_long_multi_groups %>%
          group_by(key) %>%
          reframe(across(all_of(output.var), ~rstatix::games_howell_test(. ~value, data = grouped_data_multi_groups, detailed = T))) %>%
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
          summarise("Group comparison" = paste0(results_agregated, collapse = ", "), .groups = "drop") %>%
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
    # Collect all multi-group result data frames (Dunn, G-H, non-sig KW) into a list.
    multigroup_results_list <- list()
    if(exists("aggregated_dunn_results")) {
      multigroup_results_list <- c(multigroup_results_list, list(aggregated_dunn_results))
    }
    if(exists("aggregated_games_howell_results")) {
      multigroup_results_list <- c(multigroup_results_list, list(aggregated_games_howell_results))
    }
    if(exists("aggregated_kw_multi_groups_results")) {
      multigroup_results_list <- c(multigroup_results_list, list(aggregated_kw_multi_groups_results))
    }

    # Combine the result tables if any exist. `bind_rows` works because each result
    # type is for a mutually exclusive key-variable pair. Then coalesce the results.
    if(length(multigroup_results_list) > 0) {
      combined_multigroup_results <- dplyr::bind_rows(multigroup_results_list) %>%
        group_by(key) %>%
        summarise(across(everything(), ~ na.omit(.)[1]), .groups = "drop")

      # Join the combined test results with descriptive statistics.
      results_multigroup_tests <- full_join(combined_multigroup_results, descriptive_stats_wide, by = intersect(names(combined_multigroup_results), names(descriptive_stats_wide)))
    } else {
      # If no multi-group tests were run, the results table is just the descriptive stats.
      results_multigroup_tests <- descriptive_stats_wide
    }

    results_multigroup_tests <- results_multigroup_tests  %>%
      mutate(
        across(ends_with("Group difference"), ~replace(., duplicated(.), "")),
        across(ends_with("Group difference"), ~replace(., is.na(.), ""))
      )

    # Combine with 2-group results, filtering for the correct grouping variables.
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
    # This final block determines which of the generated tables to return based on which
    # tests were run and yielded results. It aims to find the most complete table available.
    # The logic is complex due to the various possible combinations of test outcomes.
    if(exists("table_wilcox_and_multigroup") & exists("table_both_2group_and_multigroup")) {
      table_wilcox_and_multigroup = table_wilcox_and_multigroup %>%
        full_join(table_both_2group_and_multigroup) %>%
        mutate(across(contains("Group difference"), ~ifelse(is.na(.), "", .)))

      # Relocate columns for better presentation.
      sort.names = table_wilcox_and_multigroup %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()

      table_wilcox_and_multigroup = table_wilcox_and_multigroup %>%
        relocate(all_of(sort.names))
      return(table_both_2group_and_multigroup)

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

      # Return formatted table if only multi-group tests were run/significant.
      if(!exists("aggregated_wilcox_results") & !exists("aggregated_welch_results")) {
        table_multigroup_only_formatted <- results_multigroup_tests %>%
          longer_tab()

        sort.names = table_multigroup_only_formatted %>% select(ends_with(c("variable","n(%)","Group difference"))) %>% names()
        table_multigroup_only_formatted = table_multigroup_only_formatted %>%
          relocate(all_of(sort.names))
        return(table_multigroup_only_formatted)
      }

      # Fallback for other specific cases.
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

# # -------------------------------------------------------------------------------------------------
# # CODE EXAMPLES -----------------------------------------------------------------------------------
# # -------------------------------------------------------------------------------------------------
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
# qqq = mult.g.comp(groups = c("Family_status", "Education","Gender"),
#                   outcome.var = c("Age","Work_years","eps"),show_non_significant_results = T,short_results = T,
#                   df = dat)
#
# qqq %>% view()
#


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
                   show_non_significant_results = TRUE,
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


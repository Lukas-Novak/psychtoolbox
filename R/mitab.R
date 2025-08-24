#......................................................
# Documentation
#' Measurement invariance table
#'
#' @param group1_nam name of the first group. Used if per_group_models is TRUE.
#' @param group2_nam name of the second group. Used if per_group_models is TRUE.
#' @param ordered logical, if set to TRUE items will be treated as ordered variables
#' @param model lavaan model to test
#' @param data data frame or tibble
#' @param std.lv logical, if TRUE, the latent variables are standardized.
#' @param meanstructure logical, if TRUE, the model includes mean structures.
#' @param group name of grouping variable
#' @param yes_no_results logical, if TRUE, a column indicating significant model differences is added, based on CFI and RMSEA criteria.
#' @param estimator name of estimator to be used during fitting procedure (e.g., "MLR", "WLSMV").
#' @param robust logical, if TRUE, robust fit statistics are extracted. This is recommended for estimators like MLR or WLSMV.
#' @param cfi.difference logical, if TRUE, the change in CFI between nested models is included in the output.
#' @param rmsea.difference logical, if TRUE, the change in RMSEA between nested models is included in the output.
#' @param per_group_models logical, if TRUE, single-group models are fitted for each group and included in the output table. Defaults to FALSE.
#' @param ... optional arguments to be passed to the lavaan::cfa function.
#'
#' @return A tibble containing a formatted summary of the measurement invariance testing results.
#'
#' @keywords MI, measurement equivalence, invariance of a measurement
#' @details This function automates measurement invariance testing using lavaan and presents the results in a clear, formatted table. It uses standard lavaan functions for all model fitting.
#' @references Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes for testing measurement invariance. Structural Equation Modeling, 9(2), 233-255.
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#'
#' @importFrom dplyr mutate select rename relocate as_tibble
#' @importFrom insight format_p
#' @importFrom stringr str_detect
#' @importFrom lavaan cfa fitMeasures
#'
#' @examples
#' # The famous Holzinger and Swineford (1939) example
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#'
#' library(lavaan)
#' dat <- HolzingerSwineford1939
#
#' # Example with a robust ML estimator (MLR)
#' res.tab.mlr <- mitab(
#'   group1_nam = "Grant-White",
#'   group2_nam = "Pasteur",
#'   ordered = FALSE,
#'   model = HS.model,
#'   data = dat,
#'   std.lv = TRUE,
#'   meanstructure = TRUE,
#'   group = "school",
#'   yes_no_results = TRUE,
#'   estimator = "MLR",
#'   robust = TRUE,
#'   cfi.difference = TRUE,
#'   rmsea.difference = TRUE,
#'   per_group_models = TRUE # Optionally show per-group fits
#' )
#'
#' print(res.tab.mlr)
#'
#' # Example with an ordered dataset and the WLSMV estimator
#' set.seed(123)
#' ord_data <- HolzingerSwineford1939
#' ord_data[, paste0("x", 1:9)] <- lapply(ord_data[, paste0("x", 1:9)],
#'                                        function(z) as.integer(cut(z, breaks = 4)))
#'
#' res.tab.wlsmy <- mitab(
#'   group1_nam = "Grant-White",
#'   group2_nam = "Pasteur",
#'   ordered = TRUE,
#'   model = HS.model,
#'   data = ord_data,
#'   std.lv = TRUE,
#'   meanstructure = TRUE,
#'   group = "school",
#'   yes_no_results = TRUE,
#'   estimator = "WLSMV",
#'   robust = TRUE, # Using robust=TRUE is recommended for WLSMV
#'   cfi.difference = TRUE,
#'   rmsea.difference = TRUE
#' )
#' print(res.tab.wlsmy)
#' @export
#......................................................
mitab = function(group1_nam, group2_nam, ordered, model, data, std.lv, meanstructure, group, yes_no_results, estimator, robust = FALSE, cfi.difference = TRUE, rmsea.difference = TRUE, per_group_models = FALSE, ...) {
  # --- Input Validation ---
  if(!is.factor(data[[group]])) stop("The 'group' argument has to be type factor" )
  if(!is.data.frame(data)) stop('data must be a data frame')

  # --- 1. Determine which fit indices to extract based on robust flag and estimator ---
  fit_names <- c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')

  if (robust == TRUE) {
    if (estimator %in% c("MLR", "MLM")) {
      fit_names <- c('chisq.scaled', 'df.scaled', 'pvalue.scaled', 'cfi.robust', 'tli.robust', 'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    }
    else if (estimator %in% c("WLSMV", "DWLS")) {
      fit_names <- c('chisq.scaled', 'df.scaled', 'pvalue.scaled', 'cfi.robust', 'tli.robust', 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled', 'srmr')
    }
  }

  # --- 2. Fit all required lavaan models ---
  message("Fitting measurement invariance models...")

  fit.baseline <- lavaan::cfa(model, data = data, ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
  fit.configural <- lavaan::cfa(model, data = data, group = group, ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
  fit.metric <- lavaan::cfa(model, data = data, group = group, group.equal = "loadings", ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
  fit.scalar <- lavaan::cfa(model, data = data, group = group, group.equal = c("loadings", "intercepts"), ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
  fit.strict <- lavaan::cfa(model, data = data, group = group, group.equal = c("loadings", "intercepts", "residuals"), ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)

  if (per_group_models) {
    message("Fitting per-group models...")
    group_levels <- levels(data[[group]])
    data_g1 <- data[data[[group]] == group_levels[1], ]
    data_g2 <- data[data[[group]] == group_levels[2], ]

    fit.g1 <- lavaan::cfa(model, data = data_g1, ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
    fit.g2 <- lavaan::cfa(model, data = data_g2, ordered = ordered, std.lv = std.lv, meanstructure = meanstructure, estimator = estimator, ...)
  }
  message("All models fitted. Assembling table...")

  # --- 3. Assemble the results table ---
  results_list <- list()
  results_list$baseline <- c("Overall model", round(lavaan::fitMeasures(fit.baseline, fit_names), 3))

  if (per_group_models) {
    results_list$g1 <- c(group1_nam, round(lavaan::fitMeasures(fit.g1, fit_names), 3))
    results_list$g2 <- c(group2_nam, round(lavaan::fitMeasures(fit.g2, fit_names), 3))
  }

  results_list$configural <- c("Configural  model", round(lavaan::fitMeasures(fit.configural, fit_names), 3))
  results_list$metric     <- c("Metric  model", round(lavaan::fitMeasures(fit.metric, fit_names), 3))
  results_list$scalar     <- c("Scalar  model", round(lavaan::fitMeasures(fit.scalar, fit_names), 3))
  results_list$strict     <- c("Strict  model", round(lavaan::fitMeasures(fit.strict, fit_names), 3))

  tab.fit <- as.data.frame(do.call(rbind, results_list), stringsAsFactors = FALSE)
  colnames(tab.fit) <- c("Model", "x2", "df", "pvalue", "CFI", "TLI", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "SRMR")

  # --- 4. Post-processing and formatting ---
  tab.fit <- tab.fit %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      rmsea2 = as.numeric(rmsea),
      rmsea = paste0(rmsea, " (", rmsea.ci.lower, "-", rmsea.ci.upper, ")")
    ) %>%
    dplyr::rename("RMSEA (90% CI)" = "rmsea") %>%
    dplyr::select(-"rmsea.ci.lower", -"rmsea.ci.upper")

  # --- Helper function for CFI difference ---
  cfi.dif <- function(x) {
    x <- x %>%
      dplyr::mutate(
        CFI.dif = c(NA, abs(round(diff(as.numeric(CFI)), digits = 3))),
        pvalue = insight::format_p(as.numeric(pvalue))
      ) %>%
      dplyr::relocate("CFI.dif", .after = "CFI")

    if(yes_no_results == TRUE) {
      x <- x %>% dplyr::mutate(mod.dif = ifelse(!is.na(CFI.dif) & CFI.dif > 0.01, "Yes", "No"))
    }
    if(any(names(x) == 'mod.dif')) {
      x <- x %>% dplyr::rename("Model difference - CFI" = "mod.dif")
    }
    if(cfi.difference == TRUE) {
      x <- x %>% dplyr::rename("delta CFI" = "CFI.dif")
    } else {
      x <- x %>% dplyr::select(-"CFI.dif")
    }
    return(x)
  }
  tab.fit <- tab.fit %>% cfi.dif()

  # --- Helper function for RMSEA difference ---
  rmsea.dif <- function(x) {
    x <- x %>%
      dplyr::mutate(RMSEA.dif = c(NA, abs(round(diff(as.numeric(rmsea2)), digits = 3)))) %>%
      dplyr::relocate("RMSEA.dif", .after = `RMSEA (90% CI)`)

    if(yes_no_results == TRUE) {
      x <- x %>% dplyr::mutate(mod.dif = ifelse(!is.na(RMSEA.dif) & RMSEA.dif > 0.015, "Yes", "No"))
    }
    if(any(names(x) == 'mod.dif')) {
      x <- x %>% dplyr::rename("Model difference - RMSEA" = "mod.dif")
    }
    if(rmsea.difference == TRUE) {
      x <- x %>% dplyr::rename("delta RMSEA" = "RMSEA.dif")
    } else {
      x <- x %>% dplyr::select(-"RMSEA.dif")
    }
    return(x)
  }
  tab.fit <- tab.fit %>%
    rmsea.dif() %>%
    dplyr::select(-"rmsea2")

  return(tab.fit)
}

## The famous Holzinger and Swineford (1939) example
# HS.model <- ' visual  =~ x1 + x2 + x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9 '
#
# mitab(group1_nam = "Grant-White",
#                 group2_nam = "Pasteur",
#                 ordered = FALSE,
#                 model = HS.model,
#                 data = lavaan::HolzingerSwineford1939,
#                 std.lv = T,
#                 meanstructure = TRUE,
#                 group = "school",
#                 yes_no_results = TRUE,
#                 estimator = "MLR",
#                 robust = TRUE,
#                 cfi.difference = TRUE,
#                 rmsea.difference = TRUE)

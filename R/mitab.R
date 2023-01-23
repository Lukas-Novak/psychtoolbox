#......................................................
# Documentation
#' Measurement invariance table
#'
#' @param group1_nam name of the first group
#' @param group2_nam name of the second group
#' @param ordered logical, if set to TRUE items will be treated as ordered variables
#' @param model lavaan model to test
#' @param data data frame or tibble
#' @param std.lv logical, if TRUE than standardized loadings are stored in temporal output
#' @param meanstructure logical, if TRUE than model with meanstructure is estimated
#' @param group name of grouping variable
#' @param yes_no_results logical, if TRUE than lasy output indicating difference between models is added, currently working only based on CFI and RMSEA
#' @param estimator name of estimator to be used during fitting procedure
#' @param robust logical, if TRUE, than robust results are printed, working only with estimators  providing robust results (e.g. MLR or DWLS)
#' @param cfi.difference logical, if TRUE, delta of the CFI is printed in output
#' @param rmsea.difference logical, if TRUE, delta of the RMSEA is printed in output
#'
#'
#' @return data frame
#  @value data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @keywords MI, measurement equivalence, invariance of a measurement
#' @details This function creates table with the key output from measurement invariance testing.
#' @references Myles Hollander and Douglas A. Wolfe (1973). Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 27--33 (one-sample), 68--75 (two-sample).
#' Or second edition (1999).
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr relocate
#' @importFrom dplyr as_tibble
#' @importFrom insight format_p
#' @importFrom stringr str_detect
#' @importFrom lavaan cfa
#' @importFrom lavaan fitMeasures
#' @importFrom equaltestMI eqMI.main
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
#' res.tab.mi <- mitab(
#' group1_nam = "Grant-White",
#' group2_nam = "Pasteur",
#' ordered = FALSE,
#' model = HS.model,
#' data = dat,
#' std.lv = TRUE,
#' meanstructure = TRUE,
#' group = "school",
#' yes_no_results = TRUE,
#' estimator = "MLR",
#' robust = TRUE,
#' cfi.difference = TRUE)
#'
#' print(res.tab.mi)
#' @export
#......................................................
mitab = function(group1_nam, group2_nam, ordered, model, data, std.lv, meanstructure, group, yes_no_results, estimator, robust = FALSE, cfi.difference = FALSE, rmsea.difference = FALSE) {
  if(!is.factor(data[[group]])) stop("The 'group' argument has to be type factor" ) # group has to be factor in order to correctly name g.1 and g.2
  if(!is.data.frame(data)) stop('data must be a data frame')

  if(robust == FALSE) {
    meas.invar.cfa=cfa(model, data = data, ordered = ordered, std.lv= std.lv,
                       meanstructure = meanstructure, estimator = estimator)

    tab.fit = matrix(nrow = 7, ncol = 10)

    colnames(tab.fit)=c("Model","x2","df","pvalue","CFI","TLI","rmsea","rmsea.ci.lower", "rmsea.ci.upper", "SRMR")

    tab.fit[1, ] = c("Overall model", round(fitMeasures(meas.invar.cfa,
                                                        c('chisq', 'df', 'pvalue',
                                                          'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                          "rmsea.ci.upper", 'srmr')), digits = 3))


    # results of this function are the same as from the lavaan
    mult.group.m = eqMI.main(model = model,
                             data = data,
                             group = group,
                             meanstructure = meanstructure,
                             output = "both",
                             equivalence.test = T,
                             adjRMSEA = T,
                             projection = T,
                             ordered = ordered,
                             estimator = estimator)

    # male table
    tab.fit[2, ] = c(group1_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g1,
                                                   c('chisq', 'df', 'pvalue',
                                                     'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                     "rmsea.ci.upper", 'srmr')), digits = 3))
    # female table
    tab.fit[3, ] = c(group2_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g2,
                                                   c('chisq', 'df', 'pvalue',
                                                     'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                     "rmsea.ci.upper", 'srmr')), digits = 3))

    # Configure invariance
    tab.fit[4, ] = c("Configural  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.combine.groups,
                                                            c('chisq', 'df', 'pvalue',
                                                              'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                              "rmsea.ci.upper", 'srmr')), digits = 3))
    # Metric invariance
    tab.fit[5, ] = c("Metric  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.metric,
                                                        c('chisq', 'df', 'pvalue',
                                                          'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                          "rmsea.ci.upper", 'srmr')), digits = 3))
    # Scalar invariance
    tab.fit[6, ] = c("Scalar  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.scalar,
                                                        c('chisq', 'df', 'pvalue',
                                                          'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                          "rmsea.ci.upper", 'srmr')), digits = 3))
    # Strict (error) invariance
    tab.fit[7, ] = c("Strict  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.strict.residuals,
                                                        c('chisq', 'df', 'pvalue',
                                                          'cfi',"tli",'rmsea',"rmsea.ci.lower",
                                                          "rmsea.ci.upper", 'srmr')), digits = 3))

    # mult.group.m$eqMI.stat # checking chi-square

    tab.fit = tab.fit %>%
      as_tibble() %>%
      mutate(rmsea = paste0(rmsea, " ","(",rmsea.ci.lower,"-",rmsea.ci.upper,")")) %>%
      rename(
        "RMSEA (90% CI)" = "rmsea") %>%
      select(!starts_with(c("rmsea.ci.","rmsea.ci.lower","rmsea.ci.upper")))
  }
  if(robust == TRUE) {
    meas.invar.cfa=cfa(model, data = data, ordered = ordered, std.lv= std.lv,
                       meanstructure = meanstructure, estimator = estimator)

    tab.fit = matrix(nrow = 7, ncol = 10)

    colnames(tab.fit)=c("Model","x2","df","pvalue","CFI","TLI","rmsea","rmsea.ci.lower", "rmsea.ci.upper", "SRMR")

    tab.fit[1, ] = c("Overall model", round(fitMeasures(meas.invar.cfa,
                                                        c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                          'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                          "rmsea.ci.upper.scaled", 'srmr')), digits = 3))


    # results of this function are the same as from the lavaan
    mult.group.m = eqMI.main(model = model,
                             data = data,
                             group = group,
                             meanstructure = meanstructure,
                             output = "both",
                             equivalence.test = T,
                             adjRMSEA = T,
                             projection = T,
                             ordered = ordered,
                             estimator = estimator)

    # male table
    tab.fit[2, ] = c(group1_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g1,
                                                   c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                     'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                     "rmsea.ci.upper.scaled", 'srmr')), digits = 3))
    # female table
    tab.fit[3, ] = c(group2_nam, round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.configural.g2,
                                                   c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                     'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                     "rmsea.ci.upper.scaled", 'srmr')), digits = 3))

    # Configure invariance
    tab.fit[4, ] = c("Configural  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.combine.groups,
                                                            c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                              'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                              "rmsea.ci.upper.scaled", 'srmr')), digits = 3))
    # Metric invariance
    tab.fit[5, ] = c("Metric  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.metric,
                                                        c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                          'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                          "rmsea.ci.upper.scaled", 'srmr')), digits = 3))
    # Scalar invariance
    tab.fit[6, ] = c("Scalar  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.scalar,
                                                        c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                          'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                          "rmsea.ci.upper.scaled", 'srmr')), digits = 3))
    # Strict (error) invariance
    tab.fit[7, ] = c("Strict  model", round(fitMeasures(mult.group.m$convention.sem$LavaanOut$fit.strict.residuals,
                                                        c('chisq.scaled', 'df.scaled', 'pvalue.scaled',
                                                          'cfi.scaled',"tli.scaled",'rmsea.scaled',"rmsea.ci.lower.scaled",
                                                          "rmsea.ci.upper.scaled", 'srmr')), digits = 3))

    # mult.group.m$eqMI.stat # checking chi-square
    tab.fit = tab.fit %>%
      as_tibble() %>%
      mutate(
        rmsea2 = rmsea,
        rmsea = paste0(rmsea, " ","(",rmsea.ci.lower,"-",rmsea.ci.upper,")")) %>%
      rename(
        "RMSEA (90% CI)" = "rmsea") %>%
      select(!starts_with(c("rmsea.ci.","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled")))
  }

  cfi.dif = function(x) {
    if(!is.data.frame(x)) stop('x must be a data frame')
    if(length(x) < 1) stop('must be higher length than 1')
    if(!sum(str_detect(names(x),"CFI"))) stop("There must be a column with CFI values")
    x = x %>%
      tibble("CFI.dif" = c(NA, round(diff(as.numeric(x$CFI)),digits = 3))) %>%
      relocate("CFI.dif", .after = "CFI") %>%
      mutate(
        pvalue = format_p(as.numeric(pvalue))
      )
    if(yes_no_results == TRUE) {
      x = x %>% mutate("mod.dif" = ifelse(CFI.dif > 0.01, "Yes", "No"))
    }
    if(any(names(x) == 'mod.dif')) {x = x %>% rename("Model difference - CFI" = "mod.dif")
    }
    x = x %>%
      rename("CFI difference" = "CFI.dif")
    if(cfi.difference == FALSE)
      x = x %>%
      select(!"CFI difference")
    print(x)
    if(cfi.difference == TRUE)
      x = x %>%
      rename("delta CFI" = "CFI difference")
    print(x)
  }
  tab.fit = tab.fit %>%
    cfi.dif()

  rmsea.dif = function(x) {
    if(!is.data.frame(x)) stop('x must be a data frame')
    if(length(x) < 1) stop('must be higher length than 1')
    if(!sum(str_detect(names(x),"rmsea2"))) stop("There must be a column with rmsea2 values")
    x = x %>%
      tibble("RMSEA.dif" = c(NA, round(diff(as.numeric(rmsea2)),digits = 3))) %>%
      relocate("RMSEA.dif", .after = `RMSEA (90% CI)`)
    if(yes_no_results == TRUE) {
      x = x %>% mutate("mod.dif" = ifelse(RMSEA.dif > 0.015, "Yes", "No"))
    }
    if(any(names(x) == 'mod.dif')) {
      x = x %>% rename("Model difference - RMSEA" = "mod.dif")
    }
    x = x %>%
      rename("RMSEA difference" = "RMSEA.dif")
    if(rmsea.difference == FALSE)
      x = x %>%
      select(!"RMSEA difference")
    print(x)
    if(rmsea.difference == TRUE)
      x = x %>%
      rename("delta RMSEA" = "RMSEA difference")
    print(x)
  }
  tab.fit = tab.fit %>%
    rmsea.dif() %>%
    select(-rmsea2)
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

# Documentation
#' Lasy linear regression function
#'
#' @description This function performs linear regression and print results in tibble output.
#' This function aims to provide the results of the regression analysis in the format, which is frequently
#' desired in academic journals.
#'
#' @param data data frame or tibble object
#' @param independent.var independent variable/s
#' @param dependent.var dependent variable/s
#' @param covariates covariates to be included in a model
#' @param print.cov Print effect of covariates, default is FALSE
#' @param check_multicolinearity Should multicolinearity assumption be checked? Default is TRUE
#' @param Z_score_independent Should independent variables be z-scored? Default is FALSE
#'
#' @return data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @details
#' Currently, this function does not provide model fit indicators such as AIC or BIC.
#'
#' @keywords linear-regression
#'
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
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr if_all
#' @importFrom janitor row_to_names
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom reshape2 melt
#' @importFrom janitor remove_empty
#' @importFrom purrr keep
#' @importFrom stats as.formula
#' @importFrom stats confint.default
#' @importFrom stats lm
#' @importFrom stats setNames
#' @importFrom stats p.adjust
#' @importFrom utils tail
#' @importFrom dplyr tibble
#' @importFrom stats coef
#' @importFrom dplyr across
#' @importFrom stringr str_replace
#' @importFrom tidyselect ends_with
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom stringr str_replace_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr full_join
#' @importFrom tidyr as_tibble
#' @importFrom dplyr rename
#' @importFrom dplyr starts_with
#' @importFrom janitor round_half_up
#' @importFrom car vif
#' @importFrom tidyr tibble
#'
#' @examples
#' # data simulation
#' library(dplyr)
#' data.PAQ = tibble(.rows = 1000)
#' data.PAQ <- data.PAQ %>%
#'  mutate("multiple__exper_1" = rnorm(n = nrow(data.PAQ), mean = 20, sd = 5)) %>%
#'  mutate("exper_1" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5)) %>%
#'  mutate("exper_1" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5),
#'         "exper_2" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5),
#'         "last_binary_vasdl" = rnorm(n = nrow(data.PAQ), mean = 20, sd = 20),
#'         "last_binary_val2" = rnorm(n = nrow(data.PAQ), mean = 5, sd = 5),
#'         "last_binary_val3" = rnorm(n = nrow(data.PAQ), mean = 10, sd = 40),
#'         "last_binary_val4" = rnorm(n = nrow(data.PAQ), mean = 50, sd = 5),
#'         "last_binary_val5" = rnorm(n = nrow(data.PAQ), mean = 5, sd = 4),
#'         "last_binary_val6" = rnorm(n = nrow(data.PAQ), mean = 65, sd = 5))
#'
#' lin.reg.output <- lasy.lin.reg(independent.var = c("last_binary_vasdl"),
#'                               covariates = c("last_binary_val6"),
#'                               dependent.var = c("last_binary_val5","last_binary_val4"),
#'                               Z_score_independent = FALSE,
#'                               check_multicolinearity = TRUE,
#'                               data = data.PAQ,
#'                               print.cov = FALSE)
#'
#' print(lin.reg.output)
#' @export
#......................................................

lasy.lin.reg <- function(independent.var, dependent.var, print.cov = FALSE, Z_score_independent = FALSE, check_multicolinearity = TRUE, covariates = NULL, data) {
  paste("the error with even numbers occured:", date())
  # creating empty lists
  models.adj= list()
  models.crude= list()
  multicolinearity_value = 10


  # multicolinearity checking function
  multicolinearity_function = function(x){
    if (any(bind_rows(x)$VIF > multicolinearity_value, na.rm = TRUE)) {
      multicolinearity_Variables <- select(filter(bind_rows(x), VIF  > multicolinearity_value), Var) %>%
        filter(!duplicated(Var))
      stop("There is multicolineariy in your model between the following variables: ",
           paste(multicolinearity_Variables$Var,collapse = ","), ". You have to either remove one of them or run two separate analysis with one of these variables in each analysis.")
    } else {
      message("There is no multicolinearity in your models")
    }
  }

  # creating Z-scores if needed
  if(!Z_score_independent == TRUE) {
    data = data %>%
     mutate(across(c(independent.var) & where(~is.numeric(.)), ~scale(.)))
  }

  #........................................................................................................................
  # crude effect
  #........................................................................................................................
  for (dep.var in  dependent.var) {
    data.func = data
    # crude effect regression
    models.crude[[dep.var]] <- lm(as.formula(paste(dep.var,"~",paste(c(independent.var), collapse="+"))), data = data.func)

    # Milticolinearity
    #..............................................................................
    if (length(independent.var)  == 1) {
      # print("Ok, you have only one predictor here, thus not VIF is calculated")
    } else if (length(independent.var)  > 1) {
      # print("Ok, you have more than one predictor calculating VIF...")
      vif_of_model_terms_crude <- vif(models.crude[[dep.var]]) %>%
        as_tibble(rownames = "Var") %>% rename_with(~str_replace_all(.,
                                                                     "^value$|^GVIF$|^VIF$", "VIF"))
    }
    #..............................................................................

    models.crude[[dep.var]] <- cbind(
        B = coef(models.crude[[dep.var]]),
        confint.default(models.crude[[dep.var]], level = 0.95),
      `Pr(>|t|)` = summary(models.crude[[dep.var]])$coefficients[,"Pr(>|t|)"]
    )
    models.crude[[dep.var]]<- cbind(models.crude[[dep.var]], adj_pval = NA_character_) %>%
      as_tibble(models.crude[[dep.var]], rownames = "Var")

    # Milticolinearity
    #..............................................................................
    if (length(independent.var)  > 1) {
      models.crude[[dep.var]] <- models.crude[[dep.var]] %>%
        full_join(vif_of_model_terms_crude)
    }
    #..............................................................................

    models.crude[[dep.var]] <- models.crude[[dep.var]] %>%
      filter(if_any(everything(.),  ~str_detect(., paste(independent.var, collapse = "|"))))
    for (i in seq_along(models.crude)) {
      models.crude[[i]]$adj_pval <- p.adjust(as.numeric(models.crude[[i]]$`Pr(>|t|)`,method = "BH", n = i))

      # Milticolinearity
      #..............................................................................
      if (length(independent.var)  > 1) {
        models.crude[[i]]$VIF <- as.numeric(models.crude[[i]]$VIF)
      }
      #..............................................................................

      models.crude[[i]]$B <- round_half_up(as.numeric(models.crude[[i]]$B),digits = 2)
      models.crude[[i]]$`2.5 %` <- round_half_up(as.numeric(models.crude[[i]]$`2.5 %`),digits = 2)
      models.crude[[i]]$`97.5 %` <- round_half_up(as.numeric(models.crude[[i]]$`97.5 %`),digits = 2)
      models.crude[[i]]$sig.stars <- insight::format_p(models.crude[[i]]$adj_pval, stars_only = T)
      models.crude[[i]]$Crude <- paste0(models.crude[[i]]$B, " ",
                                        "(", models.crude[[i]]$`2.5 %`,
                                        "-",
                                        models.crude[[i]]$`97.5 %`,") ",
                                        models.crude[[i]]$sig.stars)
    }
  }

  # multicolinearity control
  if (check_multicolinearity == TRUE) {
    multicolinearity_function(models.crude)
    }

  for(i in seq_along(models.crude)){
    models.crude[[i]] <- models.crude[[i]] %>%
      filter(!if_all(c("B", "2.5 %", "97.5 %", "Pr(>|t|)"), ~is.na(.))) %>%
      select(Var,Crude)
    print(models.crude)
  }


  #...........................................................................
  # adjusted effect
  #...........................................................................
  for (dep.var in  dependent.var) {
    # adj effect regression
    models.adj[[dep.var]] <- lm(as.formula(paste(dep.var,"~",paste(c(independent.var, covariates), collapse="+"))),
                                 data = data.func)

    # Milticolinearity
    #..............................................................................
    if (length(independent.var)  == 1) {
      # print("Ok, you have only one predictor here, thus not VIF is calculated")
    } else if (length(independent.var)  > 1) {
      # print("Ok, you have more than one predictor calculating VIF...")
      vif_of_model_terms_adj <- vif(models.adj[[dep.var]]) %>%
        as_tibble(rownames = "Var") %>% rename_with(~str_replace_all(.,
                                                                     "^value$|^GVIF$|^VIF$", "VIF"))
    }
    #..............................................................................

    models.adj[[dep.var]] <- cbind(exp(cbind(
      B = coef(models.adj[[dep.var]]),
      confint.default(models.adj[[dep.var]], level = 0.95))),
      `Pr(>|t|)` = summary(models.adj[[dep.var]])$coefficients[,"Pr(>|t|)"]
      )

    models.adj[[dep.var]] <- cbind(models.adj[[dep.var]], adj_pval = NA_character_)
    models.adj[[dep.var]] <- as_tibble(models.adj[[dep.var]], rownames = "Var")

    # Milticolinearity
    #..............................................................................
    if (length(independent.var)  > 1) {
      models.adj[[dep.var]] <- models.adj[[dep.var]] %>%
        full_join(vif_of_model_terms_adj)
    }
    #..............................................................................

    if(print.cov == FALSE) {
      models.adj[[dep.var]] <- models.adj[[dep.var]] %>%
        filter(if_any(everything(.),  ~str_detect(., paste(independent.var, collapse = "|"))))
    }
    for (i in seq_along(models.adj)) {
      models.adj[[i]]$adj_pval <- p.adjust(as.numeric(models.adj[[i]]$`Pr(>|t|)`,method = "BH", n = i))

      # Milticolinearity
      #..............................................................................
      if (length(independent.var)  > 1) {
        models.adj[[i]]$VIF <- as.numeric(models.adj[[i]]$VIF)
      }
      #..............................................................................

      models.adj[[i]]$B <- round_half_up(as.numeric(models.adj[[i]]$B),digits = 2)
      models.adj[[i]]$`2.5 %` <- round_half_up(as.numeric(models.adj[[i]]$`2.5 %`),digits = 2)
      models.adj[[i]]$`97.5 %` <- round_half_up(as.numeric(models.adj[[i]]$`97.5 %`),digits = 2)
      models.adj[[i]]$sig.stars <- insight::format_p(models.adj[[i]]$adj_pval, stars_only = T)
      models.adj[[i]]$Adjusted <- paste0(models.adj[[i]]$B, " ",
                                         "(", models.adj[[i]]$`2.5 %`,
                                         "-",
                                         models.adj[[i]]$`97.5 %`,") ",
                                         models.adj[[i]]$sig.stars)
    }
  }

  # multicolinearity control
  if (check_multicolinearity == TRUE) {
    multicolinearity_function(models.adj)
  }

  if(print.cov == TRUE) {
    for (l in seq_along(models.adj)) {
      models.adj[[l]] <- models.adj[[l]] %>%
        filter(if_all(everything(.), ~!str_detect(., "Intercept")))
    }
  }

  for(i in seq_along(models.adj)){
    models.adj[[i]] <- models.adj[[i]] %>%
      filter(!if_all(c("B", "2.5 %", "97.5 %", "Pr(>|t|)"), ~is.na(.))) %>%
      select(Var,Adjusted)
    print(models.adj)
  }

  #................................................................................
  # Merging crude and adjusted effects together
  #................................................................................
  merged.effects <-  c(models.crude,models.adj)

  # larger alternative of melt func from reshap2 pcg
  #tibble::enframe(merged.effects) %>% tidyr::unnest(cols = c(value))

  melted.df <- melt(merged.effects) %>%
    as_tibble()

  melted.df.wide = melted.df %>%
    group_by(Var) %>%
    mutate(id = row_number()) %>%
    ungroup() %>%
    mutate(eff.type = ifelse(is.na(Crude),"Adjusted", "Crude")) %>%
    pivot_wider(values_from = c("Crude","Adjusted"), names_from = c("id","L1")) %>%
    remove_empty(which = c("cols"))




  a = melted.df.wide %>%
    select(starts_with(c("Var","eff.type","Adjusted_"))) %>%
    rename_with(~str_replace(., "Adjusted_\\d{1,2}_", "")) %>%
    remove_empty(which = c("rows"))
  a



  b = melted.df.wide  %>%
    select(starts_with(c("Var","eff.type","Crude_"))) %>%
    rename_with(~str_replace(., "Crude_\\d{1,2}_", "")) %>%
    remove_empty(which = c("rows"))


  b

  c <- full_join(b,a) %>% drop_na()


  # c %>%
  #   as_tibble() %>%
  #   rename_at(vars(!starts_with(c("Var","eff.type"))), ~paste0(rep(seq(1:2),2)))


  c <- setNames(rbind(names(c), c), names(c))

  fc <- c

  col.n.ff <- seq(1,length(dependent.var), by =4)
  col.n.ff
  #col.n.ff <- ifelse(col.n.ff==length(dependent.var), length(dependent.var)-1, col.n.ff)
  col.n.ff <- col.n.ff[!abs(col.n.ff) == max(col.n.ff)]
  col.n.ff

  # if number of columns is not ok, than following will be runned:
  if(length(col.n.ff) > 0) {

    if(any(str_detect(paste0(try(str_replace(names(fc)[3:length(fc)], names(fc)[3:length(fc)],
                                             paste0(rep(seq(1:4),2))), silent = T)),"Error in str_replace"))) {

      tab.lasy.reg <- c %>% janitor::row_to_names(row_number = 1) %>%
        mutate(across(ends_with(c("Var","eff.type")), ~str_replace_all(., "Var|eff.type", "")))

      paste("the error with even numbers occured:", date())

      print(tab.lasy.reg)

      paste("the error with even numbers occured:", date())

      # if the names are even than there is need to use different procedure of renaming variables
      # names(fc)[3:length(fc)] <- str_replace(names(fc)[3:length(fc)], names(fc)[3:length(fc)],
      #                                        paste0(c("1", "2", "3", "4", "5", "1", "2", "3", "4")))
      #..............................................
      # this is the way how to fix it:
      # bind_rows(ff[c(1:2,8,9,10,11)], ff[c(1:5)])
      #................................................

      ff <- fc %>% melt()

      ee = list()


    }
    if(!any(str_detect(paste0(try(str_replace(names(fc)[3:length(fc)], names(fc)[3:length(fc)],
                                              paste0(rep(seq(1:4),2))), silent = T)),"Error in str_replace"))) {
      names(fc)[3:length(fc)] <- str_replace(names(fc)[3:length(fc)], names(fc)[3:length(fc)],
                                             paste0(rep(seq(1:4),2)))

      ff <- fc %>% melt()

      ee = list()


      # there is need to create condition: if the number of dependent.var is even than the following code ming be applyed, however if it will be odd than there is need to
      # subrract one number from the first part
      # func detecting even number:
      # (length(dependent.var)) %% 2 == 0


      for (i in col.n.ff) {
        ee[[i]] <- bind_rows(ff[, c(1,2,c(i+2):c(i+5))])
        ee <- ee %>%
          keep(~ !is.null(.))
      }


      # removing duplicates
      #.....................................
      for (i in seq_along(ee)) {
        ee[[i]]$eff.type <- ifelse(duplicated(ee[[i]]$eff.type), "" , ee[[i]]$eff.type)
        print(ee)
      }

      remaining.vars <- ff[, c(1,2,c((tail(col.n.ff, n = 1)+6):ncol(ff)))] %>%
        mutate(across(ends_with(c("eff.type")), ~ifelse(duplicated(.), "", .)))
      #.....................................

      # binding lists together
      vv = ee %>% bind_rows()

      tab.lasy.reg.to.clean <- bind_rows(vv,remaining.vars) %>% row_to_names(row_number = 1)

      tab.lasy.reg <- tab.lasy.reg.to.clean %>%
        mutate(across(ends_with(c("Var","eff.type")), ~str_replace_all(., "Var|eff.type", "")))

      print(tab.lasy.reg)
      return(tab.lasy.reg)
    }
  } else
    ff <- fc %>% melt()
  tab.lasy.reg <- ff %>%
    row_to_names(row_number = 1)
  if(is.null(covariates)){
    tab.lasy.reg = tab.lasy.reg %>%
      filter(!eff.type %in% "Adjusted")
  }
  tab.lasy.reg = tab.lasy.reg %>%
    mutate(across(ends_with(c("eff.type")), ~ifelse(duplicated(.), "", .)))
}


# library(dplyr)
# library(stringr)
# library(reshape2)
# library(janitor)
# library(tidyverse)

#...........................................................................................
# testing data
# #...........................................................................................
# load("./data/paq.validation.study.rda")
# data.PAQ =  paq.validation.study
# #
# #
# data.PAQ = tibble(.rows = 1000)
# #
# data.PAQ <- data.PAQ %>%
#   mutate("multiple__exper_1" = rnorm(n = nrow(data.PAQ), mean = 20, sd = 5)) %>%
#   mutate("exper_1" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5)) %>%
#   mutate("exper_1" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5),
#          "exper_2" = rnorm(n = nrow(data.PAQ),  mean = 20, sd = 5),
#          "last_binary_vasdl" = rnorm(n = nrow(data.PAQ), mean = 20, sd = 20),
#          "last_binary_val2" = rnorm(n = nrow(data.PAQ), mean = 5, sd = 5),
#          "last_binary_val3" = rnorm(n = nrow(data.PAQ), mean = 10, sd = 40),
#          "last_binary_val4" = rnorm(n = nrow(data.PAQ), mean = 50, sd = 5),
#          "last_binary_val5" = rnorm(n = nrow(data.PAQ), mean = 5, sd = 4),
#          "last_binary_val6" = rnorm(n = nrow(data.PAQ), mean = 65, sd = 5))
#
# data.PAQ = data.PAQ %>% mutate(across(c(independent.var), ~ifelse(. > 10, NA_character_, .)))
#
# dependent.var = c("exper_1","exper_2","multiple__exper_1")
# #
# independent.var = c("last_binary_vasdl")
# covariates = c("last_binary_val6")
# data = "data.PAQ"
# print.cov = FALSE
#
# data(paq.validation.study)
# regress.output <- pokus(independent.var = c("PAQ"),
#                         check_multicolinearity = TRUE,
#                         covariates = c("Age","education"),
#                         dependent.var = c("TEQ"),
#                         data = paq.validation.study)
#
# regress.output
#

#...........................................................................................


#
# zsco = lasy.lin.reg(independent.var = c("PANAS_P","SIAS","PAQ_duration"),
#              dependent.var = c("SMDS_duration","RMET_duration","Age","P_DDF"),
#              covariates = c("Gender"),
#              Z_score_independent = FALSE,
#              check_multicolinearity = TRUE,
#              data = data_for_testing,
#              print.cov = FALSE)
#
# zsco
#
# nozscore = lasy.lin.reg(independent.var = c("N_DDF","PANAS_P","SIAS"),
#              covariates = c("RMET_duration","SMDS_duration"),
#              dependent.var = dependent.var,
#              Z_score_independent = FALSE,
#              data = data_for_testing ,
#              print.cov = FALSE)
#


# a  = lasy.lin.reg(independent.var = c("PAQ","Gender"),
#              covariates = c("education","Age"),
#              dependent.var = c("TEQ"),
#              data = data.PAQ,
#              print.cov = FALSE)





# tab.lasy.reg %>%
#   flextable::flextable() %>%
#   flextable::autofit() %>%
#   flextable::save_as_docx(path = paste0(getwd(),"/tab.lasy.reg.docx"))

#
# ff[, c(1,2,c(1+2):c(1+5))]
#
# ff[, c(1,2,c(5+2):c(5+5))]
#
# ff[, c(1,2,c(5+2):c(5+5))]
#
# ff[, c(1,2,c(6+2):c(6+5))]
#

# re=lm(default~student+balance+income, family="binomial", data=ISLR::Default)
# exp(cbind(B = coef(re), confint.default(re))) %>% round_half_up(digits = 2)
#
# lm(Survived ~ Sex + Class, data = tit) %>% broom::tidy() %>% mutate(estimate = exp(estimate))
#
# datas = data=ISLR::Default
# a=lasy.log.reg(dependent.var = "default", independent.var = c("student","balance"), covariates = "income", data = datas)
# a
#
# radka <- haven::read_sav(choose.files())
# a = lasy.log.reg(independent.var = "GSES_sum", dependent.var = c("headache_dich", "stomachache_dich", "backache_dich", "intestinal_problem_dich",
#                                              "feeling_low_dich", "irritability_dich", "sleep_diff_dich", "dizziness_dich","nervousness_dich"
#                                              ), data = radka, covariates = c("age","gender"))

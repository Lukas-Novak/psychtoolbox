# Documentation
#' Lasy logistic regression function
#'
#' @description This function performs logistic regression and print results in tibble output.
#' This function aims to provide the results of the regression analysis in the format, which is frequently
#' desired in academic journals
#'
#' @param data data frame or tibble object
#' @param independent.var independent variable/s
#' @param dependent.var dependent variable/s
#' @param covariates covariates to be included in a model
#' @param print.cov Print effect of covariates, default is FALSE
#'
#' @return data frame
#'
#' @docType data
#'
#' @format An object of class \code{"tibble"}
#'
#' @details
#' Currently, this function does not provide model fit indicators such as AIC or BIC
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
#' @importFrom janitor row_to_names
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom reshape2 melt
#' @importFrom janitor remove_empty
#' @importFrom purrr keep
#' @importFrom stats as.formula
#' @importFrom stats confint
#' @importFrom stats glm
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
#'
#' @examples
#' # data loading
#' data(paq.validation.study)
#' # dichotomization of variables
#' paq.validation.study <- paq.validation.study %>%
#' dplyr::mutate(edu_dich = as.factor(ifelse(
#'  education == "University master or higher",
#'  "University","lower_edu"
#'  )),
#' econom_stat_dich = as.factor(ifelse(
#'  economical_status == "Student",
#'  "Student","non_student"
#' )))
#'
#' # dichotomization of variables
#' regress.output <- lasy.log.reg(independent.var = c("TEQ","PAQ","G_EOT","G_DDF"),
#'                               covariates = c("Age"),
#'                               dependent.var = c("econom_stat_dich",
#'                                             "family_status",
#'                                             "edu_dich"),
#'                               data = "paq.validation.study")
#'
#' print(regress.output)
#' @export
#......................................................

lasy.log.reg <- function(independent.var, dependent.var, covariates, print.cov = FALSE, data) {
  # creating empty lists
  models.adj= list()
  models.crude= list()
  #...........................................................................
  # crude effect
  #...........................................................................
  for (dep.var in  dependent.var) {
    data.func = get(data)
    # crude effect regression
    models.crude[[dep.var]] <- glm(as.formula(paste(dep.var,"~",paste(c(independent.var), collapse="+"))), data = data.func, family = "binomial")
    models.crude[[dep.var]] <- cbind(
      exp(cbind(
        OR = coef(models.crude[[dep.var]]),
        confint(models.crude[[dep.var]], level = 0.95))),
      `Pr(>|z|)` = summary(models.crude[[dep.var]])$coefficients[,"Pr(>|z|)"]
    )
    models.crude[[dep.var]]<- cbind(models.crude[[dep.var]], adj_pval = NA_character_) %>%
      as_tibble(models.crude[[dep.var]], rownames = "Var") %>%
      filter(if_any(everything(.),  ~str_detect(., paste(independent.var, collapse = "|"))))
    for (i in seq_along(models.crude)) {
      models.crude[[i]]$adj_pval <- p.adjust(as.numeric(models.crude[[i]]$`Pr(>|z|)`,method = "BH", n = i))
      models.crude[[i]]$OR <- round(as.numeric(models.crude[[i]]$OR),digits = 2)
      models.crude[[i]]$`2.5 %` <- round(as.numeric(models.crude[[i]]$`2.5 %`),digits = 2)
      models.crude[[i]]$`97.5 %` <- round(as.numeric(models.crude[[i]]$`97.5 %`),digits = 2)
      models.crude[[i]]$sig.stars <- insight::format_p(models.crude[[i]]$adj_pval, stars_only = T)
      models.crude[[i]]$Crude <- paste0(models.crude[[i]]$OR, " ",
                                        "(", models.crude[[i]]$`2.5 %`,
                                        "-",
                                        models.crude[[i]]$`97.5 %`,") ",
                                        models.crude[[i]]$sig.stars)
    }
  }

  for(i in seq_along(models.crude)){
    models.crude[[i]] <- models.crude[[i]] %>% select(Var,Crude)
    print(models.crude)
  }


  #...........................................................................
  # adjusted effect
  #...........................................................................

  for (dep.var in  dependent.var) {
    data.func = get(data)
    # adj effect regression
    models.adj[[dep.var]] <- glm(as.formula(paste(dep.var,"~",paste(c(independent.var, covariates), collapse="+"))),
                                 data = data.func, family = "binomial")
    models.adj[[dep.var]] <- cbind(exp(cbind(
      OR = coef(models.adj[[dep.var]]),
      confint(models.adj[[dep.var]], level = 0.95))),
      `Pr(>|z|)` = summary(models.adj[[dep.var]])$coefficients[,"Pr(>|z|)"])
    models.adj[[dep.var]] <- cbind(models.adj[[dep.var]], adj_pval = NA_character_)
    models.adj[[dep.var]] <- as_tibble(models.adj[[dep.var]], rownames = "Var")
    if(print.cov == FALSE) {
      models.adj[[dep.var]] <- models.adj[[dep.var]] %>%
        filter(if_any(everything(.),  ~str_detect(., paste(independent.var, collapse = "|"))))
    }
    for (i in seq_along(models.adj)) {
      models.adj[[i]]$adj_pval <- p.adjust(as.numeric(models.adj[[i]]$`Pr(>|z|)`,method = "BH", n = i))
      models.adj[[i]]$OR <- round(as.numeric(models.adj[[i]]$OR),digits = 2)
      models.adj[[i]]$`2.5 %` <- round(as.numeric(models.adj[[i]]$`2.5 %`),digits = 2)
      models.adj[[i]]$`97.5 %` <- round(as.numeric(models.adj[[i]]$`97.5 %`),digits = 2)
      models.adj[[i]]$sig.stars <- insight::format_p(models.adj[[i]]$adj_pval, stars_only = T)
      models.adj[[i]]$Adjusted <- paste0(models.adj[[i]]$OR, " ",
                                         "(", models.adj[[i]]$`2.5 %`,
                                         "-",
                                         models.adj[[i]]$`97.5 %`,") ",
                                         models.adj[[i]]$sig.stars)
    }
  }

  if(print.cov == TRUE) {
    for (l in seq_along(pokus)) {
      models.adj[[l]] <- models.adj[[l]] %>%
        filter(if_all(everything(.), ~!str_detect(., "Intercept")))
    }
  }

  for(i in seq_along(models.adj)){
    models.adj[[i]] <- models.adj[[i]] %>% select(Var,Adjusted)
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


  names(fc)[3:length(fc)] <- str_replace(names(fc)[3:length(fc)], names(fc)[3:length(fc)],
                                         paste0(rep(seq(1:4),2)))
  ff <- fc %>% melt()


  col.n.ff <- seq(1,length(dependent.var), by =4)
  col.n.ff
  #col.n.ff <- ifelse(col.n.ff==length(dependent.var), length(dependent.var)-1, col.n.ff)
  col.n.ff <- col.n.ff[!abs(col.n.ff) == max(col.n.ff)]
  col.n.ff


  # if number of columns is not ok, than following will be runned:
  if(length(col.n.ff) > 0) {

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

  } else
    tab.lasy.reg <- ff %>%
    row_to_names(row_number = 1) %>%
    mutate(across(ends_with(c("eff.type")), ~ifelse(duplicated(.), "", .)))
}


# library(dplyr)
# library(stringr)

#...........................................................................................
# testing data
#...........................................................................................
# data.PAQ =  readRDS(paste0(getwd(),"/Data/paq.validation.study.Rds")) %>%
#   mutate("multiple__exper_1" = rbinom(n = nrow(data.PAQ), prob = 0.5, size =0:1)) %>%
#   mutate("binary__exper_1" = rbinom(n = nrow(data.PAQ), prob = 0.3, size =0:1)) %>%
#   mutate("binary2__exper_1" = rbinom(n = nrow(data.PAQ), prob = 0.6, size =0:1),
#          "binary2__exper_2" = rbinom(n = nrow(data.PAQ), prob = 0.9, size =0:1),
#          "last_binary_vasdl" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.55),
#          "last_binary_val2" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.2),
#          "last_binary_val3" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.2),
#          "last_binary_val4" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.2),
#          "last_binary_val5" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.2),
#          "last_binary_val6" = rbinom(n = nrow(data.PAQ), size = 0:1, prob = 0.2))
#
# dependent.var = c("family_status","Gender","economical_status",
#               "education","multiple__exper_1","binary__exper_1","binary2__exper_1","binary2__exper_2",
#               "last_binary_vasdl","last_binary_val2",
#               "last_binary_val3", "last_binary_val4","last_binary_val5","last_binary_val6")
#
# independent.var = c("TEQ","Age","PAQ")
# covariates = c("ethnicity")
# data = "data.PAQ"
# print.cov = FALSE
#...........................................................................................

# lasy.log.reg(independent.var = c("TEQ","Age","PAQ"),
#              covariates = c("ethnicity"),
#              data = "data.PAQ",
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

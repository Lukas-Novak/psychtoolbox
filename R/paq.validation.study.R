#' @title paq.validation.study
#' @description This dataset contains data which were used for validation of the Perth Alexithymia Questionnaire in the Czech Republic.
#' @format A data frame with 848 rows and 53 variables:
#' \describe{
#'   \item{\code{P_DIF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{P_DDF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{P_DAF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{N_DIF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{N_DDF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{N_DAF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{G_EOT}}{double COLUMN_DESCRIPTION}
#'   \item{\code{G_DIF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{G_DDF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{G_DAF}}{double COLUMN_DESCRIPTION}
#'   \item{\code{family_status}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{Gender}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_5}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_16}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_CON_2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_CON_4}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ_CON_14}}{double COLUMN_DESCRIPTION}
#'   \item{\code{TEQ}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Age}}{double COLUMN_DESCRIPTION}
#'   \item{\code{age_group}}{double COLUMN_DESCRIPTION}
#'   \item{\code{age.quality}}{double COLUMN_DESCRIPTION}
#'   \item{\code{ethnicity}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{education}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{economical_status}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_1}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_2}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_3}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_4}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_5}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_6}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_7}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_8}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_9}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_10}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_11}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_12}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_13}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_14}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_15}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_16}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_17}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_18}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_19}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_20}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_21}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_22}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_23}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ_24}}{double COLUMN_DESCRIPTION}
#'   \item{\code{PAQ}}{double COLUMN_DESCRIPTION}
#'   \item{\code{edu_dich}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{econom_stat_dich}}{integer COLUMN_DESCRIPTION}
#'}
#' @details # dichotomization of variables
#'paq.validation.study <- paq.validation.study %>%
#'  dplyr::mutate(edu_dich = as.factor(ifelse(
#'    education == "University master or higher",
#'    "University","lower_edu"
#'  )),
#'  econom_stat_dich = as.factor(ifelse(
#'    economical_status == "Student",
#'    "Student","non_student"
#'  )))
#'
"paq.validation.study"

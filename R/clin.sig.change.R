# Clinically significant change
#......................................................
# Documentation
#' @title Clinically significant change
#' @description  This easy function calculates Clinically significant change (clinical cut-off scores) as defined by Jacobson and Truax (1991).
#'
#' @param SD_0 standard deviation of the non-clinical population
#' @param SD_1 standard deviation of the clinical population
#' @param M_0 mean of the non-clinical population
#' @param M_1 mean of the clinical population
#'
#' @return numeric vector
#'
#' @docType data
#'
#' @format numeric vector of values
#'
#' @details This function computes cut-off score differentiating between the clinical and non-clinical population based on the Jacobson and Truax (1991) formula (p. 13). The mathematical formula can be also found in Biescad & Timulak(2014, p. 150).
#'
#' @references Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12-19, DOI:
#'  [https://doi.org/10.1037/0022-006X.59.1.12]()
#'
#'  Matus Biescad & Ladislav Timulak (2014). Measuring psychotherapy
#'outcomes in routine practice: Examining Slovak versions of three commonly used outcome
#'instruments, European Journal of Psychotherapy & Counselling, 16:2, 140-162, DOI:
#'  [https://doi.org/10.1080/13642537.2014.895772]()
#'
# @seealso [psychtoolbox::CRI()] function for calculation of the Reliable Change Index
#' @seealso [psychtoolbox::RCI()] function for calculation of the Reliable Change Index
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#' @importFrom stats sd
#'
#' @examples
#' clin.cut.off=clin_sig_chang(SD_0 = 3.5,
#'                             SD_1 = 2.1,
#'                             M_0 = 4.2,
#'                             M_1 = 12.1)
#'clin.cut.off
#'
#' @export
#......................................................

clin_sig_chang <- function(SD_0,SD_1,M_1,M_0) {
  (SD_0*M_1+SD_1*M_0)/(SD_0+SD_1)
}

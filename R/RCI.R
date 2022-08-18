# Clinically significant change
#......................................................
# Documentation
#' @title Reliable Change Index (RCI)
#' @description  This function calculates Reliable Change Index (RCI) as modifed by Wiger and Solberg (2001, p.148).
#'
#' @param SD_0 standard deviation of the non-clinical population
#' @param test.ret.rel test-retest reliability of the instrument
#'
#' @return numeric vector
#'
#' @docType data
#'
#' @format numeric vector of values
#'
#' @details This function computes value corresponding to "the minimum amount of change that could not be attributed to the error of measurement" (Biescad & Timulak, 2014, p. 150). If score change from before to post treatment
#' is lower that value resulting from this function, than change in client score can not be attributed to the effectiveness of the therapy but rather other factors such as a measurement error (Biescad & Timulak, 2014). This function
#' is a result of modification of the original Jacobson and Truax (1991) formula by Wiger and Solberg (2001, p.148).
#'
#' @references Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12-19, DOI:
#'  [https://doi.org/10.1037/0022-006X.59.1.12]()
#'
#'  Matus Biescad & Ladislav Timulak (2014). Measuring psychotherapy
#'outcomes in routine practice: Examining Slovak versions of three commonly used outcome
#'instruments, European Journal of Psychotherapy & Counselling, 16:2, 140-162, DOI:
#'  [https://doi.org/10.1080/13642537.2014.895772]()
#'
#'Wiger, D. E., & Solberg, K. B. (2001). Tracking Mental Health Outcomes:
#'A Therapist’s Guide to Measuring Client Progress, Analyzing Data,
#'and Improving Your Practice (1., Vol. 2001). Wiley.
#'
#' @seealso [psychtoolbox::clin_sig_chang()] function for calculation of the clinical cut-off scores
#' @author Lukas Novak, \email{lukasjirinovak@@gmail.com}
#'
#' @examples
#'
#' re.ch.in = RCI(SD_0 = 4.87, test.ret.rel = 0.66)
#' re.ch.in
#'
#' @export
#......................................................

RCI <- function(SD_0,test.ret.rel) {
  1.96*(sqrt(2*SD_0^2*(1-test.ret.rel)))
}

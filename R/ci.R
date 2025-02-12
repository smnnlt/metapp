#' Get the confidence interval for a given effect size
#'
#' Calculates the confidence interval for
#'
#' @param es A data frame of the class mpp with an effect size and its variance.
#'   For example, the output of \code{\link{md}}, \code{\link{smd}},
#'   \code{\link{smcr}}, \code{\link{ppc}}.
#' @param level the confidence level. Defaults to 0.95.
#' @param df The degrees of freedom. Defaults to NA, which assumes a normal
#'   distribution. Otherwise a t-distribution is assumed.
#' @returns A data.frame with the effect size (es), its variance (var) and the
#'   lower and upper limits of the confidence interval (ci_low, ci_high).
#' @examples
#' # calculate a SMD first
#' g <- smd(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#'
#' # calculate confidence interval
#' get_ci(g)
#'
#' @export
get_ci <- function(es, level = 0.95, df = NA) {
  if (is.na(df)) { # z distribution
    q <- stats::qnorm((1-level)/2)
  } else { # t-distribution
    q <- stats::qt((1-level)/2, df)
  }
  data.frame(
    es = es$es,
    var = es$var,
    ci_low = es$es + q * sqrt(es$var),
    ci_high = es$es - q * sqrt(es$var)
  )
}

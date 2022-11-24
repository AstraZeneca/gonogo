NULL

#' Per-var p-values
#'
#' @param vars A data frame with the following columns:
#'
#' | Column | Type | Description |
#' | df     | numeric() | Degrees of freedom for the variable |
#' | se.hat | numeric() | Estimated standard error |
#' | p      | numeric() | p-value for the variable |
#' | negatively.significant | numeric() | Whether the variable is negatively
#'                                        significant |
#' | delta  | numeric() | Estimation of the effect for the variable |
#' | delta.ci.up | Upper bound of the confidence interval for the variable |
#' | delta.ci.dn | Lower bound of the confidence interval for the variable |
#'
#' @param .params Parameters used for computing the p-values
#'
#' @export
PValuesPerVar <- function(vars, .params) {
  structure(tibble::lst(vars, .params),
    class = "PValuesPerVar"
  )
}

#' Calculate p.values for a given run of a trial
#'
#'
#' @param  study  A [Study()] with a [Summary()].
#' @param  alpha  A significance level.
#' @returns A [PValuesPerVar] object.
#'
#' @export
p_values_per_var <- function(study, alpha = getOption("gonogo.alpha.default", 0.05)) {
  summary <- study$summary
  std.p <- function(q) {
    pt(q, df = summary$degrees.of.freedom, lower.tail = FALSE)
  }
  std.q <- function(p) {
    qt(p, df = summary$degrees.of.freedom)
  }
  vars <- tibble::tibble(
    df = summary$degrees.of.freedom,
    delta.hat = summary$vars$delta.hat,
    se.hat = summary$vars$delta.se,
    p = 2 * std.p(abs(.data$delta.hat / .data$se.hat)),
    negatively.significant = .data$p <= alpha & .data$delta.hat < 0,
    delta.ci.dn = .data$delta.hat + std.q(alpha / 2) * .data$se.hat,
    delta.ci.up = .data$delta.hat + std.q(1 - alpha / 2) * .data$se.hat
  )

  PValuesPerVar(vars, .params = alpha)
}

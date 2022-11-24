NULL

#' PowerPerVar
#'
#' Details the power of each individual variable in a study
#'
#' @param distr Distribution used for the power calculation
#' @param alpha Significance level used for the power calculation
#' @param vars  Dataframe with `n` rows (one per variable) and the following
#'   columns:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | name | character() | Variable name |
#' | delta | numeric() | Effect used for the power calculation |
#' | sigma.sq | numeric() | Variance used for the power calculation |
#' | df | numeric() | Degrees of freedom used for the power calculation |
#' | se | numeric() | Standard error used for the power calculation |
#' | power | numeric() | Power of the variable |
#'
#' @export
PowerPerVar <- function(distr, alpha, vars) {
  structure(tibble::lst(distr, alpha, vars),
    class = "PowerPerVar"
  )
}

power_per_var_helper <- function(vars.names = "N/A",
                                 vars.domains = NULL,
                                 vars.delta,
                                 vars.sigma.sq,
                                 n.patients,
                                 alpha = getOption("gonogo.alpha.default", 0.05),
                                 distr = c("t", "norm")) {
  stopifnot(is.scalar(alpha))
  distr <- match.arg(distr)
  the.distr <- inference_distr_per_var(
    n.patients = n.patients,
    distr = distr,
    vars.sigma.sq = vars.sigma.sq
  )
  PowerPerVar(
    distr = distr,
    alpha = alpha,
    vars = tibble::tibble(
      domain = vars.domains,
      name = vars.names,
      delta = vars.delta,
      sigma.sq = vars.sigma.sq,
      df = the.distr$df,
      se = the.distr$se,
      # We consider a two sample, two sided test.
      # It is a non-strict test; that is, we only consider
      # the "probability" of detecting the effect in one direction
      # (e.g. the true one)
      power = 1 - the.distr$std.p(
        -the.distr$std.q(alpha / 2),
        ncp = -abs(vars.delta / the.distr$se)
      )
    )
  )
}

#' Power calculations for a given trial design
#' (note that it takes the effect from the trial itself,
#'  not from the target value)
#'
#' @param  trial  [Trial()]
#' @param  use.effect Effect to use for the power calculation:
#'   (See `parse_effect_vector`)
#' @param ... Other arguments (passed through to `power_per_var_helper`).
#'
#' @details
#' (*) This option requires that the `trial |> has_ground_truth()`.
#'
#' @export
power_per_var <- function(trial, use.effect = "tv", ...) {
  power_per_var_helper(
    vars.delta = parse_effect(trial, use.effect),
    vars.sigma.sq = diag(trial$endpoints$sigma),
    vars.names = trial$endpoints$vars$name,
    vars.domains = trial$endpoints$vars$domain,
    n.patients = trial$n.patients,
    ...
  )
}

# (Helper function)
inference_distr_per_var <- function(n.patients,
                                    distr,
                                    vars.sigma.sq) {
  stopifnot(distr %in% c("t", "norm"))
  vars.se <- sqrt(vars.sigma.sq * (1 / n.patients$control + 1 / n.patients$active))
  if (distr == "t") {
    vars.df <- (n.patients$control - 1) + (n.patients$active - 1)
    pdistr <- function(q, ncp = 0) {
      pt(q, df = vars.df, ncp = ncp, lower.tail = FALSE)
    }
    qdistr <- function(p) {
      qt(p, df = vars.df, lower.tail = FALSE)
    }
  } else if (distr == "norm") {
    vars.df <- Inf
    pdistr <- function(q, ncp = 0) {
      pnorm(q, mean = ncp, sd = 1, lower.tail = FALSE)
    }
    qdistr <- function(p) {
      qnorm(p, mean = 0, sd = 1, lower.tail = FALSE)
    }
  } else {
    stopifnot(FALSE)
  }
  tibble::lst(
    df = vars.df, distr,
    std.p = pdistr,
    std.q = qdistr,
    se = vars.se
  )
}

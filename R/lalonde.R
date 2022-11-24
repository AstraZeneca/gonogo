#' @import dplyr tibble
NULL

#' Default probabilities for a decision framework based on Lalonde's proposal (2007).
#' @export
lalonde.probs.default <- tibble::lst(
  max.prob.lt.tv = 0.9,
  min.prob.gt.lrv = 0.8
)

#' An object specifying the probabilities for a decision framework based
#' on Lalonde's proposal
#'
#' @param FSR Maximum allowed false Stop probability.
#' @param FGR Maximum allowed false Go probability.
#'
#' @export
make_lalonde_probs <- function(FSR = 0.1, FGR = 0.2) {
  list(
    max.prob.lt.tv = 1 - FSR,
    min.prob.gt.lrv = 1 - FGR
  )
}

#' Lalonde per-variable thresholds
#'
#' Stores per-variable lalonde thresholds and eventual decisions
#' (if the data to make the decision was given)
#'
#' @param total.vars List of overall variable counts (`n.go`, `n.stop`, `n.discuss`)
#' @param domains Data frame with one row per domain and the following columns:
#'
#' | Column | Type | Description |
#' |--------|------|-------------|
#' | name   | character | Name |
#' | n.go   | numeric   | Number of go variables |
#' | n.stop | numeric   | Number of stop variables |
#' | n.discuss | numeric | Number of discuss variables |
#'
#' @param vars Data frame with one row per variable and the following columns:
#'
#' | Column | Type | Description |
#' |--------|------|-------------|
#' | name   | character | Name |
#' | tv     | numeric() | Target value for the variable |
#' | lrv    | numeric() | Lowest Relevant Value (LRV) for the variable |
#' | tv.threshold | numeric | Threshold derived from `tv` and `max.prob.lt.tv` |
#' | lrv.threshold | numeric | Threshold derived from `lrv` and `max.prob.lt.lrv` |
#' | go.threshold | numeric | Lower threshold for `go` |
#' | stop.threshold | numeric | Upper threshold for `stop` |
#' | decision | DECISION | Decision based on the thresholds |
#'
#' @seealso [lalonde_per_var()]
LalondePerVar <- function(total.vars,
                          domains,
                          vars) {
  assertthat::assert_that(is.list(total.vars))
  assertthat::has_name(total.vars, c("n.go", "n.stop", "n.discuss"))
  assertthat::assert_that(is.data.frame(domains))
  assertthat::has_name(domains, c("name", "n.go", "n.stop", "n.discuss"))
  assertthat::assert_that(is.data.frame(vars))
  assertthat::has_name(vars, c(
    "name", "tv", "lrv", "tv.threshold", "lrv.threshold",
    "go.threshold", "stop.threshold", "decision"
  ))
  structure(tibble::lst(total.vars, domains, vars), class = "LalondePerVar")
}

#' Lalonde per-variable thresholds
#'
#' @param obj Object containing [Endpoints()] information.
#' @param ... Other arguments
#'
#' @returns A [LalondePerVar()] object.
#'
#' @seealso [LalondePerVar()].
#'
#' @export
lalonde_per_var <- function(obj, ...) UseMethod("lalonde_per_var")

#' Lalonde per-variable thresholds
#'
#' @param obj Study
#' @param lalonde.probs Probabilities for Lalonde thresholds
#' @param ... Other arguments (ignored, should be empty)
#'
#' @returns See [LalondePerVar()]
#'
#' @export
lalonde_per_var.Study <- function(obj, lalonde.probs = lalonde.probs.default, ...) {
  assertthat::assert_that(length(list(...)) == 0)
  study <- obj
  summary <- study$summary
  endpoints <- study$trial$endpoints

  t.tv <- qt(lalonde.probs$max.prob.lt.tv,
    df = summary$degrees.of.freedom
  )

  t.lrv <- qt(lalonde.probs$min.prob.gt.lrv,
    df = summary$degrees.of.freedom,
    lower.tail = FALSE
  )

  n.vars <- n_vars(summary)

  tv.threshold <- rep(NA, n.vars)
  lrv.threshold <- rep(NA, n.vars)
  go <- rep(NA, n.vars)
  stop <- rep(NA, n.vars)
  delta.se <- sqrt(diag(summary$sigma.delta.hat))
  for (i in seq(n.vars)) {
    tv.threshold[i] <- endpoints$vars$tv[i] - delta.se[i] * t.tv
    lrv.threshold[i] <- endpoints$vars$lrv[i] - delta.se[i] * t.lrv


    # c.f. Lalonde (2007), Go/Pause/Stop
    # For the default values...
    #
    # Go if PCT90 > TV and PCT20 > LRV
    # PCT90 = vars.delta.hat[i] + sqrt(sigma[i,i])*t_df,90
    # PCT20 = vars.delta.hat[i] + sqrt(sigma[i,i])*t_df,20
    # i.e. vars.delta.hat[i] > TV - sqrt(sigma[i,i])*t_df,90
    # and  vars.delta.hat[i] > LRV - sqrt(sigma[i,i])*t_df,20
    go[i] <- max(tv.threshold[i], lrv.threshold[i])
    # i.e. go if vars.delta.hat[i] > go[i]

    # Stop if PCT90 <= TV
    # PCT90 = vars.delta.hat[i] + sqrt(sigma[i,i])*t_df,90
    # i.e. vars.delta.hat <= TV - sqrt(sigma[i,i])*t_df,90
    stop[i] <- tv.threshold[i]
    # i.e. stop if vars.delta.hat[i] < stop[i]
  }
  #
  stop.p <- pt((summary$vars$delta.hat - endpoints$vars$tv) / delta.se,
    df = summary$degrees.of.freedom,
    lower.tail = TRUE
  )
  go.p <- pt((summary$vars$delta.hat - endpoints$vars$lrv) / delta.se,
    df = summary$degrees.of.freedom,
    lower.tail = FALSE
  )

  decision <-
    if (is.numeric(summary$vars$delta.hat)) {
      ifelse(summary$vars$delta.hat >= go,
        "Go",
        ifelse(summary$vars$delta.hat <= stop,
          "Stop",
          "Discuss"
        )
      )
    } else {
      rep(NA, n.vars)
    }


  vars <- tibble::tibble(
    name = endpoints$vars$name,
    domain = endpoints$vars$domain,
    tv   = endpoints$vars$tv,
    lrv  = endpoints$vars$lrv,
    tv.threshold,
    lrv.threshold,
    go.threshold = go,
    stop.threshold = stop,
    go.p,
    stop.p,
    decision = factor(decision, levels = decision.levels)
  )

  domains <- vars %>%
    group_by(.data$domain) %>%
    summarize(
      n.go = sum(decision == DECISION$Go),
      n.stop = sum(decision == DECISION$Stop),
      n.discuss = sum(decision == DECISION$Discuss)
    ) %>%
    ungroup() %>%
    arrange(match(.data$domain, endpoints$domain$name))

  total <- domains %>%
    summarize(
      n.go = sum(.data$n.go),
      n.stop = sum(.data$n.stop),
      n.discuss = sum(.data$n.discuss)
    )


  LalondePerVar(total, domains, vars)
}

#' @describeIn lalonde_per_var [Trial()]
#' @export
lalonde_per_var.Trial <- function(obj, ...) {
  lalonde_per_var(make_exact_study(obj), ...)
}

#' @describeIn lalonde_per_var [Endpoints()]
#' @param n.patients [NPatients()]
#' @export
lalonde_per_var.Endpoints <- function(obj, n.patients, ...) {
  lalonde_per_var(make_trial(obj, n.patients), ...)
}

#' @describeIn lalonde_per_domain.Study Generic method
#' @export
lalonde_per_domain <- function(obj, ...) UseMethod("lalonde_per_domain")

#' @describeIn lalonde_per_domain.Study Specialization for [Trial()]
#' @export
lalonde_per_domain.Trial <- function(obj, ...) {
  lalonde_per_domain(make_exact_study(obj), ...)
}


#' Lalonde thresholds for multiple endpoints in a domain
#'
#' Return thresholds that can be used for applying the
#' Lalonde framework to multiple domains.
#'
#' @param obj A [Study()] description, which includes endpoints,
#'   a number of patients and true effects.
#' @param params A list containing two values:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | p.FS | numeric | False Stop probability |
#' | p.FG | numeric | False Go probability |
#'
#' @param ... (No additional arguments are allowed)
#'
#' @returns A tibble with the following columns; each row represents a
#'   threshold.
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | domain | character | The name of a domain |
#' | name | character | The name of an endpoint |
#' | se | numeric | Standard error for that endpoint according to the number of patients in the study |
#' | df | numeric | Degrees of freedom in the estimation of the SE |
#' | tv | numeric | Target Value |
#' | lrv | numeric | Lower Reference Value |
#' | domain.n.vars | numeric | Number of variables in the domain |
#' | required | numeric | A number between 1 and the number of variables in the domain |
#' | fraction | numeric | required as a fraction of the total number of variables |
#' | level | numeric | A significance level (either p.FG or p.FS) |
#' | level.adj | numeric | level adjusted to the fraction |
#' | decision | character | Decision which the threshold concerns (Go, Stop or non-negativity) |
#' | ref.value | numeric | Reference value (either tv or lrv) |
#' | percent | numeric | Percentile of the distribution under the null that this threshold represents |
#' | t.quantile | numeric | Quantile of the distribution under the null corresponding to `percent` |
#' | threshold | numeric | Value of the threshold, in units of the `tv`/`lrv` |
#'
#'
#' @export
lalonde_per_domain.Study <- function(obj, params, ...) {
  study <- obj
  assertthat::assert_that(length(list(...)) == 0)

  summary <- study$summary
  endpoints <- study$trial$endpoints

  dd <- tibble::tibble(
    domain    = endpoints$vars$domain,
    name      = endpoints$vars$name,
    se        = sqrt(diag(summary$sigma.delta.hat)),
    df        = summary$degrees.of.freedom,
    tv        = endpoints$vars$tv,
    lrv       = endpoints$vars$lrv
  ) %>%
    rowwise() %>%
    summarise(cur_data(),
      domain.n.vars = {
        endpoints$domains[endpoints$domains$name == .data$domain, ]$n.vars[1]
      },
      {
        domain.n.vars <- .data$domain.n.vars
        e1 <- tibble(
          required = seq(from = domain.n.vars, to = 1, by = -1),
          fraction = .data$required / domain.n.vars,
          level = params$p.FG,
          level.adj = .data$fraction * .data$level,
          decision = "Go",
          ref.value = cur_data()$lrv,
          percent = 1 - .data$level.adj
        )
        e2 <- tibble(
          required = domain.n.vars,
          fraction = 1,
          level = params$p.FS,
          level.adj = .data$fraction * .data$level,
          decision = "Stop",
          ref.value = cur_data()$tv,
          percent = .data$level.adj
        )
        e3 <- tibble(
          required = seq(from = domain.n.vars, to = 1, by = -1),
          fraction = .data$required / domain.n.vars,
          level = params$alpha,
          level.adj = .data$fraction * .data$level,
          decision = "NoNeg",
          ref.value = 0,
          percent = .data$level.adj
        )
        e <- bind_rows(e1, e2, e3)
        e
      }
    )
  dd <- dd %>%
    rowwise() %>%
    mutate(
      t.quantile = qt(p = .data$percent, df = .data$df),
      threshold = .data$ref.value + .data$t.quantile * .data$se
    )
  dd
}

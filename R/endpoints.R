#' Endpoints class
#'
#' An Endpoints object stores the variables that will be measured in
#' a study, their covariance structure, and the reference
#' values for each endpoint.
#'
#' @param sigma  The (a priori) covariance matrix. Needed for simulation.
#' @param domains  A data frame containing one row per domain, and the following
#'   columns:
#'
#' | Parameter | Type      | Description              |
#' | --------- | --------- | ------------------------ |
#' | name      | character | The name of the domain   |
#' | n.vars    | numeric   | Number of variables in the domain |
#'
#' @param vars   A data frame containing one row per variable, and the
#'   following columns:
#'
#' | Parameter | Type      | Description              |
#' | --------- | --------- | ------------------------ |
#' | name      | character | The name of the variable |
#' | domain    | character | The name of the domain the variable belongs to |
#' | sd        | numeric   | Standard deviation of the variable |
#' | no.effect | numeric   | The No-Effect value. Must be 0. |
#' | lrv       | numeric   | The Lower Reference Value (LRV) |
#' | tv        | numeric   | The Target Value (TV) |
#'
#' @param .params  Additional information about how the endpoints
#'   are defined. For instance, they may have been defined in
#'   terms of an intra-domain and an inter-domain correlation.
#'
#' @seealso [make_endpoints()]
#'
#' @export
Endpoints <- function(sigma,
                      domains,
                      vars,
                      .params) {
  assertthat::assert_that(all(vars$no.effect == 0))

  s <- tibble::lst(sigma, domains, vars, .params)
  base::structure(s, class = "Endpoints")
}

#' Endpoints builder
#'
#' Helper function to define endpoints given domains, correlations, and
#' relevant/target values for each endpoint.
#'
#' @param n.domains     Number of domains
#' @param domain.n.vars   List of number of variables for each domain
#' @param corr.inter    Correlation between variables in different domains
#' @param corr.intra    Correlation between variables in the same domain
#' @param vars.sd  Standard deviations for each variable
#' @param vars.no.effect  Must be 0
#' @param vars.lrv  The Lower Reference Value (LRV)
#' @param vars.tv   The Target Value (TV)
#' @param domain.names Names of the domains.
#' @param vars.names Names of the endpoints.
#'
#' @returns [Endpoints()] object
#'
#' @details
#' The arguments `corr.inter` and `corr.intra` are scalars: the correlation
#' between variables depends only on whether they are in the same domain
#' or in a different domain.
#'
#' The argument `domain.n.vars` is recycled into a vector of length
#' `n.domains`, as follows:
#'
#' | Original length of `domain.n.vars` | Behaviour |
#' | ---------------------------------- | --------- |
#' |   1    |  The value is the same for all variables |
#' | `n.domains` | No recycling occurs; each domain has a different number of variables. |
#' | other       | *Unsupported* |
#'
#' Let `n.vars <- sum(domain.n.vars)` (where `domain.n.vars` is the
#' recycled value there of, according to the above).
#'
#' The arguments of the form `vars.*` are each individually
#' recycled into a vector of length `n.vars`:
#'
#' | Original length of `vars.*`    | Behaviour |
#' | ------------------------------ | --------- |
#' |   1    |  The value is the same for all variables |
#' | `n.domains` | The value is the same for all variables in each domain. |
#' | `n.vars`    | No recycling occurs; each variable has its own value |
#' | other       | *Unsupported* |
#'
#' @export
make_endpoints <- function(n.domains = 4,
                           domain.n.vars = 1,
                           corr.inter = 0,
                           corr.intra = 0,
                           vars.sd = 1,
                           vars.no.effect = responses.default$NoEffect,
                           vars.lrv = responses.default$LRV,
                           vars.tv = responses.default$TV,
                           domain.names = NULL,
                           vars.names = NULL) {

  # Check n.domains
  stopifnot(n.domains > 0)

  # Check domain.n.vars
  if (length(domain.n.vars) == 1) {
    domain.n.vars <- rep(domain.n.vars, n.domains)
  }
  stopifnot(length(domain.n.vars) == n.domains)
  stopifnot(all(domain.n.vars > 0))

  n.vars <- sum(domain.n.vars)

  if (n.domains > 1) {
    # Check corr.inter
    stopifnot(is.scalar(corr.inter))
    stopifnot(-1 <= corr.inter && corr.inter <= 1)
  } else {
    corr.inter <- NA
  }

  if (any(domain.n.vars > 1)) {
    # Check corr.intra
    stopifnot(is.scalar(corr.intra))
    stopifnot(-1 <= corr.intra && corr.intra <= 1)
  } else {
    corr.intra <- NA
  }

  if (!is.na(corr.inter) &&
    !is.na(corr.intra) &&
    corr.inter > corr.intra) {
    warning("corr.inter > corr.intra")
  }

  # Check vars.sd
  vars.sd <- recycle.to.vars(xs = vars.sd, domain.n.vars)
  stopifnot(length(vars.sd) == n.vars)

  # Recycle vars.tv
  vars.tv <- recycle.to.vars(xs = vars.tv, domain.n.vars)
  stopifnot(length(vars.tv) == n.vars)

  # Recycle vars.lrv
  vars.lrv <- recycle.to.vars(xs = vars.lrv, domain.n.vars)
  stopifnot(length(vars.lrv) == n.vars)

  # Recycle vars.no.effect
  vars.no.effect <- recycle.to.vars(xs = vars.no.effect, domain.n.vars)
  stopifnot(length(vars.no.effect) == n.vars)

  # Make names
  if (is.null(domain.names)) {
    domain.names <- paste0("d", seq(n.domains))
  }
  assertthat::assert_that(length(domain.names) == n.domains)
  assertthat::assert_that(!any(duplicated(domain.names)))

  vars.domain <- rep(domain.names, times = domain.n.vars)

  if (is.null(vars.names)) {
    vars.names <- paste0(
      rep(domain.names, times = domain.n.vars),
      ".v",
      unlist(lapply(domain.n.vars, seq))
    )
  }
  assertthat::assert_that(length(vars.names) == n.vars)
  assertthat::assert_that(!any(duplicated(vars.names)))

  # COMPUTE CORRELATION MATRIX
  # ============================
  sigma <- matrix(0, nrow = n.vars, ncol = n.vars)
  # Make a lookup table to check to which domain each variable belongs

  # TODO Could use some vector-foo to avoid the nested loops,
  # but the matrix is too small for it to matter
  for (i in seq(n.vars)) {
    for (j in seq(n.vars)) {
      if (i == j) {
        sigma[i, j] <- vars.sd[i]^2
        # Covariance for variables within the same domain
      } else if (vars.domain[i] == vars.domain[j]) {
        sigma[i, j] <- vars.sd[i] * vars.sd[j] * corr.intra
      } else {
        sigma[i, j] <- vars.sd[i] * vars.sd[j] * corr.inter
      }
      # Covariance for variables in unrelated domains
    }
  }
  if (!(all(eigen(sigma)$values > 0))) {
    warning("sigma is not positive definite")
  }

  Endpoints(
    sigma = sigma,
    domains = tibble::tibble(name = domain.names, n.vars = domain.n.vars),
    vars = tibble::tibble(
      name = vars.names,
      domain = vars.domain,
      sd = vars.sd,
      no.effect = vars.no.effect,
      lrv = vars.lrv,
      tv = vars.tv
    ),
    .params = tibble::lst(
      corr.inter,
      corr.intra,
      domain.n.vars
    )
  )
}

#' Total number of endpoints
#' @param x Object
#' @export
n_vars <- function(x) UseMethod("n_vars")

#' @describeIn n_vars Number of endpoints.
#' @export
n_vars.Endpoints <- function(x) nrow(x$vars)

#' Total number of domains
#' @param x Object
#' @export
n_domains <- function(x) UseMethod("n_domains")

#' @describeIn n_domains Number of domains.
#' @export
n_domains.Endpoints <- function(x) nrow(x$domains)


#' Recycle a variable value vector
#'
#' Recycle a vector to obtain one value for each
#' variable. This is done hierarchically: the vector
#' may contain one single value, one value per domain,
#' or one value per variable.
#'
#' @param obj Object containing endpoint information.
#' @param xs Vector
#'
#' @export
recycle_vars <- function(obj, xs) UseMethod("recycle_vars")

#' @describeIn recycle_vars [Endpoints()]
#' @export
recycle_vars.Endpoints <- function(obj, xs) {
  endpoints <- obj
  if (length(xs) == 1) {
    xs <- rep(xs, times = n_vars(endpoints))
  } else if (length(xs) == n_domains(endpoints)) {
    xs <- xs[match(endpoints$vars$domain, endpoints$domain$name)]
  }
  assertthat::assert_that(length(xs) == n_vars(endpoints))
  xs
}

#' Remove creation parameter and underlying data
#' information from an object.
#'
#' @param x Object
#' @returns The same object `x` but without creation-related parameters.
#'
#' @export
scrub <- function(x) UseMethod("scrub")

#' @describeIn scrub Remove parameters from an endpoints object.
#' @export
scrub.Endpoints <- function(x) {
  e <- x
  e$.params <- NULL
  e
}

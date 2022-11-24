#' NPatients
#'
#' Stores the sizes of the patient groups in a study
#'
#' @param total   The total number of patients
#' @param active  The total number of patients in the treatment group
#' @param control The total number of patients in the control group
#' @param .params Original parameters used to define the NPatients object
#'   (for example, if it was defined simply in terms of the number of patients
#'    per arm, that would be included here)
#'
#' @details
#' It is always the case that `total == active + control`
#'
#' @seealso [make_n_patients()]
#'
#' @export
NPatients <- function(total,
                      active,
                      control,
                      .params) {
  assertthat::assert_that(total == active + control)


  structure(tibble::lst(total, active, control, .params),
    class = "NPatients"
  )
}

#' NPatients builder
#'
#' Helper function to define the number of patients in a study
#'
#' @param per.group  number of patients in each group
#' @param control    number of patients in the control group
#' @param active     number of patients in the treatment group
#'
#' @details
#'
#' The function can be used in two ways:
#' - Only `per.group` is given. Then `active <- control <- per.group`.
#' - Both `control` and `active` are given.
#'
#' @return [NPatients()] object
#'
#' @export
make_n_patients <- function(per.group = NULL,
                            control = NULL,
                            active = NULL) {
  if (!is.null(per.group)) {
    stopifnot(all(
      is.null(control),
      is.null(active)
    ))
    stopifnot(is.scalar(per.group))
    active <- per.group
    control <- per.group
  } else {
    stopifnot(is.scalar(active))
    stopifnot(is.scalar(control))
  }
  NPatients(
    total = active + control,
    active = active,
    control = control,
    .params = tibble::lst(per.group)
  )
}


#' Trial class
#'
#' Details the endpoints and the number of patients of a trial.
#'
#' @details
#' This is not a clinical trial protocol in proper sense of the word
#' as it omits most of the details that would be needed to consider it as such.
#'
#' @param endpoints Endpoints (see [Endpoints()])
#' @param n.patients  Number of patients (see [NPatients()])
#' @param vars  Data frame containing one row for each variable, and the
#'   following fields:
#'
#' | Column | Type      | Description |
#' | ------ | ----      | ----------- |
#' | name   | character | Name of the variable |
#' | active | numeric   | A priori value for the active group |
#' | control | numeric  | A priori value for the control group |
#' | effect  | numeric  | ´active` - `control` |
#'
#' The ordering of the rows should be consistent with the order
#' of rows in `endpoints$vars`.
#'
#' @details
#' Trial specifications may or may not contain a priori values for
#' variable `effect` and for the `active` group.
#'
#' To check whether a trial contains a priori information,
#' use [has_ground_truth()].
#'
#' @seealso [make_trial()]
#'
#' @export
Trial <- function(endpoints,
                  n.patients,
                  vars) {
  assertthat::assert_that(class(endpoints) == "Endpoints")
  assertthat::assert_that(is.null(n.patients) ||
    class(n.patients) == "NPatients")
  assertthat::assert_that(is.data.frame(vars))
  assertthat::has_name(vars, c("name", "active", "control", "effect"))

  structure(tibble::lst(
    endpoints,
    n.patients,
    vars
  ),
  class = "Trial"
  )
}

#' @describeIn n_domains Number of domains in a trial
#' @export
n_domains.Trial <- function(x) n_domains(x$endpoints)

#' @describeIn n_vars Number of endpoints in a trial
#' @export
n_vars.Trial <- function(x) n_vars(x$endpoints)

#' Make trial
#'
#' Helper function to define a trial for a study
#'
#' @param endpoints Endpoints (see [make_endpoints()])
#' @param n.patients Number of patients (see [make_n_patients()])
#' @param n.patients.per.group If `n.patients` is not given, then
#'   `n.patients` will be set to `make_n_patients(per.group = n.patients.per.group)`.
#' @param vars.effect A priori value of each variable. (vector or list).
#'   See [parse_effect_vector()].
#' @param ... Other arguments (passed through to [make_endpoints()]).
#'
#' @export
#'
make_trial <- function(endpoints = NULL,
                       n.patients = NULL,
                       vars.effect = NA,
                       n.patients.per.group = NULL,
                       ...) {
  if (is.null(endpoints)) {
    endpoints <- make_endpoints(...)
  } else {
    if (length(list(...)) > 0) {
      warning(paste0(
        "endpoints = ",
        deparse1(substitute(endpoints)),
        " given, arguments ",
        stringr::str_remove(
          deparse1(substitute(list(...))),
          "^list"
        ), " will be ignored"
      ))
    }
  }
  assertthat::assert_that(is(endpoints, "Endpoints"))
  if (is.null(n.patients)) {
    if (!is.null(n.patients.per.group)) {
      n.patients <- make_n_patients(per.group = n.patients.per.group)
    }
  } else {
    assertthat::assert_that(is(n.patients, "NPatients"))
    assertthat::assert_that(is.null(n.patients.per.group))
  }

  Trial(endpoints,
    n.patients,
    vars = tibble::tibble(
      name = endpoints$vars$name,
      control = endpoints$vars$no.effect
    )
  ) %>% with_ground_truth(vars.effect)
}

#' Specifying the number of patients per arm
#'
#' Creates a trial from given endpoints by specifying the number of
#' patients in each arm.
#'
#' @param endpoints [Endpoints()] object, specifying the endpoints
#'   (use [make_endpoints()] to create it).
#' @param n.patients [NPatients()] object, specifying the number of patients
#'   in each arm. Use [make_n_patients()] to create it.
#' @param per.group  A scalar; the number of patients in each arm.
#'
#' @details
#'
#' Either `n.patients` or `per.group` must be given, but not both.
#'
#' @returns A [Trial()] object.
#'
#' @export
with_n_patients <- function(endpoints, n.patients = NULL, per.group = NULL) {
  make_trial(endpoints, n.patients = n.patients, n.patients.per.group = per.group)
}

#' Adds an effect
#'
#' @param trial  A [Trial()] object.
#' @param vars.effect Effect specification (see `vars.effect` argument of
#'        [parse_effect_vector()]).
#'
#' @returns A [Trial()] object with the ground truth set to the given
#'   true effect.
#'
#' @export
with_ground_truth <- function(trial, vars.effect) {
  assertthat::assert_that(is(trial, "Trial"))
  active <- parse_effect_vector(vars.effect, trial$endpoints)
  trial$vars$active <- unlist(active)
  trial$vars$effect <- active - trial$vars$control
  trial
}

#' @describeIn with_ground_truth Alias for `with_ground_truth()`
#' @export
with_true_effect <- with_ground_truth

#' Parse an effect vector
#'
#' Create a vector of true effects based on a "human-readable" specification.
#'
#' @param x Object containing endpoints information.
#' @param vars.effect Effect specification (see `vars.effect` argument of
#'        [parse_effect_vector()]).
#'
#' @returns A vector of length `n_vars(x)`; i.e. the total number of endpoints
#'   (see [n_vars()]).
#'
#' @export
parse_effect <- function(x, vars.effect) UseMethod("parse_effect")

#' @describeIn parse_effect Use endpoints directly.
#' @export
parse_effect.Endpoints <- function(x, vars.effect) {
  endpoints <- x
  parse_effect_vector(vars.effect, endpoints)
}

#' @describeIn parse_effect Use endpoints from a Trial.
#' @export
parse_effect.Trial <- function(x, vars.effect) {
  trial <- x
  parse_effect(trial$endpoints, vars.effect)
}

#' @describeIn parse_effect Use endpoints from the Trial used for a Study.
#' @export
parse_effect.Study <- function(x, vars.effect) {
  study <- x
  parse_effect(study$trial, vars.effect)
}

#' @describeIn parse_effect Allow for parsing a NULL effect vector
#' @export
parse_effect.NULL <- function(x, vars.effect) {
  NULL
}

#' Parse an effect vector
#'
#' Create a vector of true effects based on a "human-readable" specification.
#' See also [parse_effect()] for more concise usage.
#'
#' @param vars.effect A specification of the true effect vector.
#'   Each of the components must be:
#'   - One of "no.effect","lrv","tv": The corresponding value
#'     is retrieved from `endpoints`.
#'   - NA / numeric(): The value is taken verbatim.
#'   - An expression involving the above (e.g. "2*tv", "-lrv")
#'   - A variable in params.
#'
#'   Note that in R, combining character strings and numbers
#'   in the same vector will result in the latter being converted
#'   to strings. That's no issue. Alternatively, use a list.
#' @param endpoints [Endpoints()].
#' @param params Additional variables used when interpreting the
#'   `vars.effect` vector (named list).
#'
#' @return A vector of length `n_vars(endpoints)`. See [n_vars()].
#'
#' @details
#'
#' The `vars.effect` argument is recycled into a list of length
#' `n_vars(endpoints)`.
#'
#' | Original length of `vars.effect` | Behaviour |
#' |----------------------------------|-----------|
#' |   1    |  The value is the same for all variables |
#' | n_domains(endpoints) | The value is the same for all variables in each domain. |
#' | n_vars(endpoints)    | No recycling. |
#'
#' @examples
#' parse_effect_vector(
#'   c("tv", "x+2", "lrv", 0.8),
#'   make_endpoints(n.domains = 4),
#'   params = list("x" = 42, "y" = 20)
#' )
#' # 1 44 0.5 0.8
#' @export
parse_effect_vector <- function(vars.effect, endpoints, params = NULL) {
  assertthat::assert_that(is(endpoints, "Endpoints"))

  e <- recycle_vars(endpoints, vars.effect %>% as.list())
  env <- endpoints$vars %>% select(.data$no.effect, .data$tv, .data$lrv)
  numbers <- suppressWarnings(as.numeric(e))

  evaled <- lapply(seq(n_vars(endpoints)), function(i) {
    if (is.character(e[[i]])) {
      lcase <- tolower(e[[i]])
      with(
        c(
          params,
          env[i, ],
          list(inf = Inf)
        ),
        eval(parse(text = lcase))
      )
    } else {
      NA
    }
  }) %>% unlist()

  result <- ifelse(lapply(e, is.character),
    evaled,
    numbers
  )


  # Find elements that could not be parsed
  errors.ii <- is.na(result) & !is.na(e)
  assertthat::assert_that(
    !any(errors.ii),
    msg = paste0(
      "The following effect values could not be parsed: ",
      paste(e[[errors.ii]], collapse = ", ")
    )
  )

  result
}

#' Check for existence of ground truth
#'
#' Check whether an object contains the ground truth.
#' The ground truth is necessary for performing simulations.
#' (Lalonde thresholds and variable power can be calculated even
#' without a ground truth).
#'
#' @param x Object
#'
#' @export
has_ground_truth <- function(x) {
  UseMethod("has_ground_truth")
}

#' @describeIn has_ground_truth An [Endpoints()] object has ground truth if
#'   the covariance matrix is fully defined.
#' @export
has_ground_truth.Endpoints <- function(x) {
  endpoints <- x
  with(endpoints, !is.null(sigma) && all(is.numeric(sigma)) &&
    all(!is.na(sigma)))
}

#' @describeIn has_ground_truth A [Trial()] object has ground truth if the
#'   endpoints have ground truth, and the values for the `active` patient
#'   group are defined (i.e. if `vars.effect` was given when constructing it
#'   with [make_trial()]).
#' @export
has_ground_truth.Trial <- function(x) {
  trial <- x
  with(
    trial,
    has_ground_truth(endpoints) &&
      !is.null(vars$control) &&
      all(!is.na(vars$control)) &&
      !is.null(vars$active) &&
      all(!is.na(vars$active))
  )
}

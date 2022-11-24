#' @import methods
NULL

#' Study class
#'
#' Contains the result from a study. The results from a study can
#' be combined with a policy to obtain a decision.
#'
#' @param trial The trial that was used for the study
#' @param data     (optional) The raw data collected from the study. A data frame
#'   containing the following columns:
#'
#'   | Column | Type | Description |
#'   |--------|------|-------------|
#'   | group  | factor("active","control") | Group of the patient |
#'   | …      | numeric | Values for each endpoint in the study
#'                        (i.e. `n_vars(trial)` total columns) |
#' @param summary  Summary of the data collected during the study.
#'   (see [Summary()])
#'
#' @details
#' The `data` field may be null.
#'
#' @export
Study <- function(trial, data, summary) {
  assertthat::assert_that(is(trial, "Trial"))
  assertthat::assert_that(is.null(data) || is.data.frame(data))
  assertthat::assert_that(is(summary, "Summary"))

  structure(tibble::lst(trial, data, summary), class = "Study")
}

n_domains.Study <- function(study) n_domains(study$trial)

scrub.Study <- function(study) {
  study$data <- NULL
  study
}

#' Summary class
#'
#' @param n.patients Number of patients in each group
#' @param vars      Estimated effects for the variables
#'
#'  | Column | Type | Description |
#'  | ------ | ---- | ----------- |
#'  | name   | character | Name of the variable |
#'  | domain    | character | The name of the domain the variable belongs to |
#'  | lrv       | numeric   | The Lower Reference Value (LRV) |
#'  | tv        | numeric   | The Target Value (TV) |
#'  | sd.hat | numeric() | Estimated standard deviation for the variable |
#'  | control.hat | numeric() | Estimate of the mean value for the control group |
#'  | active.hat  | numeric() | Estimate of the mean value for the active group |
#'  | delta.hat   | numeric() | Estimate of the difference between means of control and active groups |
#'  | delta.se    | numeric() | Estimated standard error for delta |
#'
#' @param sigma.hat Estimated covariance matrix
#' @param sigma.delta.hat Estimated covariance matrix for `vars$delta.hat`
#' @param degrees.of.freedom Degrees of freedom
#' @param covariance.structure Covariance structure
#'
#' @seealso [make_exact_study()], [run_simulated_study()]
#'
#' @export
Summary <- function(n.patients,
                    sigma.hat,
                    sigma.delta.hat,
                    degrees.of.freedom,
                    covariance.structure,
                    vars) {
  assertthat::has_name(
    vars,
    c(
      "name", "domain", "lrv", "tv",
      "sd.hat", "control.hat",
      "active.hat", "delta.hat", "delta.se"
    )
  )
  structure(tibble::lst(
    n.patients,
    sigma.hat,
    sigma.delta.hat,
    degrees.of.freedom,
    covariance.structure,
    vars
  ),
  class = "Summary"
  )
}




#' Summaries object
#'
#' @param summaries.list   A list of [Summary()]
#' @param vectorized  A data frame containing one row for each element
#' in summaries, and the following information (all of them `"numeric"`)
#'
#' | Column name          | Description   |
#' |----------------------|---------------|
#' | delta.hat.\[1…V\]      | Observed effects   |
#' | delta.se.\[1…V\]       | Estimated standard errors |
#' | sd.hat.\[1…V\]         | Estimated standard deviations   |
#' | lrv.\[1…V\]            | Lower reference values |
#' | tv.\[1…V\]             | Target values |
#' | degrees.of.freedom.\[1…V\]   | Degrees of freedom for the variance estimation |
#' | covariance.structure.\[1…V\] | Diagonal of the covariance matrix (i.e. variances) |
#' @param vars Information about the endpoints that the summaries refer to.
#'
#' @export
Summaries <- function(summaries.list,
                      vectorized,
                      vars) {
  assertthat::assert_that(is.null(summaries.list) || is(summaries.list, "list"))
  assertthat::assert_that(is(vectorized, "data.frame"))
  assertthat::assert_that(is(vars, "data.frame"))
  structure(tibble::lst(
    summaries.list = summaries.list,
    vectorized = vectorized,
    vars = vars
  ),
  class = "Summaries"
  )
}

attr(Summaries, "vectorized.names") <-
  c(
    "domain", "sd.hat", "delta.hat", "delta.se", "lrv", "tv",
    "degrees.of.freedom", "covariance.structure"
  )

#' Standardizes the form of a summaries object.
#'
#' @param summaries Either:
#'   - A single [Summary()] object.
#'   - A list of [Summary()] objects.
#'   - A [Summaries()] object containing summary statistics for a number of studies.
#' @param vectorized A data frame containing summaries for a number of runs of a study.
#'
#' @returns A [Summaries()] object.
#'
#' @export
make_summaries <- function(summaries, vectorized = NULL) {
  if (is(summaries, "Summaries")) {
    return(summaries)
  } else if (is(summaries, "Summary")) {
    summaries.list <- list(summaries)
  } else if (is(summaries, "list")) {
    summaries.list <- summaries
  } else {
    assertthat::assert_that(is.null(summaries))
    summaries.list <- NULL
  }

  if (is.null(vectorized)) {
    assertthat::assert_that(!is.null(summaries.list))
    vectorized <- bind_rows(
      lapply(summaries.list, function(summary) {
        values_from <- attr(Summaries, "vectorized.names")
        tidyr::pivot_wider(
          data = summary$vars
            # Replicate degrees.of.freedom and covariance.structure across variables
            %>% mutate(
              degrees.of.freedom = !!summary$degrees.of.freedom,
              covariance.structure = !!summary$covariance.structure
            )
            %>% select(c("name", all_of(values_from))),
          names_from = "name",
          names_sep = ".",
          values_from = all_of(values_from)
        )
      })
    )
  }

  vars <- tidyr::pivot_longer(
    vectorized %>%
      select(starts_with("domain.")) %>%
      distinct(),
    cols = everything(),
    names_to = "name",
    names_prefix = "domain.",
    values_to = "domain"
  )
  assertthat::assert_that(!any(duplicated(vars$name)))

  Summaries(
    summaries.list,
    vectorized,
    vars
  )
}


#' Select summary information for a specific endpoint
#'
#' @param summaries A [Summaries()] object.
#' @param var.name The name of an endpoint.
#' @returns A tibble containing the columns in the `.$vectorized` field
#'   of `summaries` corresponding to that endpoint, without the suffixes
#'   (all of them are `"numeric"` columns)
#'
#' | Column name          | Description   |
#' |----------------------|---------------|
#' | delta.hat      | Observed effect   |
#' | delta.se    | Estimated standard error |
#' | sd.hat         | Estimated standard deviation   |
#' | lrv          | Lower reference value |
#' | tv          | Target value |
#' | degrees.of.freedom   | Degrees of freedom for the variance estimation |
#' | covariance.structure | Entry in the diagonal of the covariance matrix (i.e. Variance) |
#'
#' @export
summaries_at_endpoint_vectorized <- function(summaries, var.name) {
  assertthat::assert_that(is(summaries, "Summaries"))
  suffix <- paste0(".", var.name)
  res <- summaries$vectorized %>%
    select(ends_with(suffix)) %>%
    rename_with(~ stringr::str_remove(., paste0("\\Q", suffix, "\\E$")))
  assertthat::has_name(res, which = attr(Summaries, "vectorized.names"))
  res
}

#' @describeIn n_vars Number of variables in a summary
#' @export
n_vars.Summary <- function(x) nrow(x$vars)

#' @describeIn n_vars Number of variables in a study
#' @export
n_vars.Study <- function(x) n_vars(x$trial)

#' @describeIn make_exact_study An alias for [make_exact_study()].
#' @param ... Arguments (passed through to [make_exact_study()])
#' @export
run_exact_study <- function(...) make_exact_study(...)

#' Simulate a study
#'
#' Creates a random [Study()] by simulating from the distribution of
#' the trial.
#'
#' @param trial Study trial.
#'   MUST be simulable (i.e. `has_ground_truth(trial) == TRUE`).
#' @param only.summary Whether to include the patient-level data (`FALSE`)
#'   or include only the aggregate statistics (`TRUE`).
#'
#' @returns [Study()]
#'
#' @export
run_simulated_study <- function(trial, only.summary = FALSE) {
  assertthat::assert_that(has_ground_truth(trial))
  # Simulate data for each of the two groups of patients
  is.active <- with(
    trial$n.patients,
    c(rep(0, control), rep(1, active))
  )
  is.control <- 1 - is.active
  y <- is.active %*% t(trial$vars$active) +
    is.control %*% t(trial$vars$control) +
    mvtnorm::rmvnorm(trial$n.patients$total, sigma = trial$endpoints$sigma)
  colnames(y) <- trial$endpoints$vars$name

  # Create data frame
  data <- cbind(
    tibble::tibble(
      group = factor(ifelse(is.active, "active", "control"),
        levels = c("control", "active")
      ),
    ),
    as_tibble(y)
  )

  Study(
    trial,
    summary = summarize_data(trial, data),
    data = if (only.summary) {
      NULL
    } else {
      data
    }
  )
}

#' Exact study
#'
#' Creates a [Study()] whose summary statistics match the a priori values
#' from the trial.
#'
#' @param trial [Trial()] specification to base the study on.
#' @param only.summary  For analogy with [run_simulated_study()].
#'   MUST be set to `TRUE`.
#'
#' @returns [Study()] (no `data` included)
#'
#' @details
#' This function is useful for reusing code between the
#' a priori and a posteriori analyses.
#'
#' @export
make_exact_study <- function(trial, only.summary = TRUE) {
  assertthat::assert_that(is(trial, "Trial"))
  assertthat::assert_that(only.summary)

  sigma.delta.hat <- with(
    trial, endpoints$sigma * with(n.patients, (1 / active + 1 / control))
  )
  summary <- Summary(
    n.patients = trial$n.patients,
    sigma.hat = trial$endpoints$sigma,
    sigma.delta.hat,
    vars =
      dplyr::bind_cols(
        trial$endpoints$vars %>%
          dplyr::select(.data$name, .data$domain, .data$lrv, .data$tv),
        with(
          trial,
          tibble::tibble(
            sd.hat = endpoints$vars$sd,
            control.hat = vars$control,
            active.hat = vars$active,
            delta.hat = vars$active - vars$control,
            delta.se = sqrt(diag(sigma.delta.hat))
          )
        )
      ),
    degrees.of.freedom = +Inf,
    covariance.structure = "theoretical"
  )

  Study(trial, data = NULL, summary)
}

#' Simulate a study
#'
#' Creates a random [Study()] by simulating from the distribution of
#' the trial.
#'
#' @param trial Study trial.
#'   MUST be simulable (i.e. `has_ground_truth(trial) == TRUE`).
#' @param random.mean Whether to take the mean as a randomly distributed,
#' or as the expected value.
#' @param covariance.structure Which covariance structure to use as the result of
#' the simulation.
#' @param M Number of elements to sample.
#' @param fast If TRUE, skips returning a list of individual summaries. This
#' means that it will not be usable by policies that rely on the individual
#' summaries; e.g. use the covariance matrix.
#'
#' @returns List of [Summary()]
#'
#' @export
run_study_summaries <- function(trial,
                                random.mean = TRUE,
                                covariance.structure =
                                  if (random.mean) {
                                    "unstructured"
                                  } else {
                                    "theoretical"
                                  },
                                M = 1,
                                fast = FALSE) {
  assertthat::assert_that(has_ground_truth(trial))
  if (M == 0) {
    return(list())
  }

  sigma.mu.0 <- with(
    trial, endpoints$sigma * with(n.patients, (1 / control))
  )
  sigma.mu.1 <- with(
    trial, endpoints$sigma * with(n.patients, (1 / active))
  )
  sigma.mu <- with(
    trial, endpoints$sigma * with(n.patients, (1 / active + 1 / control))
  )

  # Simulate mu.hat from its distribution
  if (random.mean) {
    control.hat <- mvtnorm::rmvnorm(n = M, mean = trial$vars$control, sigma = sigma.mu.0)
    active.hat <- mvtnorm::rmvnorm(n = M, mean = trial$vars$active, sigma = sigma.mu.1)
  } else {
    control.hat <- t(replicate(M, trial$vars$control))
    active.hat <- t(replicate(M, trial$vars$active))
  }
  delta.hat <- active.hat - control.hat

  # Simulate sigma.hat from its distribution
  if (covariance.structure == COVARIANCE.STRUCTURE$theoretical) {
    degrees.of.freedom <- +Inf
    sigma.hat <- replicate(M, trial$endpoints$sigma)
  } else {
    degrees.of.freedom <- with(trial$n.patients, active + control - 2)
    sigma.hat <- rWishart(
      n = M,
      df = degrees.of.freedom,
      Sigma = trial$endpoints$sigma
    ) / degrees.of.freedom
  }

  # Calculate sigma.delta.hat
  sigma.delta.hat <- sigma.hat * with(trial$n.patients, (1 / active + 1 / control))

  if (!fast) {
    summaries.list <- lapply(seq(M), function(m) {
      sigma.hat <- abind::adrop(sigma.hat[, , m, drop = FALSE], 3)
      sigma.delta.hat <- abind::adrop(sigma.delta.hat[, , m, drop = FALSE], 3)
      Summary(
        n.patients = trial$n.patients,
        sigma.hat = sigma.hat,
        sigma.delta.hat = sigma.delta.hat,
        vars =
          dplyr::bind_cols(
            trial$endpoints$vars %>% dplyr::select(.data$name, .data$domain, .data$lrv, .data$tv),
            tibble::tibble(
              sd.hat = sqrt(diag(sigma.hat)),
              control.hat = control.hat[m, ],
              active.hat = active.hat[m, ],
              delta.hat = delta.hat[m, ],
              delta.se = sqrt(diag(sigma.delta.hat))
            )
          ),
        degrees.of.freedom = degrees.of.freedom,
        covariance.structure = covariance.structure
      )
    })
  } else {
    summaries.list <- NULL
  }

  vars.names <- trial$vars$name
  V <- length(vars.names)
  sd.hat <- sqrt(do.call(base::cbind, lapply(seq(V), function(v) sigma.hat[v, v, , drop = FALSE])))
  delta.se <- sqrt(do.call(base::cbind, lapply(seq(V), function(v) sigma.delta.hat[v, v, , drop = FALSE])))

  vectorized <- bind_cols(
    structure(as_tibble(t(trial$endpoints$vars$domain), .name_repair = "minimal"),
      names = paste0("domain.", vars.names)
    ),
    structure(as_tibble(delta.hat, .name_repair = "minimal"),
      names = paste0("delta.hat.", vars.names)
    ),
    structure(as_tibble(sd.hat, .name_repair = "minimal"),
      names = paste0("sd.hat.", vars.names)
    ),
    structure(as_tibble(delta.se, .name_repair = "minimal"),
      names = paste0("delta.se.", vars.names)
    ),
    structure(as_tibble(t(trial$endpoints$vars$lrv), .name_repair = "minimal"),
      names = paste0("lrv.", vars.names)
    ),
    structure(as_tibble(t(trial$endpoints$vars$tv), .name_repair = "minimal"),
      names = paste0("tv.", vars.names)
    ),
    structure(as_tibble(t(rep(degrees.of.freedom, V)), .name_repair = "minimal"),
      names = paste0("degrees.of.freedom.", vars.names)
    ),
    structure(as_tibble(t(rep(covariance.structure, V)), .name_repair = "minimal"),
      names = paste0("covariance.structure.", vars.names)
    )
  )

  # TODO: Generate vectorized directly instead of from the list
  make_summaries(summaries = summaries.list, vectorized = vectorized)
}

run_study_summary <- function(...) {
  run_study_summaries(M = 1, ...)[[1]]
}


#' @describeIn run_simulated_study Alias for [run_simulated_study()].
#' @export
run_trial <- run_simulated_study

#' Summarize study data
#'
#' Produces a summary of the study data
#'
#' @param trial [Trial()]
#' @param data  A data frame (see `data` in [Study()])
#' @param covariance  The covariance structure.
#'
#' | Name    | Description |
#' |---------|-------------|
#' | unstructured | MLE from the data |
#' | theoretical      | Taken from the trial description |
#'
#' @returns [Summary()]
#'
#' @export
summarize_data <- function(trial, data, covariance = c("unstructured", "theoretical")) {
  covariance <- match.arg(covariance)
  assertthat::assert_that(is(trial, "Trial"))
  assertthat::assert_that(is.data.frame(data))
  # We assume no missing data (supporting missing data is future work)
  assertthat::are_equal(sum(data$group == "active"), trial$n.patients$active)
  assertthat::are_equal(sum(data$group == "control"), trial$n.patients$control)

  datamatrix <- function(d) d %>% select(all_of(trial$endpoints$vars$name))

  # We calculate maximum likelihood estimators for each group means,
  # and for the relative effect (taking placebo as baseline)
  vars.effect.0.hat <- colMeans(data %>% filter(.data$group == "control") %>% datamatrix())
  vars.effect.1.hat <- colMeans(data %>% filter(.data$group == "active") %>% datamatrix())
  vars.delta.hat <- vars.effect.1.hat - vars.effect.0.hat

  # We pool the variance between the two groups
  if (covariance == "unstructured") {
    data.y <- as.matrix(data %>% datamatrix())
    data.y.hat <-
      ((data$group == "active") %*% t(vars.effect.1.hat) +
        (data$group == "control") %*% t(vars.effect.0.hat))
    sigma.hat <- var(data.y - data.y.hat)
    degrees.of.freedom <- with(trial$n.patients, active + control - 2)
  } else if (covariance == "theoretical") {
    sigma.hat <- trial$endpoints$sigma
    degrees.of.freedom <- +Inf
  }
  sigma.delta.hat <- sigma.hat * with(trial$n.patients, 1 / active + 1 / control)

  Summary(
    n.patients = trial$n.patients,
    sigma.hat = sigma.hat,
    vars = dplyr::bind_cols(
      trial$endpoints$vars %>% dplyr::select(.data$name, .data$domain, .data$lrv, .data$tv),
      tibble::tibble(
        sd.hat = sqrt(diag(sigma.hat)),
        control.hat = vars.effect.0.hat,
        active.hat = vars.effect.1.hat,
        delta.hat = vars.delta.hat,
        delta.se = sqrt(diag(sigma.delta.hat))
      )
    ),
    sigma.delta.hat = sigma.delta.hat,
    degrees.of.freedom = degrees.of.freedom,
    covariance.structure = covariance
  )
}

#' Measures the probability of a (generalized) rectangular region of the plane
#' under the distribution (\eqn{\hat{Δ}}).
#'
#' @param study A [Study()] outcome, with [Summary()]
#' @param lt Lowermost, leftmost corner of the rectangle (see [parse_effect_vector])
#' @param gt Uppermost, rightmost corner of the rectangle (see [parse_effect_vector])
#' @returns A number representing the probability that, under a fiducial
#'   distribution where the mean and the covariance matrix are
#'   as observed in the `study`, an observed point will end up in the
#'   rectangle between `lt` and `gt`.
#'
#' @export
probability_study <- function(study, lt = Inf, gt = -Inf) {
  upper <- parse_effect_vector(endpoints = study$trial$endpoints, lt)
  lower <- parse_effect_vector(endpoints = study$trial$endpoints, gt)
  my_pmvt_shifted(
    lower = lower,
    upper = upper,
    delta = study$summary$vars$delta.hat,
    sigma = study$summary$sigma.delta.hat,
    df = study$summary$degrees.of.freedom
  )
}

#' Add a summary to a study
#'
#' (internal use only)
#'
#' @param study [Study()]
#' @param covariance Estimation of the covariance matrix
with_summary <- function(study, covariance = c("unstructured", "theoretical")) {
  covariance <- match.arg(covariance)
  if (study$summary$covariance.structure == covariance) {
    study
  } else {
    study$summary <- summarize_data(
      study$trial,
      study$data,
      covariance = covariance
    )
  }
  study
}

#' @import dplyr tibble tidyr
#' @importFrom grDevices rgb
#' @importFrom dplyr filter
NULL

EFFECT_VALUE_PREFIX <- "effect.for."

#' Scenarios corresponding to a specification of suitability criteria
#' for a study design, including a sample size.
#'
#' @param trial [Trial()], including endpoints and sample size
#' @param go.regions (Named) list of [Region()]s for which a Go decision is important.
#' @param stop.regions (Named) list of [Region()]s for which a Go decision is important.
#'
#' @returns An object of class "Scenario"
#'
#' @export
Scenario <- function(trial,
                     go.regions,
                     stop.regions) {
  structure(tibble::lst(
    trial,
    go.regions,
    stop.regions
  ),
  class = "Scenario"
  )
}

#' Build a scenario
#'
#' - See [with_true_effects()] for a more user-friendly version.
#'
#' @inheritParams Scenario
#'
#' @export
make_scenario <- function(trial = NULL,
                          go.regions = NULL,
                          stop.regions = NULL) {
  assertthat::assert_that(!is.null(trial))
  if (has_ground_truth(trial)) {
    warning("True effect from trial will be ignored")
  }

  if (is.null(go.regions)) {
    warning("No go.regions specified")
    go.regions <- list()
  } else {
    assertthat::assert_that(is(go.regions, "list"))
  }
  assertthat::assert_that(
    all(sapply(
      go.regions,
      function(r) {
        region_type(r) == DIRECTION$Up &&
          is_lower_bounded(r, trial$endpoints)
      }
    )),
    msg = "One of the go.regions is not lower-bounded"
  )

  if (is.null(stop.regions)) {
    warning("No stop.regions specified")
    stop.regions <- list()
  } else {
    assertthat::assert_that(is(stop.regions, "list"))
  }
  assertthat::assert_that(
    all(sapply(
      stop.regions,
      function(r) {
        region_type(r) == DIRECTION$Down &&
          is_upper_bounded(r, trial$endpoints)
      }
    )),
    msg = "One of the stop.regions is not upper-bounded"
  )
  names(go.regions) <- vctrs::vec_as_names(
    rlang::names2(go.regions),
    repair = "unique"
  )
  names(stop.regions) <- vctrs::vec_as_names(
    rlang::names2(stop.regions),
    repair = "unique"
  )

  Scenario(trial, go.regions, stop.regions)
}

#' Build a scenario from a trial by specifying true effect regions.
#'
#' @param trial A [Trial()] object (see [make_endpoints()] and [with_n_patients()],
#'   or [make_trial()]). If `trial` [has_ground_truth()], this will be ignored,
#'   and a warning thrown.
#' @param go.when A named list of regions for which a `Go` decision (rather than
#'   `Stop` decision) is preferred. These regions must be closed upwards
#'   (see [region_type()]) and lower-bounded (see [is_lower_bounded()]).
#' @param stop.when A named list of regions for which a `Stop` (rather than
#'   a `Go` decision is preferred. These regions must be closed downwards
#'   (see [region_type()]) and upper-bounded (see [is_upper_bounded()]).
#'
#' @returns A [Scenario()] with the associated regions.
#'
#' @details
#'
#' The names `go.when` and `stop.when` are only for the purposes
#' of aggregating and naming the simulation probabilities
#' (see [four_metrics()]).
#' The important part is that they fulfill the region-type and lower/upper-bounded
#' criteria.
#'
#' @examples
#' s <- make_endpoints(n.domains = 2, domain.n.vars = c(2, 1)) |>
#'   with_n_patients(per.group = 20) |>
#'   with_true_effects(
#'     go.when = list(
#'       all.tv = is_gt("tv"),
#'       one.tv = at_least_1(at_each_domain(is_gt("tv"))) %and% is_gt(0)
#'     ),
#'     stop.when = list(
#'       all.0 = is_lt(0)
#'     )
#'   )
#' s |> get_effect_points()
#' # A tibble: 4 × 6
#' # correct.decision region.name effect.name effect.for.d1.v1 effect.for.d1.v2 effect.for.d2.v1
#' # <fct>            <chr>       <chr>                  <dbl>            <dbl>            <dbl>
#' # 1 Go               all.tv      all.tv.1                   1                1                1
#' # 2 Go               one.tv      one.tv.1                   1                1                0
#' # 3 Go               one.tv      one.tv.2                   0                0                1
#' # 4 Stop             all.0       all.0.1                    0                0                0
#' @export
with_true_effects <- function(trial,
                              go.when = NULL,
                              stop.when = NULL) {
  s <- make_scenario(trial,
    go.regions = c(go.when),
    stop.regions = c(stop.when)
  )
  s <- expand_regions(s)
  s
}

#' Returns a data frame with the minimal true effects
#' from `go.regions` and `stop.regions`.
#'
#' @param scenario A scenario as returned by [with_true_effects()].
#'
#' @returns A data frame with the following columns:
#'
#' | Column | Type | Description |
#' |--------|------|-------------|
#' | correct.decision | factor | Go or Stop, depending on the region the effect belongs to. |
#' | region.name | character | Name of the region |
#' | effect.name | character | Auto-generated name for the effect (region.name + counter) |
#' | effect.for.`<name>` | double | True effect for endpoint `<name>` |
#'
#' @details
#' See [with_true_effects()] for an example.
#'
#' @export
get_effect_points <- function(scenario) {
  if (is.null(scenario$region.points)) {
    scenario <- scenario %>% expand_regions()
  }
  scenario$region.points
}

#' Get the decisions for each true effect from a scenario.
#'
#' @param scenario A scenario as returned by [with_params()].
#'
#' @return The `.$decisions.per.effect` field
#'   as produced by [aggregate_scenario()].
#'
#' @export
get_decisions_per_effect <- function(scenario) {
  if (is.null(scenario$decisions.per.effect)) {
    scenario <- scenario %>% aggregate_scenario()
  }
  scenario$decisions.per.effect
}


#' Calculates the minimal points of the go and stop regions of the scenario.
#'
#' This function is called automatically by [with_true_effects()].
#'
#' @param scenario [Scenario()] specification.
#'
#' @returns [Scenario()] on which [get_effect_points()] can be
#'   called.
#'
#' @export
expand_regions <- function(scenario) {
  # all.regions <-
  # Unpack the points from the regions
  assertthat::assert_that(is(scenario, "Scenario"))
  regions <- bind_rows(
    lapply(
      names(scenario$go.regions),
      function(name) {
        tibble::tibble_row(
          correct.decision = DECISION$Go,
          region.name = paste0(name),
          region = list(scenario$go.regions[[name]])
        )
      }
    ),
    lapply(
      names(scenario$stop.regions),
      function(name) {
        tibble::tibble_row(
          correct.decision = DECISION$Stop,
          region.name = paste0(name),
          region = list(scenario$stop.regions[[name]])
        )
      }
    )
  )

  region.points <- regions %>%
    rowwise() %>%
    summarise(bind_rows({
      # region <- cur_data()$region
      region.name <- .data$region.name
      .params <- cur_data() %>% mutate(region = NULL)
      points <- limit_points(.data$region, scenario$trial$endpoints)
      lapply(seq(length(points)), function(i) {
        point <- as.list(points[[i]])
        names(point) <- paste0(EFFECT_VALUE_PREFIX, scenario$trial$endpoints$vars$name)
        inner_join(
          by = character(),
          .params,
          tibble::tibble(
            effect.name = paste0(region.name, ".", i),
            tibble::as_tibble(point)
          )
        )
      })
    })) %>%
    ungroup()


  scenario$region.points <- region.points

  scenario
}

#' Simulate scenario with policies
#'
#' Simulates the outcome of several policies on the true effects given by a scenario.
#'
#' @param scenario A [Scenario()]. See [with_true_effects()].
#' @param policies A named list of policy schemas.
#' @param N Number of studies to simulate for each combination of true effect
#'   and policy.
#' @param cores Number of cores.
#' @param splits Number of trial splits.
#' @param verbose Show progress information.
#' @param simulate.fast Whether to simulate the estimators directly from their
#'   distribution instead of calculating them from simulated data.
#'
#' @return A named list of [Scenario()], one for each policy.
#'   Each of them contain policy criteria.
#'   If the type of the policy schema is "Policy",
#'   the function [with_params()] can be applied to obtain
#'   decisions.
#'
#' @details
#' The arguments `cores`, `splits`, `verbose` and `simulate.fast`
#' are better explained in [run_montecarlo_criteria()].
#'
#' @examples
#' ss <- make_endpoints(n.domains = 1, domain.n.vars = 1) |>
#'   with_n_patients(per.group = 17) |>
#'   with_true_effects(
#'     go.when   = list(),
#'     stop.when = list(all.lrv = is_lt("lrv"))
#'   ) |>
#'   simulate_with_policies(
#'     list(lalonde = p_lalonde),
#'     N = 3
#'   )
#' ss$lalonde$all.criteria
#' @seealso [simulate_with_policy()] for a single policy.
#' @export
simulate_with_policies <- function(scenario,
                                   policies,
                                   N = 100,
                                   cores = 1,
                                   splits = 1,
                                   verbose = FALSE,
                                   simulate.fast = FALSE) {
  assertthat::assert_that(is(scenario, "Scenario"))
  if (is.null(scenario$region.points)) {
    scenario <- scenario %>% expand_regions()
  }
  region.points <- scenario$region.points

  assertthat::assert_that(is(policies, "list"))
  assertthat::assert_that(all(sapply(policies, function(x) is(x, "Schema"))))

  names(policies) <- vctrs::vec_as_names(
    rlang::names2(policies),
    repair = "unique"
  )
  policies <- lapply(policies, function(policy) {
    policy %>% with_criteria_prefix("criteria.")
  })
  policy.params <- tibble::tibble(policy.name = names(policies))

  all.criteria <- run_montecarlo_criteria(
    trial.params = region.points,
    fun.trial = function(.trial.params) {
      effect <- .trial.params %>%
        select(starts_with(EFFECT_VALUE_PREFIX)) %>%
        as.numeric()
      scenario$trial %>% with_ground_truth(effect)
    },
    policy.params = policy.params,
    fun.policy = function(.params) policies[[.params$policy.name]],
    N = N,
    cores = cores,
    trial.splits = splits,
    verbose = verbose,
    simulate.fast = simulate.fast
  )



  sapply(
    simplify = F, USE.NAMES = T, names(policies),
    function(policy.name) {
      scenario$policy <- policies[[policy.name]]
      scenario$all.criteria <- all.criteria %>%
        filter(policy.name == !!policy.name) %>%
        mutate(policy.name = NULL)

      # Remove names from other's policies criteria
      all.names <- names(scenario$all.criteria)
      policy.criteria.names <- criteria_info(scenario$policy, scenario$trial$endpoints)$name
      all.relevant.names <- all.names[!startsWith(all.names, "criteria.") | (all.names %in% policy.criteria.names)]

      scenario$all.criteria <- scenario$all.criteria %>% select(all_of(all.relevant.names))
      scenario
    }
  )

  # scenario$policy <- policy
  # scenario$all.criteria <- all.criteria
  # scenario
}

#' Simulate a scenario with a single policy
#'
#' @param scenario A scenario produced by [with_true_effects()].
#' @param policy A single policy schema of type `"Policy"`.
#' @param ... Other arguments. Passed through to [simulate_with_policies()].
#'
#' A wrapper around [simulate_with_policies()] for the case where the
#' number of policies is 1.
#'
#' @returns A single element of the list returned by
#'  [simulate_with_policies()].
#'
#' @export
simulate_with_policy <- function(scenario, policy, ...) {
  simulate_with_policies(
    scenario = scenario,
    policies = list(a = policy),
    ...
  )$a
}

#' Apply specific params to a simulated scenario.
#'
#' @param scenario A simulated scenario (see [simulate_with_policies()]).
#'    The scenario must contain a data frame in `.$all.criteria`.
#'    The policy should be of type `"Policy"` (see [schema_type()]).
#' @param params Parameters used by the scenario policy.
#'
#' @returns A [Scenario()] with a decision dataframe (`.$all.decisions`),
#'   on which [aggregate_scenario()] may be called.
#'
#' @details
#' Params should be a list, with the names in
#' `params_info(scenario$policy, scenario$trial$endpoints)$name`, and the values
#' desired for those parameters.
#'
#' The `all.decisions` data frame contains all the columns in `.$all.criteria`,
#' plus the columns produce by applying [outcome_fun()] to the policy; that is:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | ... | ... | Columns in `.$all.criteria` (see [simulate_with_policies()]) |
#' | decision | \link{DECISION} | Decision produced by the policy for this true effect |
#' | decision.has.go | logical | Whether a "go" decision was produced |
#' | decision.has.stop | logical | Whether a "stop" decision was produced |
#'
#' Usually, if `decision.has.stop == TRUE` then the decision will be `Stop`, otherwise
#' it will be `Go` if `decision.has.go == TRUE`, otherwise `Discuss`.
#'
#' @export
with_params <- function(scenario,
                        params) {
  assertthat::assert_that(is(scenario, "Scenario"))
  assertthat::assert_that(is(params, "list"))
  assertthat::assert_that(all(params_info(scenario$policy, scenario$trial$endpoints)$name %in% names(params)))
  # TODO: Check type of policy
  outcome <- outcome_fun(
    scenario$policy,
    endpoints = scenario$trial$endpoints,
    params =  params,
    criteria = scenario$all.criteria
  )

  df <- scenario$all.criteria
  df <- bind_cols(df, outcome)

  scenario$params <- params

  scenario$all.decisions <- df
  scenario
}

#' Aggregate the decisions.
#'
#' @param scenario A scenario as returned by [with_params()].
#'
#' @returns The given `scenario` with two additional fields:
#' `.$decisions.per.effect` and `.$metrics`,
#' each of which is a `tibble`.
#'
#' @section Decisions per effect:
#'
#' The `.$decisions.per.effect` field in the result contains:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | correct.decision | \link{DECISION} | Desired decision for this true effect |
#' | region.name | character | Name of this region |
#' | effect.name | character | Name of this true effect |
#' | effect.for.`<name>` | numeric | True effect for endpoint `<name>` (as many columns as endpoints) |
#' | ... | ... | All the columns produced by [summarise_decision()] |
#'
#' @section Metrics:
#'
#' The `.$metrics` field in the result summarises the columns produced by [summarise_decision()]
#' (including `p.go`, `p.stop`, ...). The resulting tibble contains:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | correct.decision | \code{\link{DECISION}} | Desired decision for this region |
#' | region.name | character  | Name of this region |
#' | min.Gr | numeric | Smallest value of `p.go` for simulated true effects in this region |
#' | max.Gr | numeric | Largest value of `p.go` for simulated true effects in this region |
#' | min.Sr | numeric | Smallest value of `p.stop` for simulated true effects in this region |
#' | max.Sr | numeric | Largest value of `p.stop` for simulated true effects in this region |
#' | min.GR | numeric | Smallest value of `p.has.go` for simulated true effects in this region |
#' | max.GR | numeric | Largest value of `p.has.go` for simulated true effects in this region |
#' | min.SR | numeric | Smallest value of `p.has.stop` for simulated true effects in this region |
#' | max.SR | numeric | Largest value of `p.has.stop` for simulated true effects in this region |
#' | ci.error | numeric | Half the maximum width of the 95% confidence intervals containing the above columns |
#' | FSR | numeric | If the desired decision is "Go", the maximum probability of "stop" for true effects in the region |
#' | FGR | numeric | If the desired decision is "Stop", the maximum probability of "go" for true effects in the region, regardless of "stop" |
#' | FSr | numeric | If the desired decision is "Go", the maximum probability of "Stop" for true effects in the region |
#' | FGr | numeric | If the desired decision is "Stop", the maximum probability of "Go" (and not "Stop") for true effects in the region |
#' | CSr | numeric | If the desired decision is "Stop", the maximum probability of "Stop" for true effects in the region |
#' | CGr | numeric | If the desired decision is "Go", the maximum probability of "Go" (and not "Stop") for true effects in the region |
#'
#' @seealso [four_metrics()]
#'
#' @export
aggregate_scenario <- function(scenario) {
  assertthat::assert_that(!is.null(scenario$all.decisions),
    msg = "Have you called with_params() on the scenario"
  )

  df <- scenario$all.decisions
  df <- df %>%
    group_by(
      .data$correct.decision,
      .data$region.name,
      .data$effect.name,
      across(starts_with(EFFECT_VALUE_PREFIX))
    ) %>%
    summarise(summarise_decision(.data$decision,
      has.go = .data$decision.has.go,
      has.stop = .data$decision.has.stop
    )) %>%
    ungroup()

  scenario$decisions.per.effect <- df

  df2 <- df %>%
    group_by(
      .data$correct.decision,
      .data$region.name
    ) %>%
    summarise(
      min.Gr = min(.data$p.go),
      max.Gr = max(.data$p.go),
      min.Sr = min(.data$p.stop),
      max.Sr = max(.data$p.stop),
      min.GR = min(.data$p.has.go),
      max.GR = max(.data$p.has.go),
      min.SR = min(.data$p.has.stop),
      max.SR = max(.data$p.has.stop),
      ci.error = max(
        max(.data$error.p.go),
        max(.data$error.p.stop),
        max(.data$error.p.has.go),
        max(.data$error.p.has.stop)
      )
    ) %>%
    ungroup() %>%
    mutate(
      FSR = ifelse(
        .data$correct.decision == DECISION$Go,
        .data$max.SR,
        NA
      ),
      FGR = ifelse(
        .data$correct.decision == DECISION$Stop,
        .data$max.GR,
        NA
      ),
      FSr = ifelse(
        .data$correct.decision == DECISION$Go,
        .data$max.Sr,
        NA
      ),
      FGr = ifelse(
        .data$correct.decision == DECISION$Stop,
        .data$max.Gr,
        NA
      ),
      CSr = ifelse(
        .data$correct.decision == DECISION$Stop,
        .data$min.Sr,
        NA
      ),
      CGr = ifelse(
        .data$correct.decision == DECISION$Go,
        .data$min.Gr,
        NA
      )
    )

  scenario$metrics <- df2

  scenario
}

#' Compute the four metrics for a single pair of go and stop regions.
#'
#' @param scenario A simulated scenario with metrics
#'   (see [with_params()]).
#' @param go.region Name of the region for which Go is desired
#'   (among those included in `scenario`)
#' @param stop.region Name fo the region for which Stop is desired
#'   (among those included in `scenario`)
#'
#' @returns A data frame containing a single row, and the following columns.
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | go.region | character | Same as `go.region` parameter |
#' | stop.region | character | Same as `stop.region` parameter |
#' | params.`<param>` | numeric | Value of the policy parameter as given to [with_params()] |
#' | FSR | numeric | The maximum probability of "stop" for true effects in `go.region` |
#' | FSr | numeric | The maximum probability of "Stop" for true effects in `go.region` |
#' | CGr | numeric | The maximum probability of "Go" (and not "Stop") for true effects in `go.region` |
#' | Go.region.error | numeric | Maximum error at a 95% confidence level for the probabilities regarding the true effects in `go.region` |
#' | FGr | numeric | The maximum probability of "Go" (and not "Stop") for true effects in `stop.region` |
#' | FGR | numeric | The maximum probability of "go" regardless of "stop" for true effects in `stop.region` |
#' | CSr | numeric | The maximum probability of "Stop" for true effects in`stop.region` |
#' | Stop.region.error | numeric | Maximum error at a 95% confidence level for the probabilities regarding the true effects in `stop.region` |
#'
#' @details
#'
#' The bounds `Go.region.error` and `Stop.region.error` are set to the same upper bound.
#'
#' @seealso [four_metrics_all()]
#'
#' @export
four_metrics <- function(scenario,
                         go.region,
                         stop.region) {
  if (is.null(scenario$metrics)) {
    scenario <- scenario %>% aggregate_scenario()
  }
  bind_cols(
    tibble::tibble_row(go.region, stop.region),
    as_tibble(scenario$params) %>%
      rename_with(function(n) paste0("params.", n)),
    scenario$metrics %>%
      filter(.data$region.name == !!go.region &
        .data$correct.decision == DECISION$Go) %>%
      select(.data$FSR, .data$FSr, .data$CGr, Go.region.error = .data$ci.error),
    scenario$metrics %>%
      filter(.data$region.name == !!stop.region &
        .data$correct.decision == DECISION$Stop) %>%
      select(.data$FGR, .data$FGr, .data$CSr, Stop.region.error = .data$ci.error)
  ) %>%
    # Move error columns to the end
    select(!ends_with(".region.error"), ends_with(".region.error"))
}

#' Compute the four metrics for all regions
#'
#' @param scenario A simulated scenario with metrics
#'   (see [with_params()]).
#' @param go.regions A vector of region names for which a
#'   Go decision is desired, of those
#'   included when defining the scenario.
#' @param stop.regions A vector of region names for which a
#'   Stop decision is desired, of those
#'   included in the definition of the scenario.
#'
#' @returns A data frame with the following columns:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | params.`<param>` | numeric | Parameters given to [with_params()]
#' | `<go.region>`.FSR | numeric | The maximum probability of "stop" for a true effects in `<go.region>` |
#' | `<go.region>`.FSr | numeric | The maximum probability of "Stop" for true effects in `<go.region>` |
#' | `<go.region>`.CGr | numeric | The maximum probability of "Go" (and not "Stop") for true effects in `<go.region>` |
#' | `<go.region>`.error | numeric | Maximum error at a 95% confidence level for the probabilities regarding the true effects in `<go.region>` |
#' | `<stop.region>`.FGr | numeric | The maximum probability of "Go" (and not "Stop") for true effects in `<stop.region>` |
#' | `<stop.region>`.FGR | numeric | The maximum probability of "go" regardless of "stop" for true effects in `<stop.region>` |
#' | `<stop.region>`.CSr | numeric | The maximum probability of "Stop" for true effects in `<stop.region>` |
#' | `<stop.region>`.error | numeric | Maximum error at a 95% confidence level for the probabilities regarding the true effects in `<stop.region>` |
#'
#' `<param>` ranges over the parameters of the policy, <go.region> ranges over the elements `go.regions`,
#' `<stop.region>` ranges over the elements of `stop.regions`.
#'
#' @export
four_metrics_all <- function(scenario,
                             go.regions = NULL,
                             stop.regions = NULL) {
  if (is.null(scenario$metrics)) {
    scenario <- scenario %>% aggregate_scenario()
  }
  if (is.null(go.regions)) {
    go.regions <- names(scenario$go.regions)
  }
  if (is.null(stop.regions)) {
    stop.regions <- names(scenario$stop.regions)
  }
  res <- bind_cols(
    as_tibble(scenario$params) %>%
      rename_with(function(n) paste0("params.", n)),
    scenario$metrics %>%
      filter(.data$region.name %in% !!go.regions &
        .data$correct.decision == DECISION$Go) %>%
      select(.data$region.name, .data$FSR, .data$FSr, .data$CGr, error = .data$ci.error) %>%
      pivot_wider(names_from = "region.name", values_from = !"region.name", names_sep = "."),
    scenario$metrics %>%
      filter(.data$region.name %in% !!stop.regions &
        .data$correct.decision == DECISION$Stop) %>%
      select(.data$region.name, .data$FGR, .data$FGr, .data$CSr, error = .data$ci.error) %>%
      pivot_wider(names_from = "region.name", values_from = !"region.name", names_sep = ".")
  ) %>%
    # Move error columns to the end
    select(!ends_with(".error"), ends_with(".error"))
  assertthat::assert_that(nrow(res) == 1)
  res
}

#' Visualize region schemas
#'
#' @param regions A named list of region schemas (i.e. objects created by [make_region()]).
#' @param endpoints Information about `endpoints` (an [Endpoints()] object).
#' @param range.x Vector of equally-spaced points along the x axis for which to plot the regions
#' @param range.y Vector of equally-spaced points along the y axis for which to plot the regions
#'
#' @returns A `ggplot2` object.
#'
#' @export
plot_regions <- function(regions = NULL,
                         endpoints = NULL,
                         range.x = NULL,
                         range.y = NULL) {
  if (is.null(range.x)) {
    range.x <- seq(-0.5 * endpoints$vars$lrv[1], 1.5 * endpoints$vars$tv[1], length.out = 128)
  }
  if (is.null(range.y)) {
    range.y <- seq(-0.5 * endpoints$vars$lrv[2], 1.5 * endpoints$vars$tv[2], length.out = 128)
  }

  p1 <- ggplot()
  data <- NULL
  for (region.name in names(regions)) {
    data <- bind_rows(
      data,
      tibble::tibble_row() %>%
        inner_join(tibble(d1v1 = range.x), by = character()) %>%
        inner_join(tibble(d1v2 = range.y), by = character()) %>%
        mutate(
          in.region = point_in(
            regions[[region.name]],
            endpoints,
            rbind(.data$d1v1, .data$d1v2)
          )
        ) %>%
        mutate(name = region.name)
    )
  }

  #+
  # xlim(0, 1.5) +
  # ylim(0, 1.5)
  points.of.interest <-
    tibble::tribble(
      ~name, ~d1v1, ~d2v2,
      "TV", endpoints$vars$tv[1], endpoints$vars$tv[2],
      "LRV", endpoints$vars$lrv[1], endpoints$vars$lrv[2]
    )
  # print(points.of.interest)

  plot <- ggplot(data) +
    scale_x_continuous(limits = c(min(range.x), max(range.x)), expand = c(0, 0)) +
    scale_y_continuous(limits = c(min(range.y), max(range.y)), expand = c(0, 0)) +
    geom_tile(data = data, aes(
      x = .data$d1v1, y = .data$d1v2,
      alpha = .data$in.region, fill = .data$name
    )) +
    geom_point(
      data = points.of.interest,
      aes(shape = .data$name, x = .data$d1v1, y = .data$d2v2)
    ) +
    scale_alpha_manual(limits = c(TRUE, FALSE), values = c(0.5, 0)) +
    scale_shape_manual(values = unlist(response.shapes)) +
    labs(fill = "Region", shape = "Effect") +
    guides(alpha = "none") +
    (if (length(regions) == 2) {
      scale_fill_manual(
        labels = latex2exp::TeX,
        breaks = names(regions),
        values = c(
          rgb(163, 64, 221, maxColorValue = 255),
          rgb(255, 171, 30, maxColorValue = 255)
        )
      )
    } else {
      scale_fill_hue(labels = latex2exp::TeX)
    })
}

# Maximizes a monotonically increasing function in one dimension
# using binary search.
maximize_1d <- function(lower,
                        upper,
                        condition,
                        xtolabs = 1e-7) {
  assertthat::assert_that(condition(lower))
  if (condition(upper)) {
    return(upper)
  }
  while (abs(upper - lower) > xtolabs) {
    x0 <- (upper + lower) / 2
    if (condition(x0)) {
      lower <- x0
    } else {
      upper <- x0
    }
  }
  lower
}

#' Find optimal policy parameters to achieve a certain
#' FSR and FGR.
#'
#' @param scenario [Scenario()] as returned by [simulate_with_policy()]
#' @param go.region Name of the region for which to calculate the FSR.
#' @param stop.region Name of the region for which to calculate the FGR.
#' @param FSR.leq Desired FSR
#' @param FGR.leq Desired FGR
#'
#' @returns A list with three fields:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | params | list | Optimal parameters |
#' | metrics | tibble | (One row) Metrics for the optimal parameters (as returned by [four_metrics()]) |
#' | optimizer.info | list | Information on the optimization algorithm used |
#'
#' @details
#' The FGR and FSR are calculated according to [four_metrics()].
#' The function is supposed to have parameters `p.FG` and `p.FS` which
#' are directly coupled to the `FGR` and `FSR`.
#'
#' @export
optimize_params_FS_FR <- function(scenario,
                                  go.region,
                                  stop.region,
                                  FSR.leq = 0.1,
                                  FGR.leq = 0.2) {
  . <- NULL
  param.names <-
    scenario$policy %>%
    instantiate_at(scenario$trial$endpoints) %>%
    .$params.info %>%
    .$name
  assertthat::are_equal(sort(param.names), sort(c("p.FS", "p.FG")))

  the_metrics <- function(params) {
    scenario %>%
      with_params(params) %>%
      four_metrics(go.region = go.region, stop.region = stop.region)
  }
  opt <- list(p.FS = 0, p.FG = 0)
  # 1. Maximize CSr under FSr.leq
  opt$p.FS <- maximize_1d(
    lower = 0,
    upper = 1,
    condition = function(x) {
      params <- opt
      params$p.FS <- x
      the_metrics(params)$FSR <= FSR.leq
    }
  )
  opt$p.FG <- maximize_1d(
    lower = 0,
    upper = 1,
    condition = function(x) {
      params <- opt
      params$p.FG <- x
      the_metrics(params)$FGR <= FGR.leq
    }
  )
  list(
    params = opt,
    metrics = the_metrics(opt),
    optimizer.info = list(algo = "FS_FR", par = opt)
  )
}

#' Summarise decision probabilities
#'
#' @param decision A vector of values of type [DECISION].
#' @param has.go A vector of logical values, indicating whether
#'   a "go" decision was produced (independently of "stop")
#'   in that simulation.
#' @param has.stop A vector of logical values, indicating whether a
#'   "stop" decision was produced.
#'
#' @returns A one-row tibble, containing:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | n.studies.total | numeric | Total number of simulations run |
#' | n.studies.go | numeric | Number of simulations resulting in Go |
#' | n.studies.discuss | numeric | Number of simulations resulting in Discuss |
#' | n.studies.stop | numeric | Number of simulations resulting in Stop |
#' | p.go | numeric | Estimated probability of Go |
#' | p.stop | numeric | Estimated probability of Stop |
#' | p.discuss | numeric | Estimated probability of Discuss |
#' | error.p.go | numeric | Maximum error for `p.go` at the 95% confidence level |
#' | error.p.stop | numeric | Maximum error for `p.stop` at the 95% confidence level |
#' | error.p.discuss | numeric | Maximum error for `p.discuss` at the 95% confidence level |
#' | n.studies.has.go | numeric | Number of simulations resulting in "go" (independently of "stop") |
#' | p.has.go | numeric |  Estimated probability of "go" (independently of "stop") |
#' | error.p.has.go | Maximum error for `p.has.go` at the 95% confidence level |
#' | n.studies.has.stop | numeric | Number of simulations resulting in "stop" |
#' | p.has.stop | numeric |  Estimated probability of "stop" |
#' | error.p.has.stop | Maximum error for `p.has.stop` at the 95% confidence level |
#'
#' @details
#'
#' The input vectors contain one vector for each simulation. Thus,
#' they should all be the same length,
#' and `n.studies.total` coincides with the length of the input vectors.
#'
#' It holds that `n.studies.total == n.studies.go + n.studies.go + n.studies.discuss`,
#' and `1 == p.go + p.stop + p.discuss`.'
#'
#'
#' @export
summarise_decision <- function(decision,
                               has.go = NULL,
                               has.stop = NULL) {
  n.studies.total <- length(decision)
  n.studies.go <- sum(decision == DECISION$Go)
  n.studies.discuss <- sum(decision == DECISION$Discuss)
  n.studies.stop <- sum(decision == DECISION$Stop)

  # Sanity check
  x <- c(n.studies.go, n.studies.discuss, n.studies.stop)
  cis <- MultinomialCI::multinomialCI(x, alpha = 0.05)
  p <- x / sum(x)
  errors <- pmax(cis[, 2] - p, p - cis[, 1])

  p.go <- p[1]
  p.discuss <- p[2]
  p.stop <- p[3]

  error.p.go <- errors[1]
  error.p.discuss <- errors[2]
  error.p.stop <- errors[3]

  tibble.go <-
    if (!is.null(has.go)) {
      n.studies.has.go <- sum(has.go)
      assertthat::are_equal(length(has.go), n.studies.total)
      cis <- MultinomialCI::multinomialCI(c(
        n.studies.has.go,
        n.studies.total - n.studies.has.go
      ),
      alpha = 0.05
      )
      tibble::tibble_row(
        n.studies.has.go,
        p.has.go = n.studies.has.go / n.studies.total,
        error.p.has.go = max(cis[1, 2] - .data$p.has.go, .data$p.has.go - cis[1, 1])
      )
    }

  tibble.stop <-
    if (!is.null(has.stop)) {
      n.studies.has.stop <- sum(has.stop)
      assertthat::are_equal(length(has.stop), n.studies.total)
      cis <- MultinomialCI::multinomialCI(c(
        n.studies.has.stop,
        n.studies.total - n.studies.has.stop
      ),
      alpha = 0.05
      )
      tibble::tibble_row(
        n.studies.has.stop,
        p.has.stop = n.studies.has.stop / n.studies.total,
        error.p.has.stop = max(cis[1, 2] - .data$p.has.stop, .data$p.has.stop - cis[1, 1])
      )
    }

  error.p.overall <- max(
    error.p.go,
    error.p.stop,
    error.p.discuss,
    tibble.go$error.p.has.go,
    tibble.stop$error.p.has.stop
  )

  bind_cols(
    tibble::tibble_row(
      n.studies.total,
      n.studies.go,
      n.studies.discuss,
      n.studies.stop,
      p.go,
      p.discuss,
      p.stop,
      error.p.overall,
      error.p.go,
      error.p.discuss,
      error.p.stop
    ), tibble.go, tibble.stop
  )
}

#' Long form of data frame of decision probabilities
#'
#' @param data A data frame
#' @return A data frame containing:
#'
#'  - All columns in the original data frame that are not of the form
#'    `<base-name>.(go|decision|stop)`.
#'  - An additional `decision` column of `factor` type, with levels `Go`,`Discuss`,`Stop`
#'    (see [DECISION]).
#'  - For each family of columns of the form `<base-name>.(go|decision|stop)`,
#'    a single column named `<base-name>`.
#'  - For each row in the original data frame, up to three rows in the output
#'    data frame, each with one of the three decision values in the `decision`
#'    column, and depending on the `decision` column, one of the
#'    values `<base-name>.(go|decision|stop)` in the `<base-name>`
#'    column.
#'
#' @export
pivot_decisions_longer <- function(data) {
  data %>%
    tidyr::pivot_longer(
      cols = ends_with(c(".go", ".discuss", ".stop")),
      names_pattern = "(.*)\\.(go|discuss|stop)",
      names_to = c(".value", "decision")
    ) %>%
    mutate(decision = factor(stringr::str_to_title(.data$decision),
      levels = decision.levels
    ))
}

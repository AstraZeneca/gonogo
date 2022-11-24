#' @import rlang
#' @importFrom stats splinefun
NULL

####################
# Policies         #
####################

#' Produce a decision based on the "go" and "stop" predicates
#'
#' @param has.stop Whether the "stop" predicate of the policy held.
#' @param has.go Whether the "go" predicate of the policy held.
#' @returns A tibble with as many rows as the number
#'   of components in `has.stop` or `has.go`, and the following columns:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | decision | [DECISION] | Stop if `has.stop`; otherwise Go if `has.go`; else Discuss |
#' | has.go | logical | Coincides with `has.go`, appropriately recycled |
#' | has.stop | logical | Coincides with `has.stop`, appropriately recycled |
#'
#' @details
#' The length of `has.go` and `has.stop` should be the same
#' (or at least recyclable to one another).
#'
#' The type of this function coincides with the expected type of
#' a decision (see [outcome_fun()] ).
#'
#' @export
decision_outcome <- function(has.stop, has.go) {
  tibble::tibble(
    decision =
      dplyr::if_else(
        has.stop, DECISION$Stop,
        dplyr::if_else(has.go, DECISION$Go,
          DECISION$Discuss
        )
      ),
    decision.has.go = has.go,
    decision.has.stop = has.stop
  )
}

#' Predicate policy
#'
#' @param stop.if A [Schema()] of type `Predicate`
#' @param go.if A [Schema()] of type `Predicate`.
#' @returns A schema of type `Decision`.
#'
#' @seealso The outcomes of `stop.if` and `go.if` are combined
#'   according to [decision_outcome()].
#'
#' @export
p_policy <- function(stop.if,
                     go.if) {
  make_schema_alias("p_policy", stop.if, go.if)
}

#' @describeIn p_policy Desugared form of `p_policy`.
#' @inheritParams dealias
#' @export
dealias.p_policy <- function(x, ...) {
  map_schema(sequence_schemas(
    list(
      has.stop = x$stop.if %>% with_criteria_prefix("stop.if."),
      has.go   = x$go.if %>% with_criteria_prefix("go.if.")
    ),
    homogeneous = FALSE
  ),
  FUN = function(outcomes) {
    decision_outcome(
      has.stop = outcomes$has.stop,
      has.go = outcomes$has.go
    )
  },
  type.in = "(Predicate,Predicate)",
  type.out = "Policy"
  )
}

#' Extract predicates from a policy
#'
#' @param x A policy [Schema()] of type `Decision`.
#' @returns A policy schema of the form `p_policy(stop.if, go.if)`
#'   (see [p_policy()]).
#'
#' @export
as_p_policy <- function(x) UseMethod("as_p_policy")

#' @describeIn as_p_policy If the policy is already of the form `p_policy`,
#'   it is left unchanged.
#' @export
as_p_policy.p_policy <- function(x) x

#' @describeIn as_p_policy Variable subsetting ([at_selected_vars()])
#'   can be "pushed-down" into the policy predicates.
#' @export
as_p_policy.at_selected_vars <- function(x) {
  schema0 <- as_p_policy(x$schema)
  p_policy(
    stop.if = schema0$stop.if %>% at_selected_vars(x$vars.selector),
    go.if = schema0$go.if %>% at_selected_vars(x$vars.selector)
  )
}

#' @describeIn as_p_policy Syntactic sugar is removed if needed.
#' @export
as_p_policy.Alias <- function(x) as_p_policy(dealias(x))

#' @describeIn as_p_policy By default, the predicates are obtained directly
#' from the outcome of the policy; although this may result in some
#' duplicated work.
#' @export
as_p_policy.Schema <- function(x) {
  p_policy(
    stop.if =
      map_schema(
        x,
        FUN = function(outcome) outcome$decision.has.stop,
        type.in = "Policy",
        type.out = "Predicate"
      ),
    go.if =
      map_schema(
        x,
        FUN = function(outcome) outcome$decision.has.go,
        type.in = "Policy",
        type.out = "Predicate"
      )
  )
}

#' Extract the "go" predicate of a policy.
#'
#' @param schema A policy-[Schema()] of type "Policy"
#' @returns A [Schema()] of type "Predicate", corresponding to the "go" predicate
#'   of the policy.
#'
#' @details The predicate is extracted using [as_p_policy()]
#'
#' @export
has_go <- function(schema) {
  make_schema_alias("has_go", schema)
}

#' @describeIn has_go Implementation as an alias.
#' @inheritParams dealias
#' @export
dealias.has_go <- function(x, ...) {
  as_p_policy(x$schema)$go.if
}

#' Extract the "stop" predicate of a policy.
#'
#' @param schema A policy-[Schema()] of type "Policy"
#' @returns A [Schema()] of type "Predicate", corresponding to the "stop" predicate
#'   of the policy.
#'
#' @details
#' The predicate is extracted using [as_p_policy()]
#'
#' @export
has_stop <- function(schema) {
  make_schema_alias("has_stop", schema)
}


#' @describeIn has_stop Implementation as an alias.
#' @inheritParams dealias
#' @export
dealias.has_stop <- function(x, ...) {
  as_p_policy(x$schema)$stop.if
}

#### Extract PValues from predicates

# We define an extractor for "predicate_for_level"

#' Getting the significance-level for a level-predicate
#'
#' Extracts the significance level parameter when interpreting
#' a predicate as a hypothesis test.
#'
#' @param x A [Schema()] of type "Predicate"
#' @param ... Other parameters needed (typically `endpoints = y`, where `y` is of [Endpoints()] class).
#' @returns The name of the parameter (one of the names in [params_info()]
#'
#' @export
p_value_param_name <- function(x, ...) UseMethod("p_value_param_name")

#' @describeIn p_value_param_name Syntactic sugar is removed if needed
#' @export
p_value_param_name.Alias <- function(x, ...) {
  p_value_param_name(dealias(x, ...), ...)
}

#' @describeIn p_value_param_name Parameter name (`param.name`) as specified
#'   in [make_predicate_for_level()]
#' @export
p_value_param_name.predicate_for_level <- function(x, ...) {
  x$param.name
}

#' @describeIn p_value_param_name Parameter name ofthe underlying schema.
#' @export
p_value_param_name.at_each_endpoint <- function(x, ...) {
  p_value_param_name(x$schema)
}

#' @describeIn p_value_param_name Parameter name of the underlying schema.
#' @export
p_value_param_name.at_each_domain <- function(x, ...) {
  p_value_param_name(x$schema)
}

#' @describeIn p_value_param_name Parameter name of the underlying schema.
#' @export
p_value_param_name.with_criteria_prefix <- function(x, ...) {
  p_value_param_name(x$schema)
}

### p_value_criterion_name

#' Getting the p-value for a level-predicate
#'
#' Extracts the name of the criterion corresponding to the p-value when interpreting
#' a predicate as a hypothesis test.
#'
#' @param x A [Schema()] of type "Predicate"
#' @param ... Other parameters needed (typically `endpoints = y`, where `y` is of [Endpoints()] class).
#' @returns The name of the parameter (one of the names in [criteria_info()]
#'
#' @export
p_value_criterion_name <- function(x, ...) {
  UseMethod("p_value_criterion_name")
}

#' @describeIn p_value_criterion_name The syntactic sugar is removed.
#' @export
p_value_criterion_name.Alias <- function(x, ...) {
  p_value_criterion_name(dealias(x, ...))
}

#' @describeIn p_value_criterion_name p-value criterion (`criterion.name`)
#'   as specified in [make_predicate_for_level()]
#' @export
p_value_criterion_name.predicate_for_level <- function(x, ...) {
  x$criterion.name
}

#' @describeIn p_value_criterion_name The prefix is prepended to the
#'   underlying p-value criterion name.
#' @export
p_value_criterion_name.with_criteria_prefix <- function(x, ...) {
  paste0(x$prefix, p_value_criterion_name(x$schema, ...))
}

#' @describeIn p_value_criterion_name Criterion of the underlying schema
#' @export
p_value_criterion_name.at_selected_vars <- function(x, ...) {
  p_value_criterion_name(x$schema)
}


#' Extract the p-value value for a predicate interpreted as a hypothesis test
#'
#' @param x Schema of type "Predicate"
#' @param criteria Dataframe/tibble produced by [criteria_fun()]
#' @param ... Other arguments (passed through)
#' @export
get_predicate_p_value <- function(x, criteria, ...) {
  criteria[, p_value_criterion_name(x, ...)]
}

## as_schema_list

#' Interpret predicate as a list of Schemas
#'
#' @param x Schema of type "\[A\]" for some A (e.g. "\[Policy\]" or "\[Predicate\]").
#' @param ... Other parameters needed (typically `endpoints = y`, where `y` is of [Endpoints()] class).
#' @returns A list of schemas of type A.
#'
#' @export
as_schema_list <- function(x, ...) UseMethod("as_schema_list")

#' @describeIn as_schema_list Remove syntactic sugar
#' @export
as_schema_list.Alias <- function(x, ...) {
  as_schema_list(dealias(x, ...), ...)
}

#' @describeIn as_schema_list "Push down" the prefix into the elements of the list.
#' @export
as_schema_list.with_criteria_prefix <- function(x, ...) {
  lapply(
    as_schema_list(x$schema, ...),
    function(y) with_criteria_prefix(y, prefix = x$prefix)
  )
}

#' @describeIn as_schema_list Underlying list of schemas.
#' @export
as_schema_list.sequence_schemas <- function(x, ...) {
  x$schemas
}

## length_of_schema_list

#' Returns the length of the underlying list of schemas
#'
#' @param x Schema of type "\[A\]" for some A (e.g. "\[Policy\]" or "\[Predicate\]").
#' @param ... Other parameters needed (typically `endpoints = y`, where `y` is of [Endpoints()] class).
#' @returns An integer, indicating the length of the list.
#'
#' @details
#' The default implementation uses [as_schema_list()], and then
#' calculates the length.
#'
#'
#' @export
length_of_schema_list <- function(x, ...) UseMethod("length_of_schema_list")

#' @export
length_of_schema_list.default <- function(x, ...) {
  l <- as_schema_list(x, ...)
  assertthat::assert_that(is(l, "list"))
  length(l)
}

####

#' At least 1 with correction for multiple testing
#'
#' It is suggested to use [at_least_1_corrected()] instead.
#' However, this function (when it works) can infer the name
#' of the level parameter automatically.
#'
#' @param  list.schema Must be a list of predicate_for_level
#'   predicates, or equivalent.
#' @param  level.param  Name of the parameter containing
#'   the test (in case it cannot be deduced from list.schema)
#' @param correction
#'  | Name | Description |
#'  | ---  | ----------- |
#'  | min  | No correction |
#'  | bonferroni |  All p_values are multiplied by "m" |
#'  | simes | The smallest p_value is multiplied by m/1, the second smallest by m/2, etc. |
#'
#' @returns A [Schema()] of type "Predicate".
#'
#' @details
#' Simes, R. J. (1986).
#' An improved Bonferroni procedure for multiple tests of significance. Biometrika, 73(3):751-754.
#'
#' @seealso
#' This schema inherits from [make_predicate_for_level()].
#'
#' @export
at_least_1_adjusted <- function(list.schema,
                                level.param = NULL,
                                correction = c("bonferroni", "simes", "none")) {
  if (is.null(level.param)) {
    level.param <- p_value_param_name(list.schema)
  }
  pvs <- list.schema
  correction <- match.arg(correction)
  make_predicate_for_level(
    "at_least_1_adjusted",
    criterion.name = paste0("p.corrected.", correction),
    param.name = level.param,
    list.schema = pvs %>% with_criteria_prefix("at_least_1."),
    correction
  )
}

#' @describeIn at_least_1_adjusted All the names of criteria of `x`, plus a new criterion
#'   named `p.corrected.<x$correction>`
#' @inheritParams criteria_info
#' @param ... Remaining arguments; passed through.
#' @export
criteria_info.at_least_1_adjusted <- function(x, ...) {
  bind_rows(
    tibble_row(name = x$criterion.name),
    criteria_info(x$list.schema, ...)
  )
}

#' @describeIn at_least_1_adjusted Calculated criteria, including `p.corrected.<x$correction>`.
#' @inheritParams criteria_fun
#' @export
criteria_fun.at_least_1_adjusted <- function(x, endpoints, summaries) {
  criteria0 <- criteria_fun(x$list.schema, endpoints, summaries)
  predicates <- as_schema_list(x$list.schema, endpoints = endpoints)
  ii <- sapply(predicates, p_value_criterion_name)
  if (length(ii) == 0) {
    ii <- NULL
  }
  pvalues0 <- as.matrix(criteria0[, ii, drop = FALSE])

  k <- length(predicates)
  pvalues.all <-
    if (x$correction == "none") {
      matrixStats::rowMins(pvalues0)
    } else if (x$correction == "bonferroni") {
      adjusted <- pvalues0 * k
      matrixStats::rowMins(adjusted)
    } else if (x$correction == "simes") {
      if (k > 1) {
        sorted <- t(apply(pvalues0, MARGIN = 1, sort))
      } else {
        sorted <- pvalues0
      }
      assertthat::assert_that(nrow(sorted) == nrow(pvalues0))
      assertthat::assert_that(ncol(sorted) == k)
      adjusted <- k * sorted / matrix(seq(from = 1, length.out = k), nrow = nrow(sorted), ncol = k, byrow = T)
      matrixStats::rowMins(adjusted)
    }
  bind_cols(
    tibble(!!x$criterion.name := pvalues.all),
    # We keep it for debugging purposes
    criteria0
  )
}

# at_least_1_corrected


#' At least 1 with correction for multiple testing
#'
#' @param  list.schema A [Schema()] of type "\[Predicate\]"
#' @param  level Name of the parameter representing the significance level
#'    that needs to be adjusted (one of those in `param_info(list.schema, ...)`).
#' @param correction
#'  Let m be the underlying length of list.schema
#'
#'  | Name | Description |
#'  |------|-------------|
#'  | none  | No correction |
#'  | bonferroni |  Bonferroni correction |
#'  | simes | Simes/Benjamini-Hochberg correction |
#'
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#'
#' Let `m` be the underlying length of `list.schema`,
#' and assume that, when calling [outcome_fun.at_least_1_corrected()],
#' the parameter of name `<level>` is set to `alpha`.
#'
#' - In the absence of correction, the outcome is `TRUE` if and only if the
#'   outcome of any of the underlying schemas is `TRUE` when the
#'   parameter of name `<level>` is set to `alpha`.
#' - With the Bonferroni correction,
#'   then the outcome is `TRUE` if and only if, in any of the underlying schemas,
#'   the outcome is `TRUE` when the
#'   parameter of name `<level>`  is set to `alpha/m`.
#' - With the Simes correction,
#'   then the outcome is `TRUE` if and only if:
#'   - In any of the underlying schemas,
#'     the outcome is `TRUE` when the
#'     parameter of name `<level>`  is set to `alpha/m`.
#'   - OR, in *two* of the underlying schemas,
#'     the outcome is `TRUE` when the
#'     parameter of name `<level>`  is set to `2*alpha/m`.
#'   - OR, in *three* of the underlying schemas,
#'     the outcome is `TRUE` when the
#'     parameter of name `<level>`  is set to `3*alpha/m`.
#'   - ...
#'   - OR, in *all* of the underlying schemas,
#'     the outcome is `TRUE` when the parameter of name `<level>`
#'     is set to `alpha`.#'
#'
#' Reference: Simes, R. J. (1986).
#' An improved Bonferroni procedure for multiple tests of significance. Biometrika, 73(3):751-754.
#'
#' NB. The Simes correction assumes positive correlation between endpoints.
#'
#' @export
at_least_1_corrected <- function(list.schema,
                                 level,
                                 correction = c("bonferroni", "simes", "none")) {
  assertthat::assert_that(schema_type(list.schema) == "[Predicate]")
  correction <- match.arg(correction)
  make_schema(
    "at_least_1_corrected",
    level,
    list.schema,
    correction
  )
}

#' @describeIn at_least_1_corrected "Predicate"
#' @inheritParams schema_type
#' @export
schema_type.at_least_1_corrected <- function(x) {
  "Predicate"
}

#' @describeIn at_least_1_corrected The original parameters, including `x$level`
#' @inheritParams params_info
#' @param ... Remaining arguments; passed through.
#' @export
params_info.at_least_1_corrected <- function(x, ...) {
  info <- params_info(x$list.schema, ...)
  assertthat::assert_that(x$level %in% info$name)
  info
}

#' @describeIn at_least_1_corrected The underlying criteria names.
#' @inheritParams criteria_info
#' @param ... Remaining arguments; passed through.
#' @export
criteria_info.at_least_1_corrected <- function(x, ...) {
  criteria_info(x$list.schema, ...)
}

#' @describeIn at_least_1_corrected The underlying criteria values.
#' @inheritParams criteria_fun
#' @param ... Remaining arguments; passed through.
#' @export
criteria_fun.at_least_1_corrected <- function(x, ...) {
  criteria_fun(x$list.schema, ...)
}

#' @describeIn at_least_1_corrected The outcome, according to the multiplicity correction.
#' @inheritParams outcome_fun
#' @export
outcome_fun.at_least_1_corrected <- function(x, endpoints, params, criteria) {
  assertthat::assert_that(x$level %in% names(params))

  if (x$correction == "none") {
    outcome <- outcome_fun(x$list.schema, endpoints, params, criteria)
    return(Reduce("|", outcome, init = FALSE))
  }

  M <- length_of_schema_list(x$list.schema, endpoints)
  if (x$correction == "bonferroni") {
    params.M <- params
    params.M[[x$level]] <- params[[x$level]] / M
    outcome <- outcome_fun(x$list.schema, endpoints, params = params.M, criteria)
    return(Reduce("|", outcome, init = FALSE))
  }

  assertthat::assert_that(x$correction == "simes")
  acc <- FALSE
  for (m in seq(1, length.out = M)) {
    params.m <- params
    params.m[[x$level]] <- params[[x$level]] * m / M
    outcome.m <- outcome_fun(x$list.schema, endpoints, params = params.m, criteria)
    acc <- acc | (Reduce("+", outcome.m, init = 0) >= m)
  }
  acc
}

#########

#' Predicate based on hypothesis testing
#'
#' Helper to quickly define predicates that can be formulated
#' as testing a p-value (criterion) against a significance level (parameter)
#'
#' @param type Subclass constructor name.
#' @param param.name Name of the parameter containing the significance level.
#' @param criterion.name Name of the criterion containing the p-value.
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' This is intended as an abstract class; at least a [criteria_fun()]
#' implementation needs to be defined that produces a criterion of
#' name `criterion.name`.
#'
#' @export
make_predicate_for_level <- function(type, param.name, criterion.name = "p", ...) {
  make_schema(
    c(type, "predicate_for_level"),
    param.name,
    criterion.name,
    ...
  )
}

#' @describeIn make_predicate_for_level The type is "Predicate"
#' @inheritParams schema_type
#' @export
schema_type.predicate_for_level <- function(...) "Predicate"

#' @describeIn make_predicate_for_level The only criterion is one of name `criterion.name`.
#' @inheritParams criteria_info
#' @param ... Other parameters (ignored)
#' @export
criteria_info.predicate_for_level <- function(x, ...) {
  tibble::tibble(name = x$criterion.name)
}

#' @describeIn make_predicate_for_level The only parameter is one of name `param.name`.
#' @inheritParams params_info
#' @param ... Other parameters (ignored)
#' @export
params_info.predicate_for_level <- function(x, ...) {
  tibble::tibble(name = x$param.name)
}

#' @describeIn make_predicate_for_level The outcome is `TRUE` if and only if the value of the
#'   criterion is less than or equal the value of the parameter.
#' @inheritParams outcome_fun
#' @export
outcome_fun.predicate_for_level <- function(x, endpoints, params, criteria) {
  assertthat::has_name(criteria, x$criterion.name)
  assertthat::has_name(params, x$param.name)
  criteria[, x$criterion.name] <= params[[x$param.name]]
}

#' #' A policy schema following the 'with_p_values' pattern
#' #' @export
#' make_schema_with_p_values <- function(type, ...) {
#'   make_schema(c(type, "policy_with_p_values"), ...)
#' }
#'
#' #' @export
#' schema_type.policy_with_p_values <- function(...) "Policy"
#' #' @export
#' criteria_info.policy_with_p_values <- function(...) {
#'   tibble::tibble(name = c("stop.p", "go.p"))
#' }
#' #' @export
#' params_info.policy_with_p_values <- function(...) tibble::tibble(name = c("p.FS", "p.FG"))
#' #' @export
#' outcome_fun.policy_with_p_values <- function(x, endpoints, params, criteria) {
#'   dplyr::if_else(
#'     criteria$stop.p <= params$p.FS,
#'     DECISION$Stop,
#'     dplyr::if_else(
#'       criteria$go.p <= params$p.FG,
#'       DECISION$Go,
#'       DECISION$Discuss
#'     )
#'   )
#' }

#######################
#  Univariate policy  #
#######################

#' Basic univariate policy
#'
#' @details The policy assumes that there is only a single endpoint.
#' @export
p_lalonde <- make_schema_alias("p_lalonde")

#' @describeIn p_lalonde This policy is an alias for [p_univariate].
#' @inheritParams dealias
#' @export
dealias.p_lalonde <- function(...) {
  p_univariate
}

#' Basic univariate policy
#'
#' @details The policy assumes that there is only a single endpoint.
#' @export
p_univariate <- make_schema_alias("p_univariate")

#' @describeIn p_univariate This policy defined in terms of [p_univariate_has_stop]
#'   and [p_univariate_has_go].
#' @inheritParams dealias
#' @export
dealias.p_univariate <- function(...) {
  p_policy(
    stop.if = p_univariate_has_stop,
    go.if = p_univariate_has_go
  )
}

#' "stop" predicate for the univariate policy
#'
#' @seealso Defined in terms of [make_predicate_for_level()].
#'
#' @details The predicate assumes that there is only a single endpoint.
#' @inheritParams dealias
#' @export
p_univariate_has_stop <- make_predicate_for_level(
  "p_univariate_has_stop",
  param.name = "p.FS",
  criterion.name = "stop.p"
)

#' @describeIn p_univariate_has_stop The criterion is a p_value indicating
#'   whether the observed effect is significantly smaller than the target value.
#' @inheritParams criteria_fun
#' @export
criteria_fun.p_univariate_has_stop <- function(x, endpoints, summaries) {
  assertthat::assert_that(n_vars(endpoints) == 1)
  summaries <- make_summaries(summaries)
  dd <- summaries_at_endpoint_vectorized(summaries, endpoints$vars$name)
  stop.p <- with(
    dd,
    pt((delta.hat - tv) / delta.se,
      df = degrees.of.freedom,
      lower.tail = TRUE
    )
  )
  tibble::tibble(stop.p = stop.p)
}

#' "go" predicate for the univariate policy
#'
#' @seealso Defined in terms of [make_predicate_for_level()].
#'
#' @details The predicate assumes that there is only a single endpoint.
#'
#' @export
p_univariate_has_go <- make_predicate_for_level(
  "p_univariate_has_go",
  param.name = "p.FG",
  criterion.name = "go.p"
)

#' @describeIn p_univariate_has_go The criterion is a p_value indicating
#'   whether the observed effect is significantly larger than the lower-reference value.
#'
#' @inheritParams criteria_fun
#' @export
criteria_fun.p_univariate_has_go <- function(x, endpoints, summaries) {
  assertthat::assert_that(n_vars(endpoints) == 1)
  summaries <- make_summaries(summaries)
  dd <- summaries_at_endpoint_vectorized(summaries, endpoints$vars$name)
  go.p <- with(dd, pt((delta.hat - lrv) / delta.se,
    df = degrees.of.freedom,
    lower.tail = FALSE
  ))
  tibble::tibble(go.p = go.p)
}

#### Hotelling T²-based policy

#' Hotelling T²-based policy
#'
#' @param false.stop A region schema (see [make_region()]) indicating the true
#'   effects for which a "stop" decision is not desired.
#' @param false.go A region schema (see [make_region()]) indicating the true
#'   effects for which a "go" decision is not desired.
#'
#' @returns A [Schema()] of type "Policy"
#' @export
p_hotelling <- function(false.stop = r_whole, false.go = r_whole) {
  make_schema_alias("p_hotelling", false.stop, false.go)
}

#' @describeIn p_hotelling Defined in terms of [p_policy()],
#'   with [p_hotelling_predicate()] for the "stop" and "go" predicates.
#' @inheritParams dealias
#' @export
dealias.p_hotelling <- function(x, ...) {
  p_policy(
    stop.if = p_hotelling_predicate(
      region = x$false.stop,
      param.name = "p.FS"
    ),
    go.if = p_hotelling_predicate(
      region = x$false.go,
      param.name = "p.FG"
    )
  )
}

#' Predicate based on Hotelling's T² statistic
#'
#' @param param.name Name of the significance level parameter
#' @param region region Name of the region for which the predicate should
#'   be `FALSE`
#'
#' @details
#' Defined as a level predicate (see [make_predicate_for_level()])
#' with criterion name `Qinv.T2.half`, and significance level parameter
#' as `param.name`.
#'
#' @export
p_hotelling_predicate <- function(param.name, region) {
  make_predicate_for_level(
    "p_hotelling_predicate",
    param.name = param.name,
    criterion.name = "Qinv.T2.half",
    region = region
  )
}

a00_criteria_fun_p_hotelling_predicate <-
  function(x, endpoints, summary) {
    if (point_in(
      x$region,
      endpoints,
      summary$vars$delta.hat
    )) {
      tibble::tibble_row(
        Qinv.T2.half = 1
      )
    } else {
      min_T2_fun <- min_T2(summary, endpoints)
      T2 <- min_T2_fun(x$region)
      tibble::tibble_row(
        Qinv.T2.half = p_value_T2(T2, summary)
      )
    }
  }

#' @describeIn p_hotelling_predicate p-value (under the name `Qinv.T2.half`
#'   corresponding to
#'   the minimum of the distance induced by T² between the
#'   `region` and the observed effect. The p-value is 1 if the
#'   observed effect is in the region, and tends to 0 as the observed
#'   effect strays away from the region.
#'
#' @inheritParams criteria_fun
#'
#' @export
criteria_fun.p_hotelling_predicate <-
  vectorize_criteria_fun(a00_criteria_fun_p_hotelling_predicate)



# outcome_fun.p_hotelling <- function(x, endpoints, params, criteria) {
#   dplyr::if_else(
#     criteria$stop.p <= 2 * params$p.FS,
#     DECISION$Stop,
#     dplyr::if_else(
#       criteria$go.p <= 2 * params$p.FG,
#       DECISION$Go,
#       DECISION$Discuss
#     )
#   )
# }

# Helper functions for implementing [criteria_fun.p_hotelling_predicate()]
# This calculates the distance to the region by using a solver for
# quadratic programming problems.
min_T2 <- function(summary, endpoints) {
  D <- solve(summary$sigma.delta.hat)
  # This is faster than solve() and we need to compute D anyway.
  mu.hat <- summary$vars$delta.hat
  d <- D %*% mu.hat
  C <- t(mu.hat) %*% D %*% mu.hat

  function(region) {
    mats <- constraint_matrices(region, endpoints)
    min(sapply(mats, function(mat) {
      assertthat::assert_that(mat$op == ">=")
      res <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = t(mat$A.t), bvec = mat$b0)
      # print(res$solution)
      2 * res$value + C
    }))
  }
}

#' Returns a function that computes the minimum distance from
#' \eqn{\hat{μ}} to a given region, using the metric induced
#' by \eqn{\hat{Σ}}.
#'
#' The function returns both the distance and the
#' "orthogonal projection" of \eqn{\hat{μ}} on the region.
#'
#' @param summary [Summary()] statistics from the study.
#' @param endpoints [Endpoints()] information.
min_T2_with_solution <- function(summary, endpoints) {
  D <- solve(summary$sigma.delta.hat)
  # This is faster than solve() and we need to compute D anyway.
  mu.hat <- summary$vars$delta.hat
  d <- D %*% mu.hat
  C <- t(mu.hat) %*% D %*% mu.hat

  function(region) {
    mats <- constraint_matrices(region, endpoints)
    results <- lapply(mats, function(mat) {
      assertthat::assert_that(mat$op == ">=")
      res <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = t(mat$A.t), bvec = mat$b0)
      # print(res$solution)
      list(
        solution = res$solution,
        value = 2 * res$value + C
      )
    })
    values <- sapply(results, function(result) result$value)
    values.min <- min(values)
    results[[which(values == values.min)[1]]]
  }
}

p_value_T2 <- function(t2, summary) {
  V <- n_vars(summary)
  if (summary$covariance.structure == "theoretical") {
    pchisq(t2, V, lower.tail = FALSE)
  } else if (summary$covariance.structure == "unstructured") {
    NN <- summary$n.patients$total
    pf(t2 * (NN - V - 1) / (V * (NN - 2)), df1 = V, df2 = NN - V - 1, lower.tail = FALSE)
  } else {
    assertthat::assert_that(
      FALSE,
      paste0("Unsupported covariance structure ", summary$covariance.structure)
    )
  }
}

# Likelihood-based policy #
###########################

# This assumes that the regions have the same dimension
# as the whole space, and thus, the distribution of the distance
# to the orthogonal projection must be approximately chisq
#
# See Perlman  (1969)
# "One-sided testing problems in multivariate analysis."
# The Annals of Mathematical Statistics

#' Likelihood based policy
#'
#' @param false.stop A region schema (see [make_region()]) indicating the true
#'   effects for which a "stop" decision is not desired.
#' @param false.go A region schema (see [make_region()]) indicating the true
#'   effects for which a "go" decision is not desired.
#'
#' @returns A [Schema()] of type "Policy"
#'
#' @details
#' Very similar to [p_hotelling], but assuming that the
#' associated \eqn{T^2} statistic has only 1 degree of freedom.
#'
#' @export
p_likelihood <- function(false.stop = r_whole, false.go = r_whole) {
  make_schema_alias("p_likelihood", false.stop, false.go)
}

#' @describeIn p_likelihood Defined in terms of [p_policy()],
#'   with [p_likelihood_predicate()] for the "stop" and "go" predicates.
#' @inheritParams dealias
#' @export
dealias.p_likelihood <- function(x, ...) {
  p_policy(
    stop.if = p_likelihood_predicate(
      region = x$false.stop,
      param.name = "p.FS"
    ),
    go.if = p_likelihood_predicate(
      region = x$false.go,
      param.name = "p.FG"
    )
  )
}

#' Likelihood function based predicate
#'
#' Predicate based on Hotelling's T² statistic, assuming
#' the resulting statistic is chi-square distributed
#' with 1 degree of freedom.
#'
#' @param param.name Name of the significance level parameter
#' @param region region Name of the region for which the predicate should
#'   be `FALSE`
#' @returns A [Schema()] of type predicate.
#'
#' @details
#' Defined as a level predicate (see [make_predicate_for_level()])
#' with criterion name `Qinv.T2.half`, and significance level parameter
#' with name `param.name`.
#'
#' @export
p_likelihood_predicate <- function(param.name, region) {
  make_predicate_for_level(
    "p_likelihood_predicate",
    param.name = param.name,
    criterion.name = "chisq.inv.T2.half",
    region = region
  )
}

#' @describeIn p_likelihood_predicate A single criterion of name
#'   `"chisq.inv.T2.half"`, corresponding to the p-value
#'   of the minimum distance from the observed true effect
#'   to the `region`. The p-value is 1 for observed effects in the region,
#'   and becomes smaller as the observed effect is further away from the region.
#' @inheritParams criteria_fun
#' @export
criteria_fun.p_likelihood_predicate <- vectorize_criteria_fun(
  function(x, endpoints, summary) {
    if (point_in(
      x$region,
      endpoints,
      summary$vars$delta.hat
    )) {
      tibble::tibble_row(
        chisq.inv.T2.half = 1
      )
    } else {
      min_T2_fun <- min_T2(summary, endpoints)
      T2 <- min_T2_fun(x$region)
      tibble::tibble_row(
        chisq.inv.T2.half = p_value_chisq(T2, summary) / 2
      )
    }
  }
)

p_value_chisq <- function(t2, summary) {
  if (summary$covariance.structure == "theoretical") {
    pchisq(t2, df = 1, lower.tail = FALSE)
  } else if (summary$covariance.structure == "unstructured") {
    # (2N - 2 - 1 + 1)/(1*(2N-2)) = 1
    # (2N - 1 - 1) = 1
    pf(t2 / 1, df1 = 1, df2 = summary$degrees.of.freedom, lower.tail = FALSE)
  } else {
    assertthat::assert_that(
      FALSE,
      paste0("Unsupported covariance structure ", summary$covariance.structure)
    )
  }
}


#' Adjusted Hotelling T2-based policy
#'
#' @param false.stop A region schema (see [make_region()]) indicating the true
#'   effects for which a "stop" decision is not desired.
#' @param false.go A region schema (see [make_region()]) indicating the true
#'   effects for which a "go" decision is not desired.
#' @param df.go Degrees of freedom assumed for the Go region.
#' @param df.stop Degrees of freedom assumed for the Stop region.
#'
#' @returns A [Schema()] of type "Policy"
#'
#' @details
#' Very similar to [p_hotelling], but adjusting the degrees
#' of freedom according to a heuristic.
#'
#' @export
p_likelihood_adj <- function(false.stop = r_whole,
                             false.go = r_whole,
                             df.go = 1,
                             df.stop = 1) {
  make_schema_alias("p_likelihood_adj", false.stop, false.go, df.go, df.stop)
}

#' @describeIn p_likelihood_adj Defined in terms of [p_policy()],
#'   with [p_likelihood_adj_predicate()] for the "stop" and "go" predicates.
#' @inheritParams dealias
#' @export
dealias.p_likelihood_adj <- function(x, ...) {
  p_policy(
    stop.if = p_likelihood_adj_predicate(
      region = x$false.stop,
      param.name = "p.FS",
      df = x$df.stop
    ),
    go.if = p_likelihood_adj_predicate(
      region = x$false.go,
      param.name = "p.FG",
      df = x$df.go
    )
  )
}

parse_df <- function(endpoints, df_expr) {
  assertthat::assert_that(is(endpoints, "Endpoints"))
  assertthat::assert_that(length(df_expr) == 1)
  if (is.character(df_expr)) {
    with(list(max = n_vars(endpoints)), eval(parse(text = df_expr)))
  } else {
    df_expr
  }
}

#' Predicate for adjusted Hotelling T2-based policy
#'
#' Predicate based on Hotelling's T² statistic with
#' an heuristic used to adjust the degrees of freedom purpoted distribution
#' under the null hypothesis.
#'
#' @param param.name Name of the significance level parameter
#' @param region Name of the region of observed effects
#'   for which the predicate should
#'   be `FALSE`
#' @param df Degrees of freedom.
#' @returns A [Schema()] of type predicate.
#'
#' @export
p_likelihood_adj_predicate <- function(param.name, region, df = 1) {
  make_schema(
    "p_likelihood_adj_predicate",
    param.name = param.name,
    region = region,
    df = df
  )
}

#' @describeIn p_likelihood_adj_predicate "Predicate"
#' @inheritParams schema_type
#' @param ... Other arguments (ignored)
#' @export
schema_type.p_likelihood_adj_predicate <- function(...) "Predicate"

#' @describeIn p_likelihood_adj_predicate This schema has several criteria (see `criteria_fun`)
#' @inheritParams criteria_info
#' @param ... Other arguments (ignored)
#' @export
criteria_info.p_likelihood_adj_predicate <- function(x, ...) {
  tibble::tibble(name = c("t2", "measure.region", "measure.region.min", "cov.structure", "df2"))
}

#' @describeIn p_likelihood_adj_predicate This schema has a single parameter of name
#'   `param.name`.
#' @inheritParams params_info
#' @param ... Other arguments (ignored)
#' @export
params_info.p_likelihood_adj_predicate <- function(x, ...) {
  tibble::tibble(name = x$param.name)
}

Pmvt_inf <- function(delta, df, sigma) {
  function(lower, upper) {
    if (all(upper == Inf)) {
      assertthat::assert_that(
        all(delta < Inf),
        msg = "delta must be on the edge of the cone"
      )
      assertthat::assert_that(length(delta) == length(lower))
      if (any(delta == -Inf & lower > -Inf)) {
        0
      } else {
        ii <- (delta > -Inf)
        my_pmvt_shifted(
          lower = lower[ii], upper = Inf,
          delta = delta[ii],
          df = df,
          sigma = sigma[ii, ii]
        )
      }
    } else if (all(lower == -Inf)) {
      assertthat::assert_that(
        all(delta > -Inf),
        msg = "delta must be on the edge of the cone"
      )
      assertthat::assert_that(length(delta) == length(upper))
      if (any(delta == +Inf & upper < +Inf)) {
        0
      } else {
        ii <- (delta < +Inf)
        my_pmvt_shifted(
          lower = -Inf, upper = upper[ii],
          delta = delta[ii],
          df = df,
          sigma = sigma[ii, ii]
        )
      }
    } else {
      assertthat::assert_that(FALSE, msg = "Pmvt_inf only supports cones")
    }
  }
}

#' @describeIn p_likelihood_adj_predicate Criteria:
#'
#'   | Criterion | Type | Description |
#'   |-----------|------|-------------|
#'   | t2  | numeric | Minimum T^2-induced distance of the observed effect to the region |
#'   | measure.region | numeric | Probability measure of the region at the observed true effect |
#'   | measure.region.min | numeric | Minimal probability measure of the region for true effects in the region |
#'   | cov.structure | character | How the covariance matrix for the T^2 statistic is obtained |
#'   | df2 | numeric | Degrees of freedom in the estimation of the covariance matrix for the T^2 statistic |
#'
#' @inheritParams criteria_fun
#' @export
criteria_fun.p_likelihood_adj_predicate <- vectorize_criteria_fun(
  function(x, endpoints, summary) {
    if (point_in(
      x$region,
      endpoints,
      summary$vars$delta.hat
    )) {
      tibble::tibble_row(
        t2 = 0,
        measure.region = 0,
        measure.region.min = 0.5,
        cov.structure = summary$covariance.structure,
        df2 = summary$degrees.of.freedom
      )
    } else {
      min_T2_fun <- min_T2_with_solution(summary, endpoints)
      T2 <- min_T2_fun(x$region)
      Pmvt <- function(lower, upper) {
        my_pmvt_shifted(
          lower = lower, upper = upper,
          delta = T2$solution,
          df = summary$degrees.of.freedom,
          sigma = summary$sigma.delta.hat
        )
      }
      tibble::tibble_row(
        t2 = T2$value,
        measure.region = measure_region(x$region, endpoints, Pmvt),
        measure.region.min = min(sapply(limit_points(x$region, endpoints), function(mean) {
          measure_region(x$region, endpoints, Pmvt_inf(
            delta = mean,
            df = summary$degrees.of.freedom,
            sigma = summary$sigma.delta.hat
          ))
        })),
        cov.structure = summary$covariance.structure,
        df2 = summary$degrees.of.freedom
      )
    }
  }
)

degrees_freedom_corner <- {
  corner.dim <- seq(1, 8)
  y <- sapply(corner.dim, function(d) {
    sum(seq(1, d) * choose(d, seq(1, d))) / sum(choose(d, seq(1, d)))
  })
  splinefun(c(0, corner.dim), c(0, y))
}

#' @describeIn p_likelihood_adj_predicate TRUE if and only if the
#'   p-value derived from the
#'   `t2` criterion is sufficiently large relative to the `param.name` parameter.
#' @inheritParams outcome_fun
#' @export
outcome_fun.p_likelihood_adj_predicate <-
  function(x, endpoints, params, criteria) {
    ifelse(
      criteria$t2 == 0,
      FALSE,
      {
        # V <- n_vars(endpoints)
        corner.dim <- log(criteria$measure.region.min) / log(.5)
        # df1 <- corner.dim
        df1 <- degrees_freedom_corner(corner.dim)
        # df1 <- parse_df(endpoints, x$df)
        p.outside.region <- (1 - criteria$measure.region.min)
        # factor <- 0.5
        p_value_chisq_vec(criteria$t2,
          criteria$cov.structure,
          df1 = df1,
          df2 = criteria$df2
        ) * p.outside.region <= params[[x$param.name]]
      }
    )
  }

p_value_chisq_vec <- function(t2, covariance.structure, df1, df2) {
  ifelse(covariance.structure == "theoretical",
    pchisq(t2, df = df1, lower.tail = FALSE),
    pf(t2 * (df2 - df1 + 1) / (df1 * df2), df1 = df1, df2 = df2 - df1 + 1, lower.tail = FALSE)
  )
}

## Measure-based policy #
#########################

#' Measure-based policy
#'
#' A policy based on a probability measure.
#'
#' @param false.stop A region schema (see [make_region()]) indicating the true
#'   effects for which a "stop" decision is not desired.
#' @param false.go A region schema (see [make_region()]) indicating the true
#'   effects for which a "go" decision is not desired.
#'
#' @returns A [Schema()] of type "Policy"
#'
#' @export
p_measure <- function(false.stop = r_whole, false.go = r_whole) {
  make_schema_alias("p_measure", false.stop, false.go)
}

#' @describeIn p_measure This policy is defined in terms of [p_policy],
#'   with the predicates defined using [p_measure_predicate()].
#' @inheritParams dealias
#' @export
dealias.p_measure <- function(x, ...) {
  p_policy(
    stop.if = p_measure_predicate(
      region = x$false.stop,
      param.name = "p.FS"
    ),
    go.if = p_measure_predicate(
      region = x$false.go,
      param.name = "p.FG"
    )
  )
}

#' Measure-based predicate
#'
#' A predicate based on a probability measure.
#'
#' @param param.name Name of the parameter controlling the strictness of the predicate.
#' @param region Region of true effects for which the predicate should be FALSE
#'
#' @returns A [Schema()] of type "Predicate".
#'
#' @details
#' The predicate is defined in terms of [make_predicate_for_level()],
#' with a single criterion "Pmvt", and a result of `TRUE` when
#' Pmvt is smaller than the parameter of tname `param.name`.
#'
#' @export
p_measure_predicate <- function(param.name, region) {
  make_predicate_for_level(
    "p_measure_predicate",
    param.name = param.name,
    criterion.name = "Pmvt",
    region = region
  )
}

#' @describeIn p_measure_predicate The schema produces a single criterion of name "Pmvt",
#' which is the probability that an observed effect will
#' be inside `region` for a probability distribution with
#' mean the observed effect, and the covariance matrix for the observed effect
#' either assumed known, or estimated from the data.
#' @inheritParams criteria_fun
#' @export
criteria_fun.p_measure_predicate <- vectorize_criteria_fun(
  function(x, endpoints, summary) {
    Pmvt <- function(lower, upper) {
      my_pmvt_shifted(
        lower = lower, upper = upper,
        delta = summary$vars$delta.hat,
        df = summary$degrees.of.freedom,
        sigma = summary$sigma.delta.hat
      )
    }
    tibble::tibble_row(
      Pmvt = measure_region(region = x$region, endpoints = endpoints, measure = Pmvt)
    )
  }
)

#' Policy based on simple hypothesis testing
#'
#' @param false.stop An effect specification for the False Stop case (see [parse_effect_vector()])
#' @param false.go   An effect specification for the False Go case (see [parse_effect_vector()])
#'
#' @returns A [Schema()] of type "Policy"
#'
#' @export
p_simple_hypothesis <- function(false.stop = c("-Inf"), false.go = c("+Inf")) {
  make_schema_alias("p_simple_hypothesis", false.stop, false.go)
}

#' @describeIn p_simple_hypothesis This policy is defined as an alias,
#'  in terms of [p_measure()].
#' @inheritParams dealias
#' @export
dealias.p_simple_hypothesis <- function(x, ...) {
  p_measure(
    false.stop = is_gt(x$false.stop),
    false.go = is_lt(x$false.go)
  )
}

## Negatively significant #
###########################

#' Predicate for negative significance
#'
#' @param level Name of the parameter representing the significance level
#'   at which to perform the check.
#' @returns A [Schema()] of type "Predicate"
#'
#' @details The predicate assumes that there is only a single endpoint.
#'
#' @export
is_negatively_significant_at_level <- function(level) {
  make_schema_alias("is_negatively_significant_at_level", level)
}

#' Predicate for negative significance
#'
#' Synonym of [is_negatively_significant_at_level()] where the parameter
#'   name is "alpha"
#'
#' @returns A [Schema()] of type "Predicate"
#'
#' @export
is_negatively_significant <- is_negatively_significant_at_level("alpha")

#' @describeIn is_negatively_significant_at_level This predicate is implemented
#'   as a [make_predicate_for_level()], with a parameter of name `level` and
#'   a criterion of name "neg.p".
#' @inheritParams dealias
#' @export
dealias.is_negatively_significant_at_level <- function(x, ...) {
  make_predicate_for_level(
    "is_negatively_significant_at_level",
    param.name     = x$level,
    criterion.name = "neg.p"
  )
}

#' @describeIn is_negatively_significant_at_level The criterion is the p-value
#'   corresponding to how much smaller than 0 the observed effect is.
#' @inheritParams criteria_fun
#' @export
criteria_fun.is_negatively_significant_at_level <- function(x, endpoints, summaries) {
  # Support calls with a single summary, and with a list of summaries
  summaries <- make_summaries(summaries)
  var.name <- endpoints$vars$name
  # Only one variable
  assertthat::assert_that(length(var.name) == 1)
  dd <- summaries_at_endpoint_vectorized(summaries, var.name)
  with(
    dd,
    tibble::tibble(neg.p = pt(
      q = delta.hat / delta.se,
      df = degrees.of.freedom,
      lower.tail = TRUE
    ))
  )
}

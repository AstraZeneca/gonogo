#' @import dplyr methods
NULL

# A Domain-Specific Language (DSL) for policies
#
# See Domain-Specific Languages in R by Thomas Mailund (book)
# for an explanation of the approach.
#
# The advantages of using a DSL (vs having the user implement policies as
# a function fulfilling a certain prototype) are the following:
#
# - Possibility to design a language with a syntax that is easy for
#   non-experienced programmers.
#
# - Ability to guarantee properties of the resulting policy (e.g. monotonicity,
#   performance).
#
# - The policy is a data structure, so it can be analyzed statically.
#   This makes certain features easier to implement.
#   For instance, the go/stop regions can be computed symbolically, and
#   their probabilities evaluated analytically. Or it can be formated
#   for display in natural language, or as a tree.
#   It can even be mechanically translated
#   to high-performance C++, vectorized R or to SAS code.
#
# Using a DSL has the following disadvantages:
#
# - Slightly more difficult for users to implement "exotic" behaviour not
#   covered by the DSL. (Although this can be fixed with suitable wrappers).
#
# - Difficult to debug the behaviour of the policy. (Although this can
#   be fixed by generating R code from the policy).


#' @describeIn make_schema Predicates and policies are defined as schemas,
#'   which, given some endpoints, can be interpreted as a concrete policy
#'   or predicate. Schemas all have class "Schema".
Schema <- "Schema"

#' Build a new schema
#'
#' Internal method to build new schemas.
#' The library user will typically not use [make_schema()] directly,
#' but instead a specific implementation.
#'
#' @param constructor Schema name; becomes one of its `class` (S3).
#' @param ...  Additional arguments for the object.
#'
#' @seealso A schema should implement the [schema_type()], [criteria_info()],
#'   [criteria_fun()], [params_info()] and [outcome_fun()] methods.
#'
#' @export
make_schema <- function(constructor, ...) {
  structure(tibble::lst(...),
    class = c(constructor, "Schema")
  )
}

#' The return type of the schema.
#'
#' The type of the schema determines the R type of the
#'   result of [outcome_fun()].
#'
#' @param x The schema (see [make_schema()])
#'
#' @returns A single string with the type of the schema.
#'    Valid types are "Decision", "Predicate" or "\[Predicate\]"
#'    (the latter can be read as "list of predicates").
#'
#' @seealso The type of the schema determines the type of [outcome_fun()]
#'
#' @export
schema_type <- function(x) UseMethod("schema_type")

#' List of names for the criteria used by this policy.
#'
#' @param x The schema.
#' @param endpoints [Endpoints()] at which the schema is interpreted.
#' @export
criteria_info <- function(x, endpoints) {
  assertthat::assert_that(is(endpoints, "Endpoints"))
  UseMethod("criteria_info")
}

#' Compute criteria for the policy given the study summary
#'
#' @param x [Schema()]
#' @param endpoints Endpoints of the studyeffect_is_gt
#' @param summaries Summary statistics for each of M studies
#'   (either a [Summary()], a [Summaries()] or a [list()] of Summary).
#'
#' @returns Data frame with M rows, and colums with names given
#'   by [criteria_info()].
#'
#' @export
criteria_fun <- function(x, endpoints, summaries) {
  assertthat::assert_that(is(endpoints, "Endpoints"))
  UseMethod("criteria_fun")
}

#' List of names of (hyper)parameters used by this policy (length P).
#'
#' @param x Policy schema ([Schema()])
#' @param endpoints [Endpoints()]
#'
#' @export
params_info <- function(x, endpoints) {
  assertthat::assert_that(is(endpoints, "Endpoints"))
  UseMethod("params_info")
}

#' Computes the outcome of a policy or predicate.
#'
#' @param x Policy or predicate schema
#' @param endpoints [Endpoints()] of the study.
#' @param params Parameters at which to apply the policy;
#'   the names of the list should be those in [params_info()].
#' @param criteria Criteria (computed by [criteria_fun()];
#'   a data frame of M rows for some M, and with the columns given
#'   by [criteria_info()].
#'
#' @returns Outcome of the policy or predicate, determined
#'   by the [schema_type()].
#'
#' - If the type is Predicate, the result is a logical vector of length M.
#' - If the type is Policy, the result is a data frame with M rows and 3 columns:
#'
#' | Name     | Type | Description |
#' |----------|------|-------------|
#' | decision | [DECISION()] | Overall policy decision: Stop if `decision.has.stop`; Go if `decision.has.go` and not `decision.has.stop`; Discuss otherwise. |
#' | decision.has.go | logical | Whether a go decision was produced. |
#' | decision.has.stop | logical | Whether a stop decision was produced. |
#'
#' - If the type is "\[Predicate\]", the result is a list
#'   of logical vectors of length M.
#'
#' @details
#' When combining the outcomes of several predicates,  this combination
#' is done pointwise for each of the M rows.
#'
#' @export
outcome_fun <- function(x, endpoints, params, criteria) {
  assertthat::assert_that(is(endpoints, "Endpoints"))
  assertthat::assert_that(is.list(params))
  assertthat::assert_that(is.data.frame(criteria))
  UseMethod("outcome_fun")
}


##################
# Selecting vars #
##################

#' Localize an object to a subset of the endpoints.
#'
#' @param obj Object to localize
#' @param vars.selector Endpoint selector.
#'
#' @returns An object of the same type at `obj`, ignoring those endpoints
#' not covered by `vars.selector`.
#'
#' @details If `obj` is an object containing information about endpoints
#' (e.g. a [Trial()] or a [Summary()])
#' only the information about the endpoins selected by `vars.selector` is preserved.
#' If `obj` is an object consuming information about endpoints
#' (e.g. a policy or predicate [Schema()]).
#'
#' @seealso [at_selected_vars.Schema()], [at_selected_vars.Region()]
#'
#' @export
at_selected_vars <- function(obj, vars.selector) UseMethod("at_selected_vars")

#' @describeIn at_selected_vars Localize an endpoints object.
#' @export
at_selected_vars.Endpoints <- function(obj, vars.selector) {
  endpoints <- obj
  vars.ii <- select_vars(vars.selector, endpoints$vars)
  vars <- endpoints$vars[vars.ii, , drop = FALSE]
  assertthat::assert_that(all(sort(names(endpoints$domains)) == sort(c("name", "n.vars"))))
  Endpoints(
    sigma = as.matrix(endpoints$sigma)[vars.ii, vars.ii, drop = FALSE],
    domains = vars %>%
      mutate(domain = factor(.data$domain, levels = unique(.data$domain))) %>%
      group_by(.data$domain) %>%
      summarise(n.vars = n()) %>%
      ungroup() %>%
      mutate(domain = as.character(.data$domain)) %>%
      rename(name = .data$domain),
    vars = vars,
    .params = NULL
  )
}

#' @describeIn at_selected_vars Localize a trial.
#' @export
at_selected_vars.Trial <- function(obj, vars.selector) {
  trial <- obj
  if (has_ground_truth(trial)) {
    warning("True effect for trial will be lost")
  }
  make_trial(
    endpoints = at_selected_vars(trial$endpoints, vars.selector),
    n.patients = trial$n.patients
  )
}

#' @describeIn at_selected_vars Localize all elements in a list.
#' @export
at_selected_vars.Summary <- function(obj, vars.selector) {
  summary <- obj
  vars.ii <- select_vars(vars.selector, summary$vars)
  Summary(
    n.patients = summary$n.patients,
    sigma.hat = summary$sigma.hat[vars.ii, vars.ii, drop = FALSE],
    sigma.delta.hat = summary$sigma.delta.hat[vars.ii, vars.ii, drop = FALSE],
    vars = summary$vars[vars.ii, , drop = FALSE],
    degrees.of.freedom = summary$degrees.of.freedom,
    covariance.structure = summary$covariance.structure
  )
}


#' @describeIn at_selected_vars Localize all elements in a list.
#' @export
at_selected_vars.Summaries <- function(obj, vars.selector) {
  summaries <- obj
  vars.ii <- select_vars(vars.selector, summaries$vars)
  Summaries(
    summaries.list =
      at_selected_vars(summaries$summaries.list, vars.selector),
    vectorized =
      summaries$vectorized %>%
        select(ends_with(paste0(
          ".",
          summaries$vars$name[vars.ii]
        ))),
    vars = summaries$vars[vars.ii, , drop = FALSE]
  )
}

#' @describeIn at_selected_vars Localize all elements in a list.
#' @export
at_selected_vars.list <- function(obj, vars.selector) {
  lapply(obj, function(elem) at_selected_vars(elem, vars.selector))
}

#' @export
at_selected_vars.NULL <- function(obj, vars.selector) {
  NULL
}

#' Localize a schema
#'
#' @param obj Schema (either a Policy or a Predicate).
#' @param vars.selector  Variable selector (object of class `Selector`).
#'
#' @returns A [Schema()] which performs the behaviour of `schema`, but
#'  only sees the endpoints selected by `vars.selector`.
#'
#' @export
at_selected_vars.Schema <- function(obj, vars.selector) {
  make_schema(
    "at_selected_vars",
    schema = obj,
    vars.selector
  )
}

#' @describeIn at_selected_vars.Schema The schema type coincides with the underlying `schema`
#' @inheritParams schema_type
#' @export
schema_type.at_selected_vars <- function(x) schema_type(x$schema)

#' @describeIn at_selected_vars.Schema The criteria names coincide with the underlying `schema`.
#' @inheritParams criteria_info
#' @export
criteria_info.at_selected_vars <- function(x, endpoints) {
  criteria_info(
    x$schema,
    at_selected_vars(endpoints, x$vars.selector)
  )
}

#' @describeIn at_selected_vars.Schema The criteria values coincide with the underlying `schema`.
#' @inheritParams criteria_fun
#' @export
criteria_fun.at_selected_vars <- function(x, endpoints, summaries) {
  criteria_fun(
    x$schema,
    at_selected_vars(endpoints, x$vars.selector),
    at_selected_vars(summaries, x$vars.selector)
  )
}

#' @describeIn at_selected_vars.Schema The parameter names coincide with the underlying `schema`.
#' @inheritParams params_info
#' @param ... Remaining arguments (passed through)
#' @export
params_info.at_selected_vars <- function(x, ...) params_info(x$schema, ...)

#' @describeIn at_selected_vars.Schema The outcome type coincides with the underlying `schema`.
#' @inheritParams outcome_fun
#' @param ... Remaining arguments (passed through)
#' @export
outcome_fun.at_selected_vars <- function(x, endpoints, ...) {
  outcome_fun(
    x$schema,
    at_selected_vars(endpoints, x$vars.selector),
    ...
  )
}

### Variable selectors

#' Internal constructor
#'
#' This constructor is used in internally to build selectors
#' (i.e. objects implementing [select_vars()]).
#'
#' @param type Constructor name/class
#' @param ... Other arguments (stored inside the selector)
#'
#' @returns A list of class `type` and class "Selector"
#'  containing the name-value pairs in `...`.
#'
#' @export
make_selector <- function(type, ...) {
  structure(tibble::lst(...), class = c(type, "Selector"))
}

#' Apply a variable selector
#'
#' @param x Variable selector
#' @param vars Dataframe containing a "name" column with
#'             unique names for each variable, and a "domain" column
#'             indicating the domain each variable belongs to.
#'
#' @export
select_vars <- function(x, vars) UseMethod("select_vars")

#' Selector for variables in a domain
#'
#' @param domains List of domains to select.
#' @returns An object of class `Selector`.
#'
#' @export
vars_in_domain <- function(domains) make_selector("vars_in_domain", domains)
#' @export
select_vars.vars_in_domain <- function(x, vars) {
  assertthat::assert_that(all(x$domains %in% vars$domain))
  which(vars$domain %in% x$domains)
}

#' Select variables by name.
#'
#' @param names Names of the variables to select.
#' @returns A selector for those variables.
#'
#' @export
vars_by_name <- function(names) make_selector("vars_by_name", names)

#' @export
select_vars.vars_by_name <- function(x, vars) {
  assertthat::assert_that(all(x$names %in% vars$name))
  which(vars$name %in% x$names)
}


#' Localize an object to the given variable(s)
#'
#' @param x Object to localize.
#' @param vars.names Names of the variables (i.e. endpoints)
#'
#' @details
#' `x` must be an object implementing [at_selected_vars()].
#'
#' @export
`%at_var%` <- function(x, vars.names) {
  at_selected_vars(x, vars_by_name(vars.names))
}

#' Localize an object to the given domains.
#'
#' @param x Object to localize
#' @param domains Names of the domains (character vector)
#'
#' @details
#' `x` must be an object implementing [at_selected_vars()].
#'
#' @export
`%at_domain%` <- function(x, domains) {
  at_selected_vars(x, vars_in_domain(domains))
}

#' Select all but the last variable.
#'
#' @export
all_but_last_var <- make_selector("all_but_last_var")
#' @export
select_vars.all_but_last_var <- function(x, vars) {
  V <- length(vars$name)
  if (V <= 1) {
    NULL
  } else {
    seq(1, V - 1, by = 1)
  }
}

#' Localize an object to the last variable.
#'
#' @export
last_var <- make_selector("last_var")
#' @export
select_vars.last_var <- function(x, vars) {
  V <- length(vars$name)
  if (V <= 0) {
    NULL
  } else {
    V
  }
}

#' Select domains using tidy_select
#'
#' @param tidy_select A [tidyr::tidyr_tidy_select()] expression.
#'   for the problem domains.
#' @returns A selector.
#'
#' @export
domains_tidy_select <- function(tidy_select) {
  make_selector("domains_tidy_select",
    tidy_select = rlang::enquo(tidy_select)
  )
}

#' @export
select_vars.domains_tidy_select <- function(x, vars) {
  domains <- unique(vars$domain)
  names(domains) <- domains
  domains <- tibble::as_tibble_row(domains)
  ii <- tidyselect::eval_select(x$tidy_select, domains)
  select_vars(vars_in_domain(domains[1, ii]), vars)
}

#' Localize an object to a tidyselect expression on domains.
#'
#' @param x  An object implementing [at_selected_vars()]
#' @param tidy_select  A [tidyr::tidyr_tidy_select()] expression
#'   corresponding to a subset of the domain names.
#'
#' @examples
#' e <- make_endpoints(n.domains = 3, domain.n.vars = 2)
#' e %at_domains% ("d1" | "d3")
#' @export
`%at_domains%` <- function(x, tidy_select) {
  at_selected_vars(x, domains_tidy_select({{ tidy_select }}))
}

## Select variables using tidy_select

#' Select endpoints using tidy_select
#'
#' @param tidy_select A [tidyr::tidyr_tidy_select()] expression.
#'   for the problem domains.
#' @returns A selector.
#'
#' @export
vars_tidy_select <- function(tidy_select) {
  make_selector("vars_tidy_select",
    tidy_select = rlang::enquo(tidy_select)
  )
}

#' @export
select_vars.vars_tidy_select <- function(x, vars) {
  vars.names <- vars$name
  names(vars.names) <- vars.names
  vars.names <- tibble::as_tibble_row(vars.names)
  ii <- tidyselect::eval_select(x$tidy_select, vars.names)
  select_vars(vars_by_name(vars.names[1, ii]), vars)
}

#' Select variables using a tidyselect expression
#'
#' @param x  An object implementing [at_selected_vars()]
#' @param tidy_select  A [tidyr::tidyr_tidy_select()] expression
#'   corresponding to a subset of the endpoint names.
#'
#' @examples
#' e <- make_endpoints(n.domains = 3, domain.n.vars = 2)
#' e %at_vars% ("d1.v1" | "d3.v2")
#' @export
`%at_vars%` <- function(x, tidy_select) {
  at_selected_vars(x, vars_tidy_select({{ tidy_select }}))
}

####################
# Low-level operations

# with_endpoints
# with_endpoints.Schema <- function(FUN) {
#   make_schema("with_endpoints",
#               FUN = FUN)
# }
#
# criteria.info(
#
#
# with_endpoints <- function(FUN) {
#   Schema(function(endpoints) {
#     FUN(endpoints)$rule(endpoints)
#   })
# }
#
# with_rule <- function(schema, fn, ...) {
#   Schema(function(endpoints) {
#     fn(schema$rule(endpoints), ...)
#   })
# }
#
# map_rule <- function(x, ...) UseMethod("map_rule")
#
# map_rule.Schema <- function(schema, ...) {
#   with_rule(schema, map_rule.Rule, ...)
# }

#' Modifies the outcome of a schema using a simple
#' function.
#'
#' @param schema Schema of type `type.in`
#' @param FUN Function mapping an outcome of type `type.in` to
#'   an outcome of type `type.out`.
#' @param type.in Type of schema.
#' @param type.out Type of the resulting schema.
#'
#' @returns A [Schema()] of type `type.out`.
#'
#' @export
map_schema <- function(schema, FUN, type.in, type.out) {
  assertthat::assert_that(schema_type(schema) == type.in)
  make_schema("map_schema", schema, FUN, type.in, type.out)
}

#' @describeIn map_schema The type is given by `type.out`
#' @inheritParams schema_type
#' @export
schema_type.map_schema <- function(x) x$type.out

#' @describeIn map_schema The criteria names coincide with the underlying `schema`.
#' @inheritParams criteria_info
#' @param ... Other arguments (passed through)
#' @export
criteria_info.map_schema <- function(x, ...) criteria_info(x$schema, ...)

#' @describeIn map_schema The criteria values coincide with the underlying `schema`.
#' @inheritParams criteria_fun
#' @export
criteria_fun.map_schema <- function(x, ...) criteria_fun(x$schema, ...)

#' @describeIn map_schema The parameter names coincide with the underlying `schema`.
#' @inheritParams params_info
#' @export
params_info.map_schema <- function(x, ...) params_info(x$schema, ...)

#' @describeIn map_schema The outcome results from applying `FUN` to the outcome of the underlying `schema`.
#' @inheritParams outcome_fun
#' @export
outcome_fun.map_schema <- function(x, ...) x$FUN(outcome_fun(x$schema, ...))


#' Composes multiple schemas
#'
#' @param schemas A [list()] of [Schema()].
#' @param homogeneous Whether to treat the list of schemas as a list of schemas with
#'   schemas of the same type (should always be set to `FALSE` if this might not be the case).
#' @param element.type If `schemas` could possibly be empty and `homogeneous` is set
#'   to `TRUE` the type of the elements
#'   of `schemas` should be given.
#' @returns
#'   If `homogeneous` is set to `TRUE`, a [Schema()] of type "\[A\]",
#'   where A is the type of any of the `schemas` (or `element.type` if given).
#'
#'   If `homogeneous` is `FALSE`, a schema of type "(A,B,C,...)", where
#'   A, B, C, ... are the schema types of the schemas in the list.
#'
#' @export
sequence_schemas <- function(schemas, homogeneous = TRUE, element.type = NULL) {
  # Check that there will be a  type
  assertthat::assert_that(!homogeneous ||
    !is.null(element.type) ||
    length(schemas) >= 1)
  make_schema(
    "sequence_schemas",
    schemas,
    homogeneous,
    element.type
  )
}

#' @describeIn sequence_schemas If `homogeneous` is set to `TRUE`,  "\[A\]",
#'   where A is the type of any of the `schemas` (or `element.type` if given).
#'   If `homogeneous` is `FALSE`, "(A,B,C,...)", where
#'   A, B, C, ... are the schema types of the schemas in the list.
#' @inheritParams schema_type
#' @export
schema_type.sequence_schemas <- function(x) {
  types <- sapply(x$schemas, schema_type)
  if (x$homogeneous) {
    types <- unique(c(x$element.type, types))
    assertthat::are_equal(length(types), 1)
    type <- paste0("[", types, "]")
  } else {
    type <- paste0("(", paste(types, collapse = ","), ")")
  }
  type
}

empty_criteria_info <- function(x, ...) {
  tibble::tibble(name = character(0))
}

#' @describeIn sequence_schemas The union of the criteria names of the
#'   underlying `schemas`. It should not be the case that
#'   several schemas share the same criteria names.
#' @inheritParams criteria_info
#' @param ... Other parameters (passed through)
#' @export
criteria_info.sequence_schemas <- function(x, ...) {
  if (length(x$schemas) == 0) {
    return(empty_criteria_info(x, ...))
  }
  info <- bind_rows(lapply(x$schemas, function(schema) criteria_info(schema, ...)))
  assertthat::assert_that(anyDuplicated(info$name) == 0)
  info
}

empty_criteria <- function(x, endpoints, summaries) {
  summaries <- make_summaries(summaries)
  tibble::tibble(.rows = nrow(summaries$vectorized))
}

#' @describeIn sequence_schemas The criteria values are all of the criteria values
#'  for all the underlying `schemas`.
#' @inheritParams criteria_fun
#' @param ... Other parameters (passed through)
#' @export
criteria_fun.sequence_schemas <- function(x, ...) {
  if (length(x$schemas) == 0) {
    return(empty_criteria(x, ...))
  }
  res <- lapply(x$schemas, function(schema) criteria_fun(schema, ...))
  bind_cols(res, .name_repair = "check_unique")
}

empty_params_info <- function(x, ...) {
  tibble::tibble(name = character(0))
}

#' @describeIn sequence_schemas The parameter names are the union of the parameter
#'   names of all the underlying schemas. Several of the `schemas`
#'   may share the same parameter names.
#' @inheritParams params_info
#' @param ... Other parameters (passed through)
#' @export
params_info.sequence_schemas <- function(x, ...) {
  if (length(x$schemas) == 0) {
    return(empty_params_info(x, ...))
  }
  bind_rows(lapply(x$schemas, function(schema) params_info(schema, ...))) %>%
    dplyr::distinct()
}

#' @describeIn sequence_schemas The outcome is a list, of the same dimensions
#'   and name structure as `schemas`, containing the outcomes for each
#'   of the `schemas`.
#' @inheritParams outcome_fun
#' @param ... Other parameters (passed through)
#' @export
outcome_fun.sequence_schemas <- function(x, ...) {
  args <- list(...)
  assertthat::assert_that(!any(c("X", "FUN") %in% names(args)))
  lapply(X = x$schemas, FUN = function(schema, ...) {
    outcome_fun(schema, ...)
  }, ...)
}

#' Adds a prefix to the criteria of a `schema`
#'
#' @param schema Underlying schema.
#' @param prefix Prefix to add to the criteria names (including eventual separator, like ".")
#'
#' @details
#' Useful in combination with, for example, [sequence_schemas()],
#' to ensure that criteria names do not clash.
#'
#' @export
with_criteria_prefix <- function(schema, prefix) {
  make_schema("with_criteria_prefix", schema, prefix)
}

#' @describeIn with_criteria_prefix The schema type coincides with the underlying `schema`.
#' @inheritParams schema_type
#' @export
schema_type.with_criteria_prefix <- function(x) schema_type(x$schema)

#' @describeIn with_criteria_prefix The criteria names are those of the underlying
#'   `schema` with `prefix` prepended (no separator).
#' @inheritParams criteria_info
#' @param ... Other parameters (passed through)
#' @export
criteria_info.with_criteria_prefix <- function(x, ...) {
  criteria_info(x$schema, ...) %>%
    mutate(name = paste0(!!(x$prefix), .data$name))
}

#' @describeIn with_criteria_prefix The criteria values are those of the underlying
#'  `schema`, with the names adequately prefixed.
#' @inheritParams criteria_fun
#' @param ... Other parameters (passed through)
#' @export
criteria_fun.with_criteria_prefix <- function(x, ...) {
  criteria_fun(x$schema, ...) %>%
    dplyr::rename_with(function(name) paste0(x$prefix, name))
}

#' @describeIn with_criteria_prefix The parameter names are those of the underlying
#'   `schema`, unmodified.
#' @inheritParams params_info
#' @param ... Other parameters (passed through)
#' @export
params_info.with_criteria_prefix <- function(x, ...) {
  params_info(x$schema, ...)
}

#' @describeIn with_criteria_prefix The outcome is the same as that of the underlying
#'   `schema`.
#' @inheritParams outcome_fun
#' @export
outcome_fun.with_criteria_prefix <- function(x, endpoints, params, criteria) {
  # print("-------")
  # print(x$prefix)
  # print(names(criteria))
  # print("-------")
  outcome_fun(
    x$schema, endpoints,
    params,
    criteria %>%
      dplyr::select(starts_with(x$prefix)) %>%
      dplyr::rename_with(function(name) {
        substring(name, nchar(x$prefix) + 1)
      })
  )
}

#' Apply a list of prefixes to a list of schemas
#'
#' @param xs A [list()] of schemas
#' @param prefixes A [list()] of prefixes (character), of the same length as xs.
#' @returns A list of schemas of the same length as `xs`,
#'   each of which is the corresponding element of `xs` with the criteria
#'   prefixed by the corresponding prefix in `prefixes`.
#'   (See [with_criteria_prefix()]).
#'
#' @export
with_criteria_prefixes <- function(xs, prefixes) {
  res <- lapply(
    seq(length(xs)),
    function(i) xs[[i]] %>% with_criteria_prefix(prefixes[[i]])
  )
  res
}

#' Schema extension
#'
#' @details
#' An schema alias defines a Schema in terms of an existing one.
#'
#' In its simplest form it is just a synonym, but the methods can be
#' overriden to modify this default behaviour.
#' For instance, the way to dealias may depend on the concrete endpoints
#' of the domain.
#'
#' @param type Constructor name, becomes part of the S3 class.
#' @param ... Other arguments (passed through).
#' @returns A [Schema()].
#'
#' @details The meaning of the alias is given by implementing the `dealias.<type>` S3 method.
#'
#' @seealso [dealias()]
#'
#' @export
make_schema_alias <- function(type, ...) {
  make_schema(c(type, "Alias"), ...)
}

#' Constructor extension
#'
#' The `delias` method returns an equivalent form of a schema.
#' This equivalent form may depend on the endpoints.
#' Note that the resulting form must be completely equivalent.
#'
#' @param x The schema.
#' @param ... Other parameters potentially needed to dealias the schema.
#' @returns A [Schema()] of the same type as `x` which is in some sense in an equivalent,
#'   simplifed form.
#'
#' @seealso [make_schema_alias()]
#'
#' @export
dealias <- function(x, ...) UseMethod("dealias")

#' @describeIn make_schema_alias By default, this method is implemented by applying the method
#'   of the same name to the `dealias`-ed form.
#' @inheritParams schema_type
#' @export
schema_type.Alias <- function(x, ...) schema_type(dealias(x, ...), ...)

#' @describeIn make_schema_alias By default, this method is implemented by applying the method
#'   of the same name to the `dealias`-ed form.
#' @inheritParams criteria_info
#' @export
criteria_info.Alias <- function(x, ...) criteria_info(dealias(x, ...), ...)

#' @describeIn make_schema_alias By default, this method is implemented by applying the method
#'   of the same name to the `dealias`-ed form.
#' @inheritParams criteria_fun
#' @export
criteria_fun.Alias <- function(x, ...) criteria_fun(dealias(x, ...), ...)

#' @describeIn make_schema_alias By default, this method is implemented by applying the method
#'   of the same name to the `dealias`-ed form.
#' @inheritParams params_info
#' @export
params_info.Alias <- function(x, ...) params_info(dealias(x, ...), ...)

#' @describeIn make_schema_alias By default, this method is implemented by applying the method
#'   of the same name to the `dealias`-ed form.
#' @inheritParams outcome_fun
#' @export
outcome_fun.Alias <- function(x, ...) outcome_fun(dealias(x, ...), ...)


### Quantification

#' Expansion over domains
#'
#' @details
#'
#' This keyword may refer to:
#'
#' - [at_each_domain.Schema()], when implementing policies.
#' - [at_each_domain.Region()], when describing observed effect regions.
#'
#' @param schema Schema that should hold at each domain.
#'
#' @export
at_each_domain <- function(schema) UseMethod("at_each_domain")

#' Expansion over domains
#'
#' @param schema A schema intended to be used within a single domain.
#' @returns A list-schema, which produces one outcome for each domain
#'   by restricting `schema` to the endpoints in that domain
#'
#' @export
at_each_domain.Schema <- function(schema) {
  make_schema_alias("at_each_domain", schema)
}

#' @describeIn at_each_domain.Schema The type of this Schema is "\[A\]",
#'   where "A" is the type of the underlying `schema`. "A" is typically "Predicate".
#' @inheritParams schema_type
#' @export
schema_type.at_each_domain <- function(x) {
  paste0("[", schema_type(x$schema), "]")
}

#' @describeIn at_each_domain This schema is implemented by creating
#'   a list with as many copies of the schema as there are domains,
#'   restricting the schema to the endpoints in that domain,
#'   and using [sequence_schemas()] to produce a list-schema from the result.
#'
#' @inheritParams dealias
#' @param endpoints The meaning of this schema depends on the endpoints.
#'
#' @export
dealias.at_each_domain <- function(x, endpoints, ...) {
  x.dealiased <- sequence_schemas(
    lapply(
      endpoints$domains$name,
      function(name) {
        (x$schema %at_domain% name) %>%
          with_criteria_prefix(paste0(name, "."))
      }
    ),
    element.type = schema_type(x$schema)
  )
  assertthat::assert_that(schema_type(x) == schema_type(x.dealiased))
  x.dealiased
}

#' Expansion over endpoints
#'
#' @param schema An schema which shall be interpreted at each endpoint.
#'
#' @returns A list schema.
#'
#' @details
#'
#' This keyword may refer to:
#' - [at_each_endpoint.Schema()], when implementing policies.
#' - [at_each_endpoint.Region()], when describing observed effect regions.
#'
#' @export
at_each_endpoint <- function(schema) {
  UseMethod("at_each_endpoint")
}

#' Expansion over endpoints
#'
#' @param schema A schema intended to be used within a single endpoint.
#'   (For instance, [p_lalonde()]).
#' @returns A list-schema, which produces one outcome for each enpoint
#'   by restricting `schema` to that endpoint.
#'
#' @export
at_each_endpoint.Schema <- function(schema) {
  make_schema_alias("at_each_endpoint", schema)
}

#' @describeIn at_each_endpoint.Schema The type of this Schema is "\[A\]",
#'   where "A" is the type of the underlying `schema`. "A" is typically "Predicate".
#'
#' @inheritParams schema_type
#'
#' @export
schema_type.at_each_endpoint <- function(x) {
  paste0("[", schema_type(x$schema), "]")
}

#' @describeIn at_each_endpoint.Schema  This schema is implemented by creating
#'   a list with as many copies of the schema as there are endpoints,
#'   restricting the schema to each endpoint,
#'   and using [sequence_schemas()] to produce a list-schema from the result.
#' @inheritParams dealias
#' @param endpoints The meaning of this schema depends on the endpoints.
#'
#' @export
dealias.at_each_endpoint <- function(x, endpoints, ...) {
  x.dealiased <- sequence_schemas(
    lapply(
      endpoints$vars$name,
      function(name) {
        (x$schema %at_var% name) %>%
          with_criteria_prefix(paste0(name, "."))
      }
    ),
    element.type = schema_type(x$schema)
  )
  assertthat::assert_that(schema_type(x) == schema_type(x.dealiased))
  x.dealiased
}

#####################

#' A schema returning a constant value
#'
#' @param type Type of the schema
#' @param outcome The desired outcome for the schema
#'
#' @export
constant_schema <- function(type, outcome) {
  make_schema("constant_schema", type, outcome)
}

#' @describeIn constant_schema The type of the schema is `type`
#' @inheritParams schema_type
#' @export
schema_type.constant_schema <- function(x) x$type

#' @describeIn constant_schema There are no criteria
#' @param ... Other arguments (ignored)
#' @export
criteria_info.constant_schema <- function(...) tibble::tibble(name = character())

#' @describeIn constant_schema There are no criteria
#' @param ... Other arguments (ignored)
#' @export
criteria_fun.constant_schema <- function(...) tibble::tibble_row()

#' @describeIn constant_schema There are no parameters
#' @param ... Other arguments (ignored)
#' @export
params_info.constant_schema <- function(...) tibble::tibble(name = character())

#' @describeIn constant_schema The outcome is `outcome`
#' @inheritParams outcome_fun
#' @param ... Other arguments (ignored)
#' @export
outcome_fun.constant_schema <- function(x, ...) x$outcome

#' Check if a policy decision is `Go`
#'
#' @param schema Underlying [Schema()] of type "Policy"
#' @returns A [Schema()] of type "Predicate"
#' @export
is_Go <- function(schema) make_schema_alias("is_Go", schema)
#' @describeIn is_Go This is implemented by running the policy, and comparing
#'   the outcome decision against `DECISION$Go`.
#' @inheritParams dealias
#' @export
dealias.is_Go <- function(x, ...) {
  map_schema(x$schema,
    FUN = function(d) {
      d$decision == DECISION$Go
    },
    type.in = "Policy",
    type.out = "Predicate"
  )
}

#' Check if a policy decision is `Stop`
#'
#' @param schema Underlying [Schema()] of type "Policy"
#' @returns A [Schema()] of type "Predicate"
#' @export
is_Stop <- function(schema) make_schema_alias("is_Stop", schema)
#' @describeIn is_Stop This is implemented by running the policy, and comparing
#'   the outcome decision against `DECISION$Stop`.
#'   The criteria and parameters are the same as the underlying policy.
#' @inheritParams dealias
#' @export
dealias.is_Stop <- function(x, ...) {
  map_schema(x$schema,
    FUN = function(d) {
      d$decision == DECISION$Stop
    },
    type.in = "Policy",
    type.out = "Predicate"
  )
}

#' Conjunction of predicates
#'
#' @param schemas A list of [Schema()]s of type "Predicate"
#'
#' @returns A [Schema()] of type predicate.
#'
#' @export
and <- function(schemas) {
  make_schema_alias("and", schemas)
}

#' @describeIn and The outcome of the predicate is TRUE if all the underlying
#'   predicates are true.
#'   The schema `and` is implemented by sequencing the list of schemas
#'   (using [sequence_schemas()]) and then applying [p_all()].
#'   The criteria are prefixed with "and.i." by using [with_criteria_prefixes()]
#'   to avoid clashes, where `i` is the index of the schema in `schemas`.
#'
#' @inheritParams dealias
#'
#' @export
dealias.and <- function(x, ...) {
  prefixes <- paste0("and.", seq(length(x$schemas)), ".")
  schemas <- with_criteria_prefixes(x$schemas, prefixes)
  p_all(
    sequence_schemas(schemas)
  )
}

#' Disjunction of predicates
#'
#' @param schemas A list of [Schema()]s of type "Predicate"
#' @returns A [Schema()] of type predicate.
#'
#' @export
or <- function(schemas) make_schema_alias("or", schemas)
#' @describeIn or The outcome of the predicate is TRUE if any the underlying
#'   predicates are true. The schema `or` is implemented by sequencing the list of schemas
#'   (using [sequence_schemas()]) and then applying [at_least_1()]. The criteria are prefixed with "or.i."
#'   to avoid clashes, where `i` is the index of the schema in `schemas`.
#'
#' @inheritParams dealias
#'
#' @export
dealias.or <- function(x, ...) {
  prefixes <- paste0("or.", seq(length(x$schemas)), ".")
  schemas <- with_criteria_prefixes(x$schemas, prefixes)
  at_least_1(
    sequence_schemas(schemas)
  )
}

#' Conjunction operator
#'
#' @details
#' This keyword has two meanings:
#' - \code{\link{\%and\%.Region}}, when defining effect regions.
#' - \code{\link{\%and\%.Schema}}, when defining policies.
#'
#' @param x,y Schemas
#'
#' @export
`%and%` <- function(x, y) UseMethod("%and%")
#' @describeIn grapes-and-grapes-.Schema This handler is used to flatten nested calls to `%and%`.
#' @export
`%and%.and` <- function(x, y) and(c(x$schemas, list(y)))

#' Conjunction of two predicates
#'
#' @param x A [Schema()] of type "Predicate"
#' @param y A [Schema()] of type "Predicate"
#' @returns A [Schema()] of type "Predicate"
#'
#' @details The `%and%` operator is an alias for [and()].
#' @export
`%and%.Schema` <- function(x, y) and(list(x, y))


#' Conjunction operator
#'
#' @details
#' This keyword has two meanings:
#' - \code{\link{\%or\%.Region}}, when defining effect regions.
#' - \code{\link{\%or\%.Schema}}, when defining policies.
#'
#' @param x,y Left and right operators of the disjunction.
#'
#' @export
`%or%` <- function(x, y) UseMethod("%or%")
#' @describeIn grapes-or-grapes-.Schema This handler is used to flatten nested calls to `%or%`.
#' @export
`%or%.or` <- function(x, y) or(c(x$schemas, list(y)))

#' Disjunction of two predicates
#'
#' @param x A [Schema()] of type "Predicate".
#' @param y A [Schema()] of type "Predicate".
#' @returns A [Schema()] of type "Predicate".
#'
#' @details
#' The `%or%` operator is an alias for [or()].
#' @export
`%or%.Schema` <- function(x, y) or(list(x, y))

#' Negation of a predicate
#'
#' @param schema A [Schema()] of type "Predicate"
#' @returns A [Schema()] of type "Predicate"
#'
#' @export
p_not <- function(schema) {
  make_schema_alias("p_not", schema)
}

#' @describeIn p_not `!` is an alias for `p_not`
#' @export
`!.Schema` <- p_not

#' @describeIn p_not The outcome of `p_not` is the negation of
#' the outcome of the underlying `schema`. The criteria and parameters
#' are the same as those of the underlying `schema`.
#' @inheritParams dealias
#' @export
dealias.p_not <- function(x, ...) {
  map_schema(x$schema,
    `!`,
    type.in = "Predicate",
    type.out = "Predicate"
  )
}

#' At least k
#'
#' @details
#' This keyword has two meanings:
#'
#' - [at_least_k.Schema], when defining policies and predicates.
#' - [at_least_k.Region], when defining effect regions.
#'
#' @param k Number of schemas that should hold
#' @param x A list Schema.
#'
#' @export
at_least_k <- function(k, x) UseMethod("at_least_k", x)

#' At least k hold
#'
#' @param k Integer constant
#' @param x A [Schema()] of type "\[Predicate\]"
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' The outcome is `TRUE` if the outcome of `k` or more of the predicates
#' in the list is `TRUE`.
#'
#' @seealso Schemas of type "\[Predicate\]" can be built using
#'   [sequence_schemas()], [at_each_endpoint()] or [at_each_domain()], for example.
#'
#' @export
at_least_k.Schema <- function(k, x) {
  make_schema_alias("at_least_k", k, predicate.list.schema = x)
}

#' @describeIn at_least_k.Schema Implementation
#' @inheritParams dealias
#' @param endpoints The meaning of this schema depends on the endpoints.
#' @param params The meaning of this schema depends on the parameters.
#' @export
dealias.at_least_k <- function(x, endpoints = NULL, params = NULL, ...) {
  with_parameter(x$k, function(k) {
    map_schema(
      x$predicate.list.schema,
      function(t) Reduce("+", t, init = 0) >= k,
      type.in = "[Predicate]",
      type.out = "Predicate"
    )
  })
}


#' At least 1
#'
#' @details
#' This keyword has two more specific meanings:
#'
#' - [at_least_1.Schema()], when defining policies and predicates.
#' - [at_least_1.Region()], when defining effect regions.
#'
#' @param list.schema List of schemas of which at least one should hold.
#'
#' @export
at_least_1 <- function(list.schema) UseMethod("at_least_1")

#' At least 1 holds
#'
#' @param list.schema A [Schema()] of type "\[Predicate\]", of which at least
#'   one should hold.
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' The outcome is `TRUE` if the outcome of one or more of the predicates
#' in the list is `TRUE`.
#'
#' @seealso Schemas of type "\[Predicate\]" can be built using
#'   [sequence_schemas()], [at_each_endpoint()] or [at_each_domain()], for example.
#'
#' @export
at_least_1.Schema <- function(list.schema) {
  make_schema_alias("at_least_1", predicate.list.schema = list.schema)
}

#' @describeIn at_least_1.Schema At least 1 is implemented as an alias.
#'
#' @inheritParams dealias
#'
#' @export
dealias.at_least_1 <- function(x, ...) {
  map_schema(
    x$predicate.list.schema,
    function(t) Reduce("|", t, init = FALSE),
    type.in = "[Predicate]",
    type.out = "Predicate"
  )
}


#' All hold
#'
#' @param predicate.list.schema A [Schema()] of type "\[Predicate\]"
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' The outcome is `TRUE` if the outcome of all of the predicates
#' used to build the list-schema is `TRUE`.
#'
#' @seealso Schemas of type "\[Predicate\]" can be built using
#'   [sequence_schemas()], [at_each_endpoint()] or [at_each_domain()], for example.
#'
#' @export
p_all <- function(predicate.list.schema) {
  make_schema_alias("p_all", predicate.list.schema)
}

#' @describeIn p_all Implementation
#' @inheritParams dealias
#' @export
dealias.p_all <- function(x, ...) {
  map_schema(
    x$predicate.list.schema,
    function(t) Reduce("&", t, init = TRUE),
    type.in = "[Predicate]",
    type.out = "Predicate"
  )
}

# With parameter

#' Introduce a parameter
#'
#' This allows for introducing a new parameter to the policy.
#'
#' @param param.expr Either the name of a parameter to introduce,
#'    or a non-character value.
#' @param cont A function taking a single argument, and returning
#'    a [Schema()].
#'
#' @details
#' If `param.expr` is a non-character value, this is equivalent to
#' applying `cont` to `param.expr`.
#'
#' Otherwise, if `param.expr` is a character string:
#'  - When parameters are given (e.g. when calling [outcome_fun()]),
#'    the schema resulting from calling `cont` with the parameter
#'    value (if `param.expr` is of type `"character"` is used).
#'  - When parameters are given, a NULL value is given to `cont`, and
#'    the resulting schema is used.
#'
#' This function is unsafe, as it relies on the schema resulting from `cont`
#' not depending meaningfully on `param.expr` except when calling methods
#' that take policy parameters as arguments (e.g. [outcome_fun()]).
#'
#' @export
with_parameter <- function(param.expr, cont) {
  assertthat::assert_that(length(param.expr) == 1)
  make_schema_alias("with_parameter", param.expr, cont)
}

#' @describeIn with_parameter Implementation
#' @inheritParams dealias
#' @param endpoints The meaning of this schema may depend on the endpoints.
#' @param params The meaning of this schema may depend on the policy parameters.
#' @export
dealias.with_parameter <- function(x, endpoints = NULL, params = NULL, ...) {
  if (is.character(x$param.expr)) {
    if (is.null(params)) {
      x$cont(NULL)
    } else {
      x$cont(params[[x$param.expr]])
    }
  } else {
    x$cont(x$param.expr)
  }
}

#' @describeIn with_parameter The parameters are the same as those
#'   of `cont(NULL)`, together with `param.expr` (if `param.expr` is
#'   of character type).
#' @export
params_info.with_parameter <- function(x, ...) {
  info <- params_info(x$cont(NULL), ...)
  if (is.character(x$param.expr)) {
    info <- bind_rows(
      tibble::tibble_row(name = x$param.expr),
      info
    ) %>% distinct()
  }
  info
}

### At most k

#' At most k hold
#'
#' @details
#' This keyword has only one implementation, namely [at_most_k.Schema].
#'
#' @inheritParams at_most_k.Schema
#'
#' @export
at_most_k <- function(k, x) UseMethod("at_most_k", x)

#' At most k hold
#'
#' @param k An integer constant
#' @param x A [Schema()] of type "\[Predicate\]"
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' The outcome is `TRUE` if the outcome of `k` or fewer of the predicates
#' in the list is `TRUE`.
#'
#' @seealso Schemas of type "\[Predicate\]" can be built using
#'   [sequence_schemas()], [at_each_endpoint()] or [at_each_domain()], for example.
#'
#' @export
at_most_k.Schema <- function(k, x) {
  make_schema_alias("at_most_k", k, predicate.list.schema = x)
}

#' @describeIn at_most_k.Schema Implementation
#'
#' @inheritParams dealias
#'
#' @export
dealias.at_most_k <- function(x, ...) {
  with_parameter(x$k, function(k) {
    map_schema(
      x$predicate.list.schema,
      function(t) Reduce("+", t, init = 0) <= k,
      type.in = "[Predicate]",
      type.out = "Predicate"
    )
  })
}

#' Flatten a list of predicates
#'
#' @param schemas A [list()] containing a mixture of schemas
#'   of type "\[Predicate\]" and "Predicate"
#' @returns A [Schema()] of type "\[Predicate\]".
#'
#' @details
#' This function can be used to combine list-schemas
#'   such as those obtained from [at_each_domain()], with
#'   single schemas.
#'
#' @seealso Some applications are [at_most_k_of.Schema] and [at_least_k_of.Schema].
flatten_predicates <- function(schemas) {
  assertthat::assert_that(length(schemas) >= 1)
  schemas <- lapply(seq(length(schemas)), function(i) {
    schema <- schemas[[i]]
    res <- if (schema_type(schema) == "Predicate") {
      map_schema(schema, function(t) list(t), type.in = "Predicate", type.out = "[Predicate]")
    } else if (schema_type(schema) == "[Predicate]") {
      schema
    } else {
      assertthat::assert_that(FALSE, msg = "Schema has wrong type")
    }
    res %>% with_criteria_prefix(paste0("arg", i, "."))
  })
  map_schema(
    sequence_schemas(schemas),
    function(ts) {
      do.call(base::c, ts)
    },
    type.in = "[[Predicate]]",
    type.out = "[Predicate]"
  )
}

## at_most_k_of

#' At most k hold
#'
#' @details
#' This keyword has only one implementation: [at_most_k_of.Schema].
#'
#' @inheritParams at_most_k_of.Schema
#'
#' @export
at_most_k_of <- function(k, x, ...) UseMethod("at_most_k_of", x)

#' At most k hold
#'
#' @param k An integer constant
#' @param ... A combination of schemas of type "Predicate" and "\[Predicate\]".
#'
#' @returns A [Schema()] of type "Predicate"
#'
#' @seealso This is a generalization of [at_most_k.Schema].
#'
#' @export
at_most_k_of.Schema <- function(k, ...) {
  make_schema_alias("at_most_k_of", k, schemas = list(...))
}

#' @describeIn at_most_k_of.Schema Implementation
#'
#' @inheritParams dealias
#'
#' @export
dealias.at_most_k_of <- function(x, ...) {
  at_most_k(x$k, flatten_predicates(x$schemas))
}

## at_least_k_of

#' At least k hold
#'
#' @details
#' This keyword has only one implementation: [at_least_k_of.Schema].
#' @param x A schema or list schema.
#'
#' @inheritParams at_least_k_of.Schema
#'
#' @export
at_least_k_of <- function(k, x, ...) UseMethod("at_least_k_of", x)

#' At least k hold
#'
#' @param k An integer constant
#' @param ... A combination of schemas of type "Predicate" and "\[Predicate\]".
#'
#' @returns A [Schema()] of type "Predicate"
#'
#' @seealso This is a generalization of [at_least_k.Schema].
#'
#' @export
at_least_k_of.Schema <- function(k, ...) {
  make_schema_alias("at_least_k_of", k, schemas = list(...))
}

#' @describeIn at_least_k_of.Schema Implementation
#'
#' @inheritParams dealias
#'
#' @export
dealias.at_least_k_of <- function(x, ...) {
  at_least_k(x$k, flatten_predicates(x$schemas))
}

## all_of

#' All hold
#'
#' @details
#' This keyword has only one implementation: [p_all_of.Schema()].
#'
#' @param x A schema
#' @inheritParams p_all_of.Schema
#'
#' @export
p_all_of <- function(x, ...) UseMethod("p_all_of", x)

#' All hold
#'
#' @param ... A combination of [Schema]s of type "Predicate" and "\[Predicate\]"
#' @returns A [Schema()] of type predicate.
#'
#' @seealso This schema is a generalization of [p_all]. It is implemented
#' as an alias (see [dealias.p_all_of()]).
#'
#' @export
p_all_of.Schema <- function(...) {
  make_schema_alias("p_all_of", schemas = list(...))
}

#' Implementation of [p_all_of()]
#'
#' @inheritParams dealias
#' @param ... Additional arguments (ignored)
#'
#' @export
dealias.p_all_of <- function(x, ...) {
  p_all(flatten_predicates(x$schemas))
}

### None of

#' None hold
#'
#' @details
#' This keyword has only one implementation: [none_of.Schema].
#'
#' @param list.schema A list schema.
#'
#' @export
none_of <- function(list.schema) UseMethod("none_of")

#' None hold
#'
#' @param list.schema A [Schema()] of type "\[Predicate\]".
#' @returns A [Schema()] of type "Predicate"
#'
#' @details
#' The outcome is `TRUE` if and only if none of the predicates
#' in the list returns `TRUE`.
#'
#' @export
none_of.Schema <- function(list.schema) {
  predicate.list.schema <- list.schema
  # TODO: Adapt to use flatten_predicates, like p_all_of does.
  make_schema_alias("p_none_of", predicate.list.schema)
}

#' @describeIn none_of.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.p_none_of <- function(x, ...) {
  at_most_k(k = 0, x$predicate.list.schema)
}

### Quantifier aliases

#' For at least k endpoints
#'
#' @details
#' There are two meanings of this keyword:
#'
#' - [for_at_least_k_endpoints.Region]: When defining effect regions.
#' - [for_at_least_k_endpoints.Schema]: When defining predicates and policies.
#'
#' @param k Number of endpoints for which the schema should hold.
#' @param schema Schema that must hold for at least `k` endpoints.
#'
#' @export
for_at_least_k_endpoints <- function(k, schema) {
  UseMethod("for_at_least_k_endpoints", schema)
}

#' Predicate holds for at least k endpoints
#'
#' @param k An integer constant
#' @param schema A predicate [Schema()] meant to be applied to a single endpoint.
#' @returns A predicate [Schema()] which is `TRUE` if `schema` holds for `k` or more of the endpoints.
#'
#' @details
#' Note that keywords such as \code{\link{\%at_domain\%}} or [at_each_domain.Schema] may restrict
#' the endpoints to over which this predicate quantifies to those in a single domain.
#' @export
for_at_least_k_endpoints.Schema <- function(k, schema) {
  make_schema_alias("for_at_least_k_endpoints", k, predicate.schema = schema)
}

#' @describeIn for_at_least_k_endpoints.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.for_at_least_k_endpoints <- function(x, ...) {
  at_least_k(x$k, at_each_endpoint(x$predicate.schema))
}

#' For at least 1 endpoints
#'
#' @param schema Schema that should be interpreted at all endpoints.
#'
#' @details
#' There are two meanings of this keyword:
#'
#' - [for_at_least_1_endpoint.Region]: When defining effect regions.
#' - [for_at_least_1_endpoint.Schema]: When defining predicates and policies.
#'
#' @export
for_at_least_1_endpoint <- function(schema) {
  UseMethod("for_at_least_1_endpoint", schema)
}

#' Predicate holds for at least 1 endpoints
#'
#' @param schema A predicate [Schema()] meant to be applied to a single endpoint.
#' @returns A predicate [Schema()] which is `TRUE` if `schema` holds for any of the endpoints.
#'
#' @details
#' Note that keywords such as \code{\link{\%at_domain\%}} or [at_each_domain.Schema] may restrict
#' the endpoints to over which this predicate quantifies to those in a single domain.
#'
#' @export
for_at_least_1_endpoint.Schema <- function(schema) {
  make_schema_alias("for_at_least_1_endpoint", predicate.schema = schema)
}

#' @describeIn for_at_least_1_endpoint.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.for_at_least_1_endpoint <- function(x, ...) {
  at_least_1(at_each_endpoint(x$predicate.schema))
}

#' For all endpoints
#'
#' @details
#' There are two meanings of this keyword:
#'
#' - [for_all_endpoints.Region()]: When defining effect regions.
#' - [for_all_endpoints.Schema()]: When defining predicates and policies.
#'
#' @param x The schema that must hold at all endpoints.
#'
#' @export
for_all_endpoints <- function(x) UseMethod("for_all_endpoints", x)

#' Predicate holds for all endpoints
#'
#' @param x A predicate [Schema()] meant to be applied to a single endpoint.
#' @returns A predicate [Schema()] which is `TRUE` if `x` holds for all of the endpoints.
#'
#' @details
#' Note that keywords such as \code{\link{\%at_domain\%}} or [at_each_domain.Schema] may restrict
#' the endpoints to over which this predicate quantifies to those in a single domain.
#' @export
for_all_endpoints.Schema <- function(x) {
  make_schema_alias("for_all_endpoints", predicate.schema = x)
}

#' @describeIn for_all_endpoints.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.for_all_endpoints <- function(x, ...) {
  p_all(at_each_endpoint(x$predicate.schema))
}

## Quantification over domains

#' Predicate holds for at least k domains
#'
#' @details
#' There are two meanings of this keyword:
#'
#' -  for_at_least_k_domains.Region : When defining effect regions (TODO).
#' - [for_at_least_k_domains.Schema()]: When defining predicates and policies.
#'
#' @inheritParams for_at_least_k_domains.Schema
#' @export
for_at_least_k_domains <- function(k, schema) UseMethod("for_at_least_k_domains", schema)

#' Predicate holds for at least k domains
#'
#' @param k An integer constant
#' @param schema A predicate [Schema()] meant to be applied to a single domain.
#' @returns A predicate [Schema()] which is `TRUE` if `x` holds for `k` or more of the domains.
#'
#' @export
for_at_least_k_domains.Schema <- function(k, schema) {
  make_schema_alias("for_at_least_k_domains", k, predicate.schema = schema)
}

#' @describeIn for_at_least_k_domains.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.for_at_least_k_domains <- function(x, ...) {
  at_least_k(x$k, at_each_domain(x$predicate.schema))
}

#' For at least one domain
#'
#' @details
#' There are two meanings of this keyword:
#'
#' -  for_at_least_1_domain.Region: When defining effect regions (TODO)
#' - [for_at_least_1_domain.Schema()]: When defining predicates and policies.
#'
#' @inheritParams for_at_least_1_domain.Schema
#' @export
for_at_least_1_domain <- function(schema) UseMethod("for_at_least_1_domain", schema)


#' Predicate holds for at least 1 domain
#'
#' @param schema A predicate [Schema()] meant to be applied to a single domain.
#' @returns A predicate [Schema()] which is `TRUE` if `x` holds for any of the domains.
#'
#' @export
for_at_least_1_domain.Schema <- function(schema) {
  make_schema_alias("for_at_least_1_domain", predicate.schema = schema)
}

#' @describeIn for_at_least_1_domain.Schema Implementation
#' @inheritParams dealias
#' @export
dealias.for_at_least_1_domain <- function(x, ...) {
  at_least_1(at_each_domain(x$predicate.schema))
}

#' For all domains
#'
#' @details
#' There are two meanings of this keyword:
#'
#' -  for_all_domains.Region : When defining effect regions. (TODO)
#' - [for_all_domains.Schema()]: When defining predicates and policies.
#'
#' @inheritParams for_all_domains.Schema
#'
#' @export
for_all_domains <- function(x) UseMethod("for_all_domains", x)

#' For all domains
#'
#' @param x A predicate [Schema()] meant to be applied to a single domain
#' @returns A predicate [Schema()] which is `TRUE` if `x` holds for all of the domains.
#'
#' @export
for_all_domains.Schema <- function(x) {
  make_schema_alias("for_all_domains", predicate.schema = x)
}

#' @describeIn for_all_domains.Schema The predicate [Schema()]
#'   `for_all_domains(x)` is syntactic sugar for
#'   `p_all(at_each_domain(x))`.
#' @inheritParams dealias
#' @export
dealias.for_all_domains <- function(x, ...) {
  p_all(at_each_domain(x$predicate.schema))
}

#' Applies a policy to a specific set of endpoints
#'
#' @param schema A policy schema.
#' @param endpoints An [Endpoints()] object.
#' @returns A [list()] with the following objects:
#'
#' | Name | Type | Description |
#' |------|------|-------------|
#' | type | character | The type of the [Schema] |
#' | criteria.info | tibble | The names of the criteria produced by the Schema |
#' | criteria.fun | tibble | A function taking `summaries` and returning the corresponding criteria |
#' | params.info | tibble | The names of the parameters taken by the policy |
#' | outcome.fun | tibble | A function taking `params` and `criteria` and producing an outcome |
#'
#' @details
#' The objects in the result correspond to the
#' methods of [Schema()] ([schema_type], [criteria_info], [criteria_fun],
#' [params_info], [outcome_fun]), with the
#' `schema` and eventually `endpoints` arguments pre-applied.
#'
#' @export
instantiate_at <- function(schema, endpoints) {
  list(
    type = schema_type(schema),
    criteria.info = criteria_info(schema, endpoints),
    criteria.fun = function(summaries) {
      criteria_fun(schema, endpoints, summaries)
    },
    params.info = params_info(schema, endpoints),
    outcome.fun = function(params, criteria) {
      outcome_fun(schema, endpoints, params, criteria)
    }
  )
}

#' @describeIn instantiate_at Alias for [instantiate_at].
#' @export
applied_at <- instantiate_at

#' Wrap single-summary criteria function
#'
#' @param FUN A function taking three parameters: `x`, `endpoints` and `summary`,
#'   the last of which is of type [Summary()],
#'   and returning a data frame.
#' @returns A function taking `x`, `endpoints` and `summaries`,
#'   and returning the concatenation of the results of applying `FUN`
#'   to each summary in `summaries`;
#'   following the specification of [criteria_fun()].
#'
#' @details
#' [criteria_fun()] is supposed to work on different types
#'  for the third argument
#'  (a [Summary()], a [Summaries()] or a [list()] of Summary),
#'  including the empty cases. This function simplifies
#'  producing a compliant implementation of `criteria_fun`.
#'
#' @seealso
#'  If `FUN` can be implemented vectorized on a [Summaries()]
#'  object, consider using [wrap_vectorized_criteria_fun()] instead.
vectorize_criteria_fun <- function(FUN) {
  function(x, endpoints, summaries) {
    summaries.list <- make_summaries(summaries)$summaries.list
    assertthat::assert_that(!is.null(summaries.list))
    if (length(summaries.list) == 0) {
      tibble::as_tibble(sapply(criteria_info(x, endpoints)$name,
        simplify = FALSE,
        function(x) numeric()
      ))
    } else {
      bind_rows(lapply(summaries.list, function(summary) {
        FUN(x, endpoints = endpoints, summary = summary)
      }))
    }
  }
}

#' Wrap vectorized criteria function
#'
#' @param FUN A function taking three parameters: `x`, `endpoints` and `summaries`,
#'   the last of which is of type [Summaries()],
#'   and returning a data frame.
#' @returns A function taking `x`, `endpoints` and `summaries`,
#'   following the specification of [criteria_fun()].
#'
#' @details
#' [criteria_fun()] is supposed to work on different types
#'  for the third argument
#'  (a [Summary()], a [Summaries()] or a [list()] of Summary),
#'  including the empty cases. This function simplifies
#'  producing a compliant implementation of `criteria_fun`.
wrap_vectorized_criteria_fun <- function(FUN) {
  function(x, endpoints, summaries) {
    summaries.list <- make_summaries(summaries)$summaries.list
    assertthat::assert_that(!is.null(summaries.list))
    if (length(summaries.list) == 0) {
      tibble::as_tibble(sapply(criteria_info(x, endpoints)$name,
        simplify = FALSE,
        function(x) numeric()
      ))
    } else {
      FUN(x, endpoints = endpoints, summaries = summaries.list)
    }
  }
}

#' See [at_params.Schema()].
#' @inheritParams at_params.Schema
#' @export
at_params <- function(x, ...) {
  UseMethod("at_params")
}

#' Instantiates a schema to a specific set of parameters
#'
#' These parameters are removed from the ones required by the schema.
#'
#' @param .params List of parameters
#' @param ... More parameters.
#'
#' @returns A schema of the same type as `x` with the param values set
#'   to
#'
#' @examples
#' p_lalonde |> at_params(p.FS = 0.1, p.FG = 0.2)
#' @export
at_params.Schema <- function(x, .params = NULL, ...) {
  make_schema("at_params", schema = x, params = c(.params, list(...)))
}

#' @describeIn at_params.Schema The type is the same as the type of `x`.
#' @inheritParams schema_type
#' @export
schema_type.at_params <- function(x, ...) schema_type(x$schema, ...)

#' @describeIn at_params.Schema The criteria are the same as the criteria of `x`.
#' @inheritParams criteria_info
#' @export
criteria_info.at_params <- function(x, ...) criteria_info(x$schema, ...)

#' @describeIn at_params.Schema The criteria are the same as the criteria of `x`.
#' @inheritParams criteria_fun
#' @export
criteria_fun.at_params <- function(x, ...) criteria_fun(x$schema, ...)

#' @describeIn at_params.Schema The parameters are those of `x` minus the
#'  ones mentioned in `.params` and `...`.
#' @inheritParams params_info
#' @export
params_info.at_params <- function(x, endpoints) {
  old <- params_info(x$schema, endpoints)
  new <- names(x$params)
  assertthat::assert_that(all(new %in% old$name))
  old %>% filter(!(.data$name %in% !!new))
}

#' @describeIn at_params.Schema The outcome is of the same type
#'   as that of `x`.
#' @inheritParams outcome_fun
#' @export
outcome_fun.at_params <- function(x, endpoints, params, criteria) {
  # Prepend, so that the overrided params have priority
  outcome_fun(x$schema, endpoints, params = c(x$params, params), criteria)
}

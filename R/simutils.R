#' @import tibble dplyr ggplot2
NULL

# Utilities to run batch simulations with caching
# (this applies to any long-running process)
#
# Simulations live in separate, intuitively-named files.
# The files are self-contained and can be tested as part of
# and can thus be tested as part of the usual testing suite.

#' Run an experiment with caching
#'
#'
#' @param experiment Experiment identifier
#' @param experiment.function Closure containing the experiment
#'   (note that we do not check changes in the environment of the function;
#'    these will require force.rerun)
#' @param force.rerun Whether to rerun the function even if an experiment file exists.
#' @param cache.result Whether to save the result of the experiment to an experiment file so that
#'   it is not run when the function is called again.
#' @param experiment.dir Directory in which to save/read from the results of the experiment.
#' @param quick.test Function taking the `experiment.function` as the first argument.
#'   (`function (f) { ... }`). The function `quick.test` is always run, even if an
#'   experiment file exists. This can be used to check that the code for the `experiment.function` is
#'   valid before running the larger experiment, and to check that it is still valid
#'   before loading a pre-existing experiment file.
#' @param depends.on Additional data that should force a re-run when changed.
#' @param random.seed Random seed for the experiment
#'
#' |   | Description |
#' |------|-------------|
#' | "name" | Derive random seed from the experiment name |
#' | NULL   | Do not set random seed |
#' | numeric(1) | Use given seed |
#'
#' @param depend.on.function Whether the experiment should be rerun if the code of
#'  `experiment.function` changes.
#'   The default is TRUE, but FALSE is recommended; `depends.on` and `force.rerun`
#'   should be used instead.
#' @param ... Arguments that will be passed to `experiment.function`, and which
#'   will force a re-run if changed. They will also be included in the filename.
#'
#' @details
#'
#' The result of the experiment (together with some metadata) is stored in
#' `{experiment.dir}/{experiment}_{...}.Rds`. Subsequent calls with
#' the same arguments will just reload the experiment from the file.
#'
#' @examples
#' options(gonogo.experiment.dir = tempdir())
#' run_experiment_inline(
#'   "sim_20220203_1354",
#'   function(N) {
#'     Sys.sleep(3)
#'     sum(seq(N))
#'   },
#'   N = 100,
#'   depend.on.function = FALSE
#' )
#' # 5050 (after 3 seconds)
#'
#' run_experiment_inline(
#'   "sim_20220203_1354",
#'   function(N) {
#'     Sys.sleep(3)
#'     sum(seq(N))
#'   },
#'   N = 100,
#'   depend.on.function = FALSE
#' )
#' # [message]: Loading from /tmp/sim_20220203_1354_N=100_3694102a11370d5d9348d75c3f8d201a9bebd820.Rds
#' # 5050 (instantly)
#' @export
run_experiment_inline <- function(experiment,
                                  experiment.function,
                                  force.rerun = FALSE,
                                  cache.result = TRUE,
                                  experiment.dir = getOption("gonogo.experiment.dir",
                                    default = "_cache"
                                  ),
                                  quick.test = NULL,
                                  depends.on = NULL,
                                  random.seed = "name",
                                  depend.on.function = TRUE,
                                  ...) {
  if (!dir.exists(experiment.dir)) {
    if (dir.create(experiment.dir)) {
      cli::cli_warn("{.var experiment.dir} does not exist and was created")
    } else {
      cli::cli_abort("{.var experiment.dir} does not exist and could not be created.")
    }
  }

  # Load code
  assertthat::assert_that(is.function(experiment.function))
  f <- experiment.function
  if (is.null(depends.on)) {
    f.checksum <- digest::sha1(if (depend.on.function) f else experiment)
  } else {
    f.checksum <- digest::sha1(list(f = if (depend.on.function) f else experiment, depends.on = depends.on))
  }
  args <- list(...)
  # Add a suffix based on the arguments
  args.slug <- paste(names(args), unname(args), sep = "=", collapse = ",")
  if (args.slug != "") {
    args.slug <- paste0("_", args.slug)
  }
  args.slug <- paste0(args.slug, "_", f.checksum)
  cache.fname <- paste0(experiment.dir, "/", experiment, args.slug, ".Rds")
  # Running quick test
  set_seed <- function() {
    if (!is.null(random.seed)) {
      if (random.seed == "name") {
        set.seed(digest::digest2int(experiment))
      } else {
        assertthat::assert_that(is.scalar(random.seed))
        set.seed(random.seed)
      }
    }
  }
  set_seed()
  if (!is.null(quick.test)) {
    quick.test(f)
  }
  if (!force.rerun && file.exists(cache.fname)) {
    message(paste0("Loading from ", cache.fname))
    obj <- tryCatch(readRDS(cache.fname), error = function(cond) {
      message(paste0(cache.fname, " -- ", cond))
      NULL
    })
    if (!is.null(obj)) {
      # Check that the cached copy matches the parameters
      stopifnot(obj$code.checksum == f.checksum)
      stopifnot(identical(obj$args, args))
      return(obj$result)
    }
  }

  set_seed()
  result <- f(...)
  if (cache.result) {
    saveRDS(tibble::lst(
      args,
      date = date(),
      code.checksum = f.checksum,
      code = deparse(f),
      result
    ), cache.fname)
  }
  result
}

#' Run simulations in parallel (deprecated)
#'
#' See [run_montecarlo_criteria()].
#'
#' @param trial.params Dataframe describing the various trial designs to simulate.
#' @param fun.trial Function mapping a row of `trial.params` to a [Trial()].
#' @param policy.params Dataframe describing the various policies to simulate.
#' @param fun.policy Function mapping a row of `policy.params` to a policy [Schema()].
#' @param n.patients.per.group Number of patients per group (in case its not given in the trial outputed by
#'          `fun.trial`).
#' @param effects Named list of effects.
#' @inheritParams make_policy_set
#' @param cores Number of cores to use.
#' @param random Whether to sample the observed effect, or take the mode.
#' @param N How many trials to simulate.
#' @param aggregate.decisions Whether to summarize the decision data.
#' @param fun.trial.extra Extra information to include with the simulation results.
#' @param summaries Data frame containing information on how to summarize the observations
#'   (currently on how to estimate the covariance)
#' @param trial.splits If larger than 1, how many cores to split simulations for a single
#'   trial across.
#' @param verbose Whether to output debugging information.
#'
#' @details
#' The data frames must not contain columns named `.load`, `.N`, `.trial.id` or `.worker`.
#' If `policies` is given, the data frame must not contain a column named `policy.name`.
#' If `effects` is given, the data frame must not contain a column named `effect.name`.
#'
#'
#' @export
run_montecarlo <- function(trial.params = tibble::tibble_row(),
                           fun.trial,
                           policy.params = NULL,
                           fun.policy = NULL,
                           n.patients.per.group = NULL,
                           effects = NULL,
                           policies = NULL,
                           policy = NULL,
                           cores = 1,
                           random = TRUE,
                           N = if (random) 100 else 1,
                           aggregate.decisions =
                             (N > 1) &&
                               (!is.null(policies) ||
                                 !is.null(policy) ||
                                 !is.null(policy.params)),
                           verbose = FALSE,
                           fun.trial.extra = function(.params, trial) NULL,
                           summaries = NULL,
                           trial.splits = 1) {
  if (!is.null(n.patients.per.group)) {
    # We need to create a new scope so that the value of function.trial.old
    # is not overwritten
    trial.params <- trial.params %>%
      inner_join(tibble(n.patients.per.group = n.patients.per.group),
        by = character()
      )
    (function() {
      fun.trial.old <- fun.trial
      fun.trial <<- function(row) {
        fun.trial.old(row) %>% with_n_patients(per.group = row$n.patients.per.group)
      }
    })()
  }
  if (!is.null(effects)) {
    trial.params <- trial.params %>%
      inner_join(tibble(effect.name = names(effects)), by = character())
    # We need to create a new scope so that the value of function.trial.old
    # is not overwritten
    (function() {
      fun.trial.old <- fun.trial
      fun.trial <<- function(row) {
        fun.trial.old(row) %>% with_ground_truth(vars.effect = {
          eff.f <- effects[[row$effect.name]]
          if (is.function(eff.f)) {
            eff.f(row)
          } else {
            eff.f
          }
        })
      }
    })()
  }


  tasks <- trial.params %>%
    rowwise() %>%
    mutate(
      .N = N,
      .trial.id = cur_group_id(),
      .load = 1
    ) %>%
    mutate(.N = list(divide_into_summands(N, trial.splits))) %>%
    tidyr::unnest(.data$.N) %>%
    ungroup()
  assertthat::assert_that(!(trial.splits > 1 && aggregate.decisions),
    msg = "splitting trials and aggregating decisions is not supported"
  )

  # Allocate tasks to cores
  if (cores > nrow(tasks)) {
    # TODO: Divide some tasks to make more work
    warning("There are more cores than tasks")
  }
  tasks <- tasks %>% arrange(-.data$.load, .data$.trial.id)
  loads <- tasks$.load
  worker <- NULL
  for (i in seq(cores)) {
    load.per.worker <- sum(loads) / (cores - i + 1)
    loads.cummul <- cumsum(loads)
    n.tasks <- max(1, sum(loads.cummul <= load.per.worker))
    worker <- c(worker, rep(i, length.out = n.tasks))
    if (n.tasks == length(loads)) {
      break
    }
    loads <- loads[seq(n.tasks + 1, length(loads))]
  }
  tasks$.worker <- worker
  tasks <- tasks %>% arrange(.data$.trial.id)

  if ((!random && (N > 1 || aggregate.decisions)) ||
    (N == 1 && aggregate.decisions)) {
    warning("Combination of settings for N, aggregate and random may be non-sensical")
  }
  # Build policy list, if applicable
  policy.set <- make_policy_set(
    policy.params = policy.params,
    fun.policy = fun.policy,
    policies = policies,
    policy = policy
  )
  list2env(policy.set, environment())
  all.policies <- all_policies(policy.set)

  # Hack to allow set.seed from the outside
  # (changing random generators resets the seed)
  if (exists(".Random.seed")) {
    seed <- .Random.seed
    RNGkind("L'Ecuyer-CMRG")
    set.seed(digest::digest2int(paste(seed, collapse = " ")))
  } else {
    RNGkind("L'Ecuyer-CMRG")
  }
  # TODO: Write this in a way that is cross-platform
  # Needed to avoid interference with the parallelization
  RhpcBLASctl::blas_set_num_threads(1)
  start.time <- Sys.time()
  if (verbose) system(paste("echo Launching workers"))
  res <- tasks %>%
    dplyr::group_split(.data$.worker) %>%
    parallel::mclapply(
      X = .,
      FUN = function(split) {
        if (verbose) system(paste0("echo -n [", unique(split$.worker), ".a\u00B7", nrow(split), "]"))
        res <- split %>%
          rowwise() %>%
          summarise(cur_data() %>% bind_cols({
            cur.trial.params <- cur_data()
            trial <- fun.trial(cur.trial.params)
            res.a <- lapply(seq(.data$.N), function(.n) {
              study <- (if (random) {
                run_simulated_study
              } else {
                run_exact_study
              })(trial)
              # Add summary statistics
              studies <-
                if (!is.null(summaries)) {
                  assertthat::has_name(summaries, "covariance")
                  assertthat::assert_that(!("study" %in% names(summaries)))
                  summaries %>%
                    rowwise() %>%
                    mutate(study = study %>%
                      with_summary(covariance = .data$covariance) %>%
                      scrub() %>% list())
                } else {
                  tibble::tibble_row(study = study %>% scrub() %>% list())
                }
              # Add decision criteria
              studies <- studies %>%
                rowwise() %>%
                mutate(
                  study =
                    list(study)
                ) %>%
                ungroup()
              # Apply policies to decision criteria
              if (!is.null(all.policies) && nrow(all.policies) > 0) {
                # study <- study %>% with_decision_criteria(criteria)
                studies <-
                  studies %>%
                  inner_join(all.policies, by = character()) %>%
                  rowwise() %>%
                  mutate(
                    decision = policy(study, only.decision = TRUE),
                    policy = NULL
                  )
              }
              # Include criteria information, remove study information
              studies <- studies %>%
                rowwise() %>%
                summarise(cur_data() %>% select(-.data$study) %>% bind_cols({
                  tibble::tibble_row()
                })) %>%
                ungroup()
              studies
            }) %>% dplyr::bind_rows()
            if (aggregate.decisions) {
              res.a <- res.a %>%
                group_by(
                  .data$.policy.id,
                  across(names(policy.params))
                ) %>%
                summarise(summarise_decision(.data$decision)) %>%
                ungroup()
            }
            res.a <- bind_cols(fun.trial.extra(cur_data(), trial), res.a)
            res.a
          }))
        if (verbose) system(paste0("echo -n [", unique(split$.worker), ".b\u00B7", nrow(split), "]"))
        res
      },
      mc.cores = cores,
      mc.set.seed = TRUE
    )

  if (verbose) system(paste("echo "))
  if (verbose) cat(paste0(difftime(Sys.time(), start.time, units = "secs"), " s\n"))
  if (verbose) system(paste("echo Combining results..."))
  res <- res %>% dplyr::bind_rows()
  if (verbose) cat(paste0(difftime(Sys.time(), start.time, units = "secs"), " s\n"))
  res
}

#' Apply a policy on a grid of observed true effects (deprecated)
#'
#' Do not use directly; use via [run_montecarlo_grid_schema()]
#'
#' @inheritParams run_montecarlo_grid_schema
#' @param a.priori Whether to add random variation to each element of the grid.
#' @param ... Other arguments; passed through to [run_montecarlo()].
#'
#' @export
run_montecarlo_grid <- function(base.trial, x, y, a.priori = TRUE, ...) {
  df <- run_montecarlo(
    trial.params = tidyr::expand_grid(x = x, y = y),
    fun.trial = function(.params) {
      base.trial %>% with_ground_truth(c(.params$x, .params$y))
    },
    random = !a.priori,
    N = 1,
    aggregate.decisions = FALSE,
    ...
  )
  ll <- lalonde_per_var(base.trial)
  list(
    df = df,
    step.x = median(diff(x)),
    step.y = median(diff(y)),
    var.x = base.trial$endpoints$vars[1, ],
    var.y = base.trial$endpoints$vars[2, ],
    ll.x = ll$vars[1, ],
    ll.y = ll$vars[2, ]
  )
}

PolicySet <- function(policy.params,
                      fun.policy) {
  structure(tibble::lst(
    policy.params,
    fun.policy
  ),
  class = "PolicySet"
  )
}

all_policies <- function(policy.set) {
  if (!is.null(policy.set$fun.policy)) {
    all.policies <- policy.set$policy.params %>%
      rowwise() %>%
      mutate(
        .policy.id = cur_group_id(),
        policy = policy.set$fun.policy(cur_data()) %>% list()
      ) %>%
      ungroup()
  } else {
    all.policies <- NULL
  }
  all.policies
}

#' Apply a schema over a grid
#'
#' @param base.trial A [Trial()], including the endpoints and the number of patients per arm.
#' @param schema A [Schema()] of type "Decision", representing a policy.
#' @param params Parameters to se for the policy (e.g. probabilities of False Go and False Stop)
#' @param x Equally-spaced sequence of observed effects to use for the first endpoint.
#' @param y Equally-spaced sequence of observed effects to use for the second endpoint.
#' @param resolution Size of the increments on the x and y axes (if derived automatically)
#' @param ... Other arguments (passed through to [run_montecarlo_grid()]).
#' @returns A list that can be passed as the first argument of [plot_lalonde_grid()].
#'
#' @details
#'   The `base.trial` should only have two endpoints. The true effect (if any) is ignored.
#'
#' @export
run_montecarlo_grid_schema <- function(base.trial, schema, params, x = NULL, y = NULL, resolution = NULL, ...) {
  if (is.null(x) || is.null(y)) {
    assertthat::assert_that(!is.null(resolution))
    tv <- parse_effect(base.trial, "tv")
    lrv <- parse_effect(base.trial, "lrv")
    if (is.null(x)) {
      assertthat::assert_that(!is.null(resolution))
      x <- seq(-0.5 * lrv[1], 1.5 * tv[1], by = resolution)
    }
    if (is.null(y)) {
      assertthat::assert_that(!is.null(resolution))
      y <- seq(-0.5 * lrv[2], 1.5 * tv[2], by = resolution)
    }
  } else {
    assertthat::assert_that(is.null(resolution))
  }

  run_montecarlo_grid(
    base.trial = base.trial,
    x = x,
    y = y,
    policy = function(study, only.decision = TRUE) {
      outcome_fun(
        schema, base.trial$endpoints, params,
        criteria_fun(schema, base.trial$endpoints, study$summary)
      )$decision
    },
    ...
  )
}

#' (Deprecated)
#'
#' Use functions in the `evaluate.R` file.
#'
#' @param policy.params Data frame describing the policies
#' @param fun.policy Function that gives a policy for a row in `policy.params`
#' @param policies List of policies
#' @param policy A single policy
make_policy_set <- function(policy.params = NULL,
                            fun.policy = NULL,
                            policies = NULL,
                            policy = NULL) {
  if (!is.null(policy)) {
    assertthat::assert_that(is.null(policies))
    assertthat::assert_that(is.null(policy.params))
    assertthat::assert_that(is.null(fun.policy))
    policy.params <- tibble::tibble_row()
    fun.policy <- function(row) policy
  }
  if (!is.null(policies)) {
    assertthat::assert_that(is.null(fun.policy))
    assertthat::assert_that(is.null(policy.params))
    policy.params <- tibble(policy.name = names(policies))
    fun.policy <- function(row) {
      policies[[row$policy.name]]
    }
  }
  PolicySet(
    policy.params = policy.params,
    fun.policy = fun.policy
  )
}


#' Computing policy criteria from trial simulations
#'
#'
#' @param trial.params Data frame with T rows, one for
#'   each trial design.
#' @param fun.trial Function taking a single argument
#'   consisting of one row of the data frame `trial.params`,
#'   and returning a [Trial()].
#'   These will be the trial designs simulated.
#' @param policy.params A data frame used to specify policies.
#'   Contains P rows, one for each policy.
#' @param fun.policy Function taking an argument
#'   consisting of one row of the data frame `policy.params`,
#'   and returning a [Schema()] of type "Decision".
#'   These will be the policies for which the criteria will be computed.
#' @param effects If not `NULL`, a named list of true effect vectors with
#'   E elements. See [parse_effect_vector()] for the syntax to use
#'   for true effects.
#' @param policy A single policy schema (given instead of `policy.params` and
#'   `fun.policy`.
#' @param cores Number of cores to use for the simulation.
#' @param random.mean Whether to simulate the observed effect randomly
#'   (`TRUE`) or use the true effect as the observed effect (FALSE).
#'   Useful f.ex. when plotting the go/discuss/stop regions of
#'   a 2 endpoint policy.
#' @param covariance.structure
#'   Whether to simulate the observed covariance matrix from
#'   its random distribution ("unstructured"), or use the true covariance
#'   matrix ("theoretical").
#' @param N Number of studies to simulate for each combination of trial
#'   design and policy.
#' @param verbose Whether to output information to the console regarding
#'   the execution of the simulation.
#' @param fun.trial.extra Returns additional information about the trial
#'   that will be included verbatim in the output data frame.
#' @param trial.splits Number of cores among which the N trials should split.
#'   (increase if there is a warning that there are more cores than tasks).
#' @param simulate.fast Simulates only the mean vector and the diagonal
#'   of the observed covariance matrix (skips simulating the underlying data,
#'   and the off-diagonal elements of the covariance matrix).
#'   Note that this means that policies that rely on the whole covariance
#'   matrix will not run.
#'
#' @returns
#'
#' A data frame, containing:
#'  - All the columns in `trial.params`.
#'  - All the columns in `policy.params`.
#'  - If `!is.null(effects)`, an `effect.name` column,
#'    containing one of the names in `names(effects)`.
#'  - All the columns returned by `fun.trial.extra`.
#'  - One column for each criterion in the policy.
#'
#' A total of `P×T×E` rows.
#'
#' @details
#'
#' The data frames given as `trial.params`
#' and `policy.params` must not contain columns named `.load`, `.N`, `.trial.id`, `.worker`
#' or `policy`.
#' If `effects` is given, the data frame must not contain a column named `effect.name`.
#'
#' For more information about `random.mean`,
#' `covariance.structure`, and `simulate.fast`, see run_study_summaries.
#'
#' @examples
#' run_montecarlo_criteria(
#'   trial.params = data.frame(n.per.group = c(20, 40)),
#'   fun.trial = function(.params) {
#'     make_endpoints(n.domains = 1, domain.n.vars = 1) |>
#'       with_n_patients(per.group = .params$n.per.group)
#'   },
#'   effects = list(
#'     all.tv = "tv",
#'     all.lrv = "lrv",
#'     all.0 = 0
#'   ),
#'   policy = p_lalonde,
#'   N = 2,
#'   simulate.fast = TRUE
#' ) |> dplyr::select(!starts_with("."))
#' # A tibble: 12 × 4
#' #    n.per.group effect.name stop.if.stop.p go.if.go.p
#' #          <dbl> <chr>                <dbl>      <dbl>
#' #  1          20 all.tv          0.884        0.00700
#' #  2          20 all.tv          0.985        0.000164
#' #  3          20 all.lrv         0.368        0.121
#' #  4          20 all.lrv         0.00942      0.805
#' #  5          20 all.0           0.00798      0.767
#' #  6          20 all.0           0.0596       0.500
#' #  7          40 all.tv          0.148        0.104
#' #  8          40 all.tv          0.0702       0.109
#' #  9          40 all.lrv         0.106        0.175
#' # 10          40 all.lrv         0.0233       0.367
#' # 11          40 all.0           0.000126     0.947
#' # 12          40 all.0           0.00000987   0.982
#' @export
run_montecarlo_criteria <-
  function(trial.params = tibble::tibble_row(),
           fun.trial,
           policy.params = NULL,
           fun.policy = NULL,
           effects = NULL,
           # policies = NULL,
           policy = NULL,
           cores = 1,
           random.mean = TRUE,
           covariance.structure = if (random.mean) {
             "unstructured"
           } else {
             "theoretical"
           },
           N = if (random.mean) 100 else 1,
           verbose = FALSE,
           fun.trial.extra = function(.params, trial) NULL,
           # summaries = NULL,
           trial.splits = 1,
           simulate.fast = FALSE) {

    # Check that the covariance structure matches one of the supported types
    covariance.structure <- match.arg(covariance.structure, names(COVARIANCE.STRUCTURE))

    if (!is.null(effects)) {
      trial.params <- trial.params %>%
        inner_join(tibble(effect.name = names(effects)), by = character())
      # We need to create a new scope so that the value of function.trial.old
      # is not overwritten
      (function() {
        fun.trial.old <- fun.trial
        fun.trial <<- function(row) {
          fun.trial.old(row) %>% with_ground_truth(vars.effect = {
            eff.f <- effects[[row$effect.name]]
            if (is.function(eff.f)) {
              eff.f(row)
            } else {
              eff.f
            }
          })
        }
      })()
    }

    tasks <- trial.params %>%
      rowwise() %>%
      mutate(
        .N = N,
        .trial.id = cur_group_id(),
        .load = 1
      ) %>%
      mutate(.N = list(divide_into_summands(N, trial.splits))) %>%
      tidyr::unnest(.data$.N) %>%
      ungroup()

    # Allocate tasks to cores, trying to make so that
    # the number of tasks per core is balanced.
    if (cores > nrow(tasks)) {
      # TODO: Divide some tasks to make more work
      warning("There are more cores than tasks")
    }
    tasks <- tasks %>% arrange(-.data$.load, .data$.trial.id)
    loads <- tasks$.load
    worker <- NULL
    for (i in seq(cores)) {
      load.per.worker <- sum(loads) / (cores - i + 1)
      loads.cummul <- cumsum(loads)
      n.tasks <- max(1, sum(loads.cummul <= load.per.worker))
      worker <- c(worker, rep(i, length.out = n.tasks))
      if (n.tasks == length(loads)) {
        break
      }
      loads <- loads[seq(n.tasks + 1, length(loads))]
    }
    tasks$.worker <- worker
    tasks <- tasks %>% arrange(.data$.trial.id)

    if (!random.mean & N > 1) {
      warning("Combination of N > 1 and non-random mean may be non-sensical")
    }
    if (!random.mean & covariance.structure != COVARIANCE.STRUCTURE$theoretical) {
      warning("Combination of covariance structure and non-random mean may be non-sensical")
    }
    if (!is.null(policy)) {
      assertthat::assert_that(is.null(fun.policy) & is.null(policy.params))
      all.policies <- tibble::tibble_row(policy = policy)
    } else {
      all.policies <-
        policy.params %>%
        rowwise() %>%
        summarise(cur_data(), {
          cur.policy.params <- cur_data()
          tibble::tibble_row(policy = fun.policy(cur.policy.params))
        }) %>%
        ungroup()
    }
    assertthat::assert_that(!is.null(all.policies) & nrow(all.policies) > 0)

    # We change to a random number generator that behaves reproducibly
    # (and well) with multithreading.
    #
    # The following is a hack to preserve the existing seed
    # for the random number generator in some form, as it would otherwise
    # be reset to an unpredictable value.
    if (exists(".Random.seed")) {
      seed <- .Random.seed
      RNGkind("L'Ecuyer-CMRG")
      set.seed(digest::digest2int(paste(seed, collapse = " ")))
    } else {
      RNGkind("L'Ecuyer-CMRG")
    }
    # Runing OpenBLAS with more than one thread interfers with paralelization.
    # TODO: Write this in a way that is cross-platform.
    RhpcBLASctl::blas_set_num_threads(1)
    start.time <- Sys.time()
    if (verbose) system(paste("echo Launching workers"))
    res <- tasks %>%
      dplyr::group_split(.data$.worker) %>%
      parallel::mclapply(
        X = .,
        FUN = function(split) {
          if (verbose) system(paste0("echo -n [", unique(split$.worker), ".a\u00B7", nrow(split), "]"))
          res <- split %>%
            rowwise() %>%
            summarise(cur_data(), {
              cur.trial.params <- cur_data()
              trial <- fun.trial(cur.trial.params)
              # Generate study summaries
              summaries <-
                run_study_summaries(
                  M = .data$.N,
                  trial,
                  random.mean = random.mean,
                  covariance.structure = covariance.structure,
                  fast = simulate.fast
                )
              # Generate data frame with all the criteria
              all.criteria <-
                all.policies %>%
                rowwise() %>%
                summarise(
                  cur_data() %>% mutate(policy = NULL),
                  fun.trial.extra(cur.trial.params, trial),
                  criteria_fun(policy, trial$endpoints, summaries)
                )

              all.criteria
            })
          if (verbose) system(paste0("echo -n [", unique(split$.worker), ".b\u00B7", nrow(split), "]"))
          res
        },
        mc.cores = cores,
        mc.set.seed = TRUE
      )

    if (verbose) system(paste("echo "))
    if (verbose) cat(paste0(difftime(Sys.time(), start.time, units = "secs"), " s\n"))
    if (verbose) system(paste("echo Combining results..."))
    res <- res %>% dplyr::bind_rows()
    if (verbose) cat(paste0(difftime(Sys.time(), start.time, units = "secs"), " s\n"))
    res
  }

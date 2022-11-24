#' @import tibble dplyr ggplot2
#' @importFrom stats median
NULL

#' Compute 2-dimensional Go/Discuss/Stop regions
#'
#' @param x Object to use for study design information.
#' @param ... Other arguments (see [calc_lalonde_grid.Endpoints()]).
#'
#' @seealso calc_lalonde_grid.Endpoints
#'
#' @export
calc_lalonde_grid <- function(x, ...) UseMethod("calc_lalonde_grid")

#' @export
#'
#' @describeIn calc_lalonde_grid [Trial()] The `n.patients` is taken from the [Trial()].
calc_lalonde_grid.Trial <- function(x, ...) {
  trial <- x
  if (has_ground_truth(trial)) {
    warning("The ground truth of the trial will be ignored")
  }
  calc_lalonde_grid(
    endpoints = trial$endpoints,
    n.patients = trial$n.patients,
    ...
  )
}

#' Compute 2-dimensional Go/Discuss/Stop regions.
#'
#' @param x Endpoint information ([Endpoints()])
#' @param n.patients Number of patients ([NPatients()])
#' @param policy A function taking in a study [Summary()],
#'   and producing a [DECISION].
#' @param effect How to build the true effect vector, mentioning "x" and "y".
#'   The length of the vector should coincide with the number of variables in
#'   `endpoints`; one of the positions should be `"x"` and the other `"y"`; the remainder
#'   should be either a number, or an expression supported by [parse_effect_vector()]
#'   (e.g. "tv", "lrv", "2*tv")
#' @param range.x Vector of length 2; maximum and minimum values for the "x" parameter.
#' @param range.y Vector of length 2; maximum and minimum values for the "y" parameter.
#' @param resolution Shifted, base 2 logarithm of the number of pixels.
#'                   `0` corresponds to ~128 pixels in each axis; `-1` to ~64 pixels, etc.
#' @param simulated Whether to simulate a random study with the mean effect corresponding to a pixel,
#'   or to use the true effect as the observed effect directly.
#' @param cores How many cores to use when running the simulation (a high number is recommended).
#' @param ... Other arguments (should be empty)
#'
#' @return
#' A list containing the following fields:
#'
#' |  Name | Type | Description |
#' |-------|------|-------------|
#' |  df   |  tibble    |  A data frame containing the decisions for each pixel |
#' |  step.x | numeric  | Width of a pixel |
#' |  step.y | numeric | Height of a pixel |
#' |  var.x | tibble | (One-row) endpoint info for the x-axis |
#' | var.y | tibble | (One-row) endpoint info for the y-axis |
#' | ll.x  | tibble | (One-row) Lalonde thresholds for the x-axis |
#' | ll.y | tibble | (One-row) Lalonde thresholds for the y-axis |
#'
#'
#' @details
#' The result can be given to [plot_lalonde_grid()] to produce a plot.
#'
#' - The tibble `df` contains the approximately `(2^(7+resolution)+1)^2` rows.
#'   In each row, the positions `x` and `y`, and the decision
#'   for the corresponding effect are given.
#'
#' - For the columns of `var.x` and `var.y`, see [Endpoints()] (`.$vars` field)
#' - For the columns of `ll.x` and `ll.y`, see [LalondePerVar()] (`.$vars` field)
#'
#' @seealso [plot_lalonde_grid()]
#'
#' @export
calc_lalonde_grid.Endpoints <-
  function(x,
           n.patients,
           policy,
           effect = c("x", "y"),
           range.x = NULL,
           range.y = NULL,
           resolution = -1,
           simulated = FALSE,
           cores = 1,
           ...) {
    endpoints <- x
    assertthat::assert_that(length(list(...)) == 0)

    var.x <- endpoints$vars[effect == "x", ]
    if (nrow(var.x) != 1) var.x <- NULL
    var.y <- endpoints$vars[effect == "y", ]
    if (nrow(var.y) != 1) var.y <- NULL

    if (is.null(range.x)) {
      assertthat::assert_that(!is.null(var.x))
      range.x <- c(NA, NA)
      range.x[1] <- var.x$lrv - 0.5 * (var.x$tv - var.x$lrv)
      range.x[2] <- var.x$tv + 0.5 * (var.x$tv - var.x$lrv)
    }

    if (is.null(range.y)) {
      assertthat::assert_that(!is.null(var.y))
      range.y <- c(NA, NA)
      assertthat::assert_that(nrow(var.y) == 1)
      range.y[1] <- var.y$lrv - 0.5 * (var.y$tv - var.y$lrv)
      range.y[2] <- var.y$tv + 0.5 * (var.y$tv - var.y$lrv)
    }


    n.points.x <- n.points.y <- (2^(7 + resolution)) + 1
    step.x <- (range.x[2] - range.x[1]) / (n.points.x - 1)
    step.y <- (range.y[2] - range.y[1]) / (n.points.y - 1)

    values.x <- seq(range.x[1], range.x[2], by = step.x)
    values.y <- seq(range.y[1], range.y[2], by = step.y)

    df <- tibble(x = values.x) %>%
      inner_join(tibble(y = values.y), by = character()) %>%
      rowwise() %>%
      mutate(params = list(list(x = .data$x, y = .data$y)))

    # Test effect vector
    # TODO: Check that the length of the vector
    # coincides with the number of variables.
    parse_effect_vector(
      endpoints = endpoints,
      effect, list(
        x = median(values.x),
        y = median(values.y)
      )
    )

    # Compute decisions
    df$decision <- parallel::mclapply(
      X = df$params,
      FUN = function(params) {
        RhpcBLASctl::blas_set_num_threads(1)
        trial <- make_trial(
          endpoints = endpoints,
          vars.effect =
            parse_effect_vector(
              endpoints = endpoints,
              vars.effect = effect,
              params = params
            ),
          n.patients = n.patients
        )
        study <- if (simulated) {
          run_simulated_study(trial, only.summary = TRUE)
        } else {
          make_exact_study(trial, only.summary = TRUE)
        }
        policy(study)
      },
      mc.cores = cores,
      mc.set.seed = TRUE
    ) %>%
      unlist()

    ll <- lalonde_per_var(make_trial(endpoints, n.patients))
    tibble::lst(df, step.x, step.y,
      var.x,
      var.y,
      ll.x = ll$vars[ll$vars$name == var.x$name, ],
      ll.y = ll$vars[ll$vars$name == var.y$name, ]
    )
  }

#' Plot a grid showing decision regions
#'
#' @param grid Grid produced by [calc_lalonde_grid()]
#' @param thresholds Whether to show decision thresholds as vertical/horizontal
#'   lines.
#' @param fill.alpha Whether to make the decision regions semitransparent
#'   (e.g. 0.4 means 40% opaque).
#'
#' @return A `ggplot2` object.
#'
#' @export
plot_lalonde_grid <- function(grid, thresholds = TRUE, fill.alpha = 1) {
  gg <- ggplot(grid$df) +
    geom_tile(aes(
      x = .data$x, y = .data$y, fill = .data$decision,
      width = grid$step.x, height = grid$step.y
    ), alpha = fill.alpha) +
    scale_fill_manual(name = "Decision", values = decision.colors) +
    geom_point(data = tibble(
      x = c(grid$var.x$lrv, grid$var.x$tv),
      y = c(grid$var.y$lrv, grid$var.y$tv),
      response = c("LRV", "TV")
    ), aes(x = .data$x, y = .data$y, shape = .data$response)) +
    scale_shape_manual(
      name = "Response",
      breaks = names(response.shapes),
      values = unlist(response.shapes)
    ) +
    xlab(grid$ll.x$name) +
    ylab(grid$ll.y$name)
  if (thresholds) {
    gg <- gg +
      scale_linetype_manual(
        name = "Thresholds",
        breaks = names(threshold.linetypes),
        values = unlist(threshold.linetypes)
      ) +
      geom_vline(data = tibble(
        xi = c(grid$ll.x$lrv.threshold, grid$ll.x$stop.threshold),
        lt = c("Go", "Stop")
      ), aes(xintercept = .data$xi, linetype = .data$lt)) +
      geom_hline(data = tibble(
        yi = c(grid$ll.y$lrv.threshold, grid$ll.y$stop.threshold),
        lt = c("Go", "Stop")
      ), aes(yintercept = .data$yi, linetype = .data$lt))
  }
  gg
}

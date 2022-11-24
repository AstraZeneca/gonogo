#' @import tibble dplyr ggplot2
#' @importFrom stats pt qt pnorm qnorm rWishart pchisq pf var
NULL

#' Check whether a value is a scalar.
#'
#' @param x Value
#' @returns `TRUE` if `x` is a scalar; `FALSE` otherwise.
#'
#' @export
is.scalar <- function(x) is.atomic(x) && length(x) == 1

#' List of possible decisions
#'
#' @export
decision.levels <- c("Go", "Discuss", "Stop")

#' List of possible decisions, in lowercase
#'
#' @export
decision.levels.lowercase <- tolower(decision.levels)

#' List of possible decisions as a factor with
#' levels \link{decision.levels}.
#'
#' @examples
#' DECISION$Go
#' DECISION$Discuss
#' DECISION$Stop
#' @export
DECISION <- list(
  Go = factor("Go", decision.levels),
  Discuss = factor("Discuss", decision.levels),
  Stop = factor("Stop", decision.levels)
)

#' Possible covariance matrices for inference
#'
#' - `unstructured`: The covariance matrix is inferred
#'   directly from the data.
#' - `theoretical`: The covariance matrix is a known
#'   constant (alt. the covariance matrix is inferred
#'   from the data, but it is assumed to be a known constant
#'   for the purposes of inference)
#'
#' @export
COVARIANCE.STRUCTURE <- {
  levels <- c("unstructured", "theoretical")
  c <- as.list(factor(levels, levels))
  names(c) <- levels
  c
}

#' AZ colors for decision levels
#'
#' @export
decision.colors <- list(
  Go = "#7fd7a7",
  Discuss = "#ffdf7f",
  Stop = "#ff7f7f"
)

#' `ggplot2` colour scale based on AZ colors (fill aesthetic)
#'
#' @param ... Passed through to [scale_fill_manual()].
#'
#' @export
scale_fill_Decision <- function(...) {
  scale_fill_manual(values = unlist(decision.colors), ...)
}

#' `ggplot2` colour scale based on AZ colors (color aesthetic)
#'
#' @param darken Whether to use darker colors than the standard ones
#' @param ... Passed through to [scale_color_manual()].
#'
#' @export
scale_color_Decision <- function(darken = FALSE, ...) {
  if (darken) {
    scale_color_manual(values = unlist(lapply(decision.colors, function(col) colorspace::darken(col, amount = 0.3))), ...)
  } else {
    scale_color_manual(values = unlist(decision.colors), ...)
  }
}

#' Default `ggplot2` line-types for some true effects: `LRV`, `TV` and `NoEffect` (i.e. 0)
#'
#' @export
response.linetypes <- list(
  NoEffect = "dotted",
  LRV = "twodash",
  TV = "solid"
)

#' Default `ggplot2` line-types for decision thresholds
#'
#' @export
threshold.linetypes <- list(
  Go = "twodash",
  Stop = "solid"
)

#' Default `ggplot2` shapes for some true effects: `LRV` and `TV`
#' @export
response.shapes <- list(
  LRV = "star",
  TV  = "triangle"
)

#' Default values for some true effects of interests
#' @export
responses.default <- list(
  NoEffect = 0,
  LRV      = 0.5,
  TV       = 1
)

#' List of some true effects of interest
response.levels <- names(responses.default)

#' Some true effects of interest as factors:
#' `NoEffect` (i.e. 0), `LRV` and `TV`
#' @export
RESPONSE <- list(
  NoEffect = factor("NoEffect", response.levels),
  LRV = factor("LRV", response.levels),
  TV = factor("TV", response.levels)
)

#' Wrap values for inclusion into a data.frame or tibble.
#'
#' Ensures that a value will take up only one row in a dataframe,
#' by wrapping it into a list (if needed)
#'
#' @param l Value to wrap
#'
#' @return If `l` is an scalar, return `l`, otherwise return a list
#'   containing `l` as its only element.
#'
#' @export
one.row <- function(l) {
  lapply(l, function(x) {
    if (is.scalar(x)) x else list(x)
  })
}

#' Expand value to match domain structure.
#'
#' Expand a vector so that it matches a given distribution of variables
#' among domains.
#'
#' Let \eqn{D} be the number of domains,
#' \eqn{V_d} be the number of variables in domain $d$
#' for \eqn{d=1,...,d}, and let \eqn{V=V_1+...+V_D} be
#' the total number of variables.
#'
#' @param xs Vector of length \eqn{1}, \eqn{D} or \eqn{V}.
#' @param domain.n.vars Vector of length \eqn{D} containing the values
#'   \eqn{V_1,...,V_D}
#'
#' @return A vector of length \eqn{V}:
#'   - If `length(xs) == 1`, the value is assumed the same
#'     across all variables and domains; a vector with \eqn{V}
#'     copies of `xs` is returned.
#'   - If `length(xs) == D`, the same value is assumed to be shared
#'     by all variables in the same domain. The result
#'     is a vector with \eqn{V_1} copies of `xs[1]`, followed
#'     by \eqn{V_2} copies of `xs[2]`, and so on.
#'   - Otherwise, the vector `xs` is returned unchanged.
#'
recycle.to.vars <- function(xs, domain.n.vars) {
  n.domains <- length(domain.n.vars)
  if (length(xs) == 1) {
    xs <- rep(xs, times = sum(domain.n.vars))
  } else if (length(xs) == n.domains) {
    xs <- rep(xs, times = domain.n.vars)
  }
  # TODO: Check that he length of the vector is indeed V
  xs
}

#' Show a progress bar while iterating
#'
#' @param vs Iteratee
#' @param fun 1-argument function. The result is ignored.
#'
#' @details
#' Apply `fun` to each element of `vs`, incrementing
#' the progress bar after each element is processed.
#'
#' @export
progressbar.for <- function(vs, fun) {
  n <- length(vs)
  pb <- utils::txtProgressBar(min = 0, max = n, initial = 0)
  i <- 0
  for (v in vs) {
    fun(v)
    i <- i + 1
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
}

#' Assert that all ellements of a list are equal.
#'
#' @param x Vector
#' @return A single element of `x` (presumably equal to all other elements)
#'
#' @details
#' An assertion (see `assertthat` package) will fail if any of the
#' elements are different.
#'
#' The [all.equal()] R function is used for the comparison.
#'
#' @export
assert_all_equal <- function(x) {
  assertthat::assert_that(length(x) >= 1)
  chosen.one <- x[[1]]
  assertthat::assert_that(all(lapply(x, function(y) {
    isTRUE(all.equal(chosen.one, y))
  }) %>% unlist()), msg = "x contains distinct elements")
  chosen.one
}

#' AZ colors for decisions, lower-case keywords.
#'
#' @export
decision.colors.lowercase <- list(
  go = "#7fd7a7",
  discuss = "#ffdf7f",
  stop = "#ff7f7f"
)

#' Divide a number into groups as evenly as possible.
#'
#' @param N Number
#' @param n.summands Number of groups
#'
#' @return A vector of length `n.summands` such that its elements
#' add up to `N`, and such that the difference between any two elements is
#' no larger than 1.
#'
#' @export
divide_into_summands <- function(N, n.summands = 1) {
  assertthat::assert_that(N >= 0)
  assertthat::assert_that(n.summands >= 1)
  rest <- (N %% n.summands == 0)
  res <- rep(N %/% n.summands, n.summands)
  res <- res + c(rep(1, N %% n.summands), rep(0, n.summands - (N %% n.summands)))
  assertthat::assert_that(sum(res) == N)
  assertthat::assert_that(length(res) == n.summands)
  res
}

#' Location-shifted and scaled multivariate version of Student's t distribution;
#' cummulative distribution function
#'
#' @param lower Lower integration bound (either length 1 or V), -Inf to ignore.
#' @param upper Upper integration bound (either length 1 or V), +Inf to ignore.
#' @param delta Mean vector (either length 1 or V)
#' @param df Degrees of freedom (+Inf results in a multivariate normal)
#' @param sigma Positive definite matrix of dimensions V×V.
#' @param type Must be set to `shifted`.
#'
#' @returns The probability of all the components of a value being larger
#' than the corresponding component of `lower` and smaller than the corresponding
#' component of `upper`.
#'
#' @details
#' This is a thin wrapper on [mvtnorm::pmvt()] in order to properly support the
#' single variable case. See the documentation of that function for more details.
#'
#'
#'
#' @export
my_pmvt_shifted <- function(lower = -Inf, upper = Inf, delta, df = Inf, sigma, type = "shifted") {
  sigma <- as.matrix(sigma)
  if (all(dim(sigma) == c(1, 1))) {
    assertthat::are_equal(type, "shifted")
    assertthat::are_equal(length(lower), 1)
    assertthat::are_equal(length(upper), 1)
    assertthat::are_equal(length(delta), 1)
    assertthat::are_equal(length(df), 1)
    sd <- sqrt(sigma[1, 1])
    pt((upper - delta) / sd, df = df, ncp = 0) - pt((lower - delta) / sd, df = df, ncp = 0)
  } else {
    mvtnorm::pmvt(lower = lower, upper = upper, delta = delta, df = df, sigma = sigma, type = type)
  }
}

#' Returns the only element of a list
#'
#' @param x A list of elements
#'
#' @return The first element of `x` if all elements of `x` are equal
#'   (and `x` is non-empty).
#'
#' @details
#' An assertion will fail if `x` is either empty, or contains no elements.
#'
#' @export
the.one <- function(x) {
  res <- unique(x)
  assertthat::are_equal(length(res), 1)
  res[[1]]
}

#' Pretty formatter for a list
#'
#' A list is formatted using a nice syntax. The name of the
#' foremost class is used, followed by the arguments
#' between parenthesis.
#'
#' @param x List to be printed
#' @param ... Other arguments (ignored)
#'
#' @return A string representing the given list.
#'
#' @export
pretty.list <- function(x, ...) {
  s <- x
  c <- class(s)[[1]]
  paste0(
    if (c == "list") "" else c,
    "(",
    paste0(sapply(rlang::names2(s), function(name) {
      if (name == "") "" else paste0(name, "=")
    }),
    s,
    collapse = ","
    ),
    ")"
  )
}

#' Generate groupings of variables into domains
#'
#' @param n.domains Number of domains
#' @param n.vars Number of variables
#' @param min.vars.per.domain Minimum number of variables in a domain
#' @param max.vars.per.domain Maximum number of variables in a domain
#' @param ordered.except.for.first Whether to consider the order of some
#'   of the first values as relevant.
#'
#' @returns A list of all possible vectors of length `n.domains`  such that
#' the sum of the elements of each vector is equal `n.vars`, and each
#' element is between `min.vars.per.domain` and `max.vars.per.domain` (both
#' inclusive).
#'
#' @details
#' Vectors
#' which only differ in the last `n.domains - ordered.except.for.first` values
#' are considered equivalent.
#'
#' @export
generate_variable_groupings <- function(n.domains,
                                        n.vars,
                                        min.vars.per.domain = 1,
                                        max.vars.per.domain = +Inf,
                                        ordered.except.for.first = 0) {
  rec <- function(D, V, m) {
    if (D < 0) {
      list()
    } else if (D == 0) {
      if (V == 0) {
        list(c())
      } else {
        list()
      }
      # D ≥ 1
    } else if (V < D * m) {
      list()
    } else if (V > D * max.vars.per.domain) {
      list()
    } else {
      mapply(
        SIMPLIFY = FALSE,
        seq(
          from = m,
          to = min(
            V - m * (D - 1),
            max.vars.per.domain
          ),
          by = 1
        ),
        FUN = function(Vd) {
          if (n.domains - D >= ordered.except.for.first) {
            m_next <- Vd
          } else {
            m_next <- m
          }
          lapply(rec(D - 1, V - Vd, m_next), function(vds) c(Vd, vds))
        }
      ) %>% do.call(what = base::c)
    }
  }
  res <- rec(D = n.domains, V = n.vars, m = min.vars.per.domain)
  assertthat::assert_that(all(sapply(res, function(x) sum(x) == n.vars)))
  assertthat::assert_that(all(sapply(res, function(x) length(x) == n.domains)))
  res
}

#' Coalesce function
#'
#' @param x One value
#' @param y Another value
#'
#' @return One of `x` or `y`. If `x` is null then `y` is returned.
coalesce.null <- function(x, y) {
  if (!is.null(x)) x else y
}

utils::globalVariables(".")

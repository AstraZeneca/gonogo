#' @import methods
NULL

# TODO: Differentiate between regions and region-schemas
# (regions being a region-schema applied to endpoints)


#' @describeIn make_region Regions have class name "Region"
#' @export
Region <- "Region"

#' Region constructor
#'
#' @param type Constructor name; it becomes one of the classes.
#' @param ... Additional arguments.
#' @returns A [list()] with classes `type` and "Region",
#'   and containing the name-value pairs in `...`.
#' @details
#' This is an abstract region schema constructor.
#' It should not be used by the user directly, but instead
#' be used to define other Region-Schemas.
#'
#' @seealso Region schemas should implement [limit_points()] and
#'   [region_type()].
#'
#' @export
make_region <- function(type, ...) {
  structure(
    class = c(type, "Region"),
    tibble::lst(...)
  )
}

#' Limit points
#'
#' @param x A region schema (see [make_region()]).
#' @param endpoints Endpoints for which the region schema should be interpreted.
#' @returns A list of numeric vectors, each of which has as many
#'   coordinates as the number of endpoints.
#'
#' @details
#' The limit points are a set of points such that any point
#' in the region is point-wise larger (for upwards regions) or
#' smaller (for downwards regions) than at least one of the limit points.
#'
#' @export
limit_points <- function(x, endpoints) UseMethod("limit_points")

#' Region type
#'
#' @param x A region schema (see [make_region()]).
#' @returns Either "up" (for upwards regions) or "down" (for downwards regions).
#'   It may also return other types for, for instance, list-region-schemas,
#'   such as those produced by [r_at_each_domain()] or [r_at_each_endpoint()].
#'
#' @export
region_type <- function(x) UseMethod("region_type")

#' List of directions
#'
#' @examples
#' DIRECTION$Up
#' DIRECTION$Down
#' @details
#' The directions are character strings `"up"` and `"down"`.
#'
#' @export
DIRECTION <- list(Up = "up", Down = "down")

#' Branching on directions
#'
#' @param dir A [DIRECTION]
#' @param up An expression
#' @param down An expression
#' @param otherwise An expression
#' @returns If the direction is `"up"`, returns `up`;
#'    if it is `"down"`, returns `down`,
#'    otherwise, returns `otherwise`.
#'
#' @export
if.Direction <- function(dir,
                         up,
                         down,
                         otherwise =
                           {
                             assertthat::assert_that(FALSE)
                           })
{
  if (dir == DIRECTION$Up) {
    up
  } else if (dir == DIRECTION$Down) {
    down
  } else {
    otherwise
  }
}

#' Limit points of the entire space of true effects
#'
#' @param dir A [DIRECTION]
#' @param endpoints An [Endpoints()] object
#' @returns Depending on the direction, a list with a single
#'   vector, either negative or positive infinity.
#'
#' @export
whole_space <- function(dir, endpoints) {
  list(if.Direction(dir,
    up = parse_effect(endpoints, -Inf),
    down = parse_effect(endpoints, +Inf)
  ))
}

#' Checks whether a region is simple
#'
#' @param region A region schema
#' @returns `TRUE` if the region type is either "up" or "down".
#' @export
is_simple_region <- function(region) {
  region_type(region) %in% DIRECTION
}

#' Dimension of the vectors of the region
#'
#' @param x A region schema (see [make_region()]).
#' @param endpoints An [Endpoints()] object.
#' @returns An integer, representing the number of dimensions.
#'
#' @export
region_dimension <- function(x, endpoints) UseMethod("region_dimension")

#' @describeIn region_dimension By default, the number of endpoints.
#' @export
region_dimension.Region <- function(x, endpoints) n_vars(endpoints)

#' Checks whether a point belongs in a region
#'
#' @returns `TRUE` if the `needle` is in the region;
#' false otherwise.
#'
#' @export
point_in <- function(x, ...) UseMethod("point_in")

#' @describeIn point_in Default implementation
#'
#' @param x A region schema (see [make_region()]).
#' @param endpoints An [Endpoints()] object.
#' @param needle The point to check membership of.
#' @param ... Must be an empty list of arguments.
#'
#' @export
point_in.Region <- function(x, endpoints, needle, ...) {
  assertthat::assert_that(length(list(...)) == 0)
  haystack <- limit_points(x, endpoints)
  needle <- as.matrix(needle)
  assertthat::assert_that(nrow(needle) == n_vars(endpoints))

  ones <- matrix(1, nrow = 1, ncol = ncol(needle))
  res <- if.Direction(region_type(x),
    up =
      lapply(haystack, function(hay) {
        hay <- as.matrix(hay) %*% ones
        colSums(needle >= hay) == nrow(needle)
      }),
    down =
      lapply(haystack, function(hay) {
        hay <- as.matrix(hay) %*% ones
        colSums(needle <= hay) == nrow(needle)
      })
  )
  Reduce("|", res)
}

## Manipulating limit point collections

#' Intersection of regions represented by a single point
#'
#' @param dir A direction
#' @param a An effect vector
#' @param b An effect vector
#' @returns If `dir` is "up",
#'   an effect vector c such that,
#'   an effect vector is pointwise larger than both `a`
#'   and `b` iff it is pointwise larger than c;
#'   mutatis mutandis with "down" "pointwise smaller".
intersect_1_with_1 <- function(dir, a, b) {
  if (dir == DIRECTION$Up) {
    pmax(a, b)
  } else if (dir == DIRECTION$Down) {
    pmin(a, b)
  } else {
    assertthat::assert_that(FALSE)
  }
}

#' Intersection of a point with a set of points.
#'
#' @param dir A direction
#' @param a An effect vector
#' @param Y A list of effect vectors
#' @returns If `dir` is "up",
#'   an list of effect vectors Z such that,
#'   an effect vector is pointwise larger than both `a`
#'   and some element in Y,  iff it is pointwise larger than
#'   some element in Z;
#'   mutatis mutandis with "down" "pointwise smaller".
intersect_1_with_many <- function(dir, a, Y) {
  lapply(Y, function(b) {
    intersect_1_with_1(dir, a, b)
  })
}

#' Intersection of two set of points
#'
#' @param dir A direction
#' @param X A list of effect vectors
#' @param Y A list of effect vectors
#' @returns If `dir` is "up",
#'   an list of effect vectors Z such that,
#'   an effect vector is pointwise larger than some element in X
#'   and some element in Y,  iff it is pointwise larger than
#'   some element in Z;
#'   mutatis mutandis with "down" "pointwise smaller".
intersect_many <- function(dir, X, Y) {
  do.call(
    base::c,
    lapply(X, function(a) {
      intersect_1_with_many(dir, a, Y)
    })
  )
}

#' Union of two sets of points
#'
#' @param X A list of effect vectors
#' @param Y A list of effect vectors
#'
#' @returns A list of effect vector containing
#'   the elements in `X` and the elements in `Y`.
r_union <- function(X, Y) {
  c(X, Y)
}

#' The whole space of effects
#'
#' @param direction Whether the region is upwards or downwards.
#'
#' @details
#' The whole space can be interpreted in two ways:
#'   as all the points larger than the smallest point;
#'   or as all the points smaller than the largest point.
#' The direction of a region is important as upwards and downwards
#' regions can each only be used in certain contexts.
#'
#' @export
r_whole <- function(direction = c("up", "down")) {
  dir <- match.arg(direction)
  make_region("r_whole", direction)
}

#' @describeIn r_whole The type of `r_whole` is its direction.
#' @inheritParams region_type
#' @export
region_type.r_whole <- function(x) x$direction

#' @describeIn r_whole The whole region as a single smallest
#'   or largest point, depending on the direction.
#' @inheritParams limit_points
#' @export
limit_points.r_whole <- function(x, endpoints) {
  whole_space(x$direction, endpoints)
}

#' Upwards cone
#'
#' @param effect Effect vector
#' @returns A region containing all true effects that are pointwise larger
#'   than `effect`.
#'
#' @export
is_gt <- function(effect) make_region("is_gt", effect)

#' @describeIn is_gt This is an upwards region (type "up")
#' @inheritParams region_type
#' @export
region_type.is_gt <- function(x) DIRECTION$Up

#' @describeIn is_gt The `effect` is the only limit point.
#' @inheritParams limit_points
#' @export
limit_points.is_gt <- function(x, endpoints) {
  list(parse_effect(endpoints, x$effect))
}

#' An alias for `is_gt`.
#' @inheritParams is_gt
#' @export
effect_is_gt <- is_gt


#' Downwards cone
#'
#' @param effect Effect vector
#' @returns A region containing all true effects that are pointwise smaller
#'   than `effect`.
#'
#' @export
is_lt <- function(effect) make_region("is_lt", effect)

#' @describeIn is_lt This is a downwards region (type "down")
#' @inheritParams region_type
#' @export
region_type.is_lt <- function(x) DIRECTION$Down

#' @describeIn is_lt The `effect` is the only limit point.
#' @inheritParams limit_points
#' @export
limit_points.is_lt <- function(x, endpoints) {
  list(parse_effect(endpoints, x$effect))
}

#' An alias for `is_lt`.
#' @inheritParams is_lt
#' @export
effect_is_lt <- is_lt


#### VARIABLE SELECTION

#' A region schema restricted to some endpoints
#'
#' @param obj A region schema defined over the selected endpoints
#' @param vars.selector A variable selector, i.e. an object
#'   implementing [at_selected_vars()].
#' @return A region schema representing the largest region
#'   such that, when restricted to the endpoints in `vars.selection`,
#'   is equal to `region`.
#'
#' @export
at_selected_vars.Region <- function(obj, vars.selector) {
  assertthat::assert_that(is_simple_region(obj))
  make_region("at_selected_vars", region = obj, vars.selector)
}

#' @describeIn at_selected_vars.Region The type coincides with that of `region`.
#' @inheritParams region_type
#' @export
region_type.at_selected_vars <- function(x) region_type(x$region)

#' @describeIn at_selected_vars.Region The limit points are the same as those
#'    of region for the selected endpoints, and -Inf (if `region` has type "up")
#'    or -Inf (if `region` has type "down") otherwise.
#' @inheritParams limit_points
#' @export
limit_points.at_selected_vars <- function(x, endpoints) {
  assertthat::assert_that(is_simple_region(x$region))
  missing <- if.Direction(
    region_type(x$region),
    up   = -Inf,
    down = +Inf
  )
  ground <- parse_effect(endpoints, missing)
  vars.ii <- select_vars(x$vars.selector, endpoints$vars)
  lapply(
    limit_points(
      x$region,
      at_selected_vars(endpoints, x$vars.selector)
    ),
    function(a) {
      res <- ground
      res[vars.ii] <- a
      res
    }
  )
}

## Region sequencing

#' Region sequencing
#'
#' @param regions A list of [Region] schemas.
#'   The list should be non-empty if `homogeneous == TRUE`.
#' @param homogeneous Whether all lists have the same direction.
#' @returns A [Region] list-schema.
#'
#' @seealso The function  [sequence_schemas()] has similar functionality.
#'
#' @export
sequence_region <-
  function(regions, homogeneous = TRUE) {
    make_region("sequence_region", regions, homogeneous)
  }

#' @describeIn sequence_region If `homogeneous == TRUE`, the type is
#'  "\[A\]", where "A" is the type of the first (and therefore all) elements of `regions`.
#'  For example, "\[up\]" if all elements of `regions` are upwards.
#'  Otherwise, "(A,B,C,...)", where "A", "B", "C", ... are the types of the elements of
#'  `regions`.
#' @inheritParams region_type
#' @export
region_type.sequence_region <- function(x) {
  types <- sapply(x$regions, region_type)
  if (x$homogeneous) {
    types <- unique(types)
    assertthat::are_equal(length(types), 1)
    type <- paste0("[", types, "]")
  } else {
    type <- paste0("(", paste(types, collapse = ","), ")")
  }
  type
}

#' @describeIn sequence_region This function returns a list
#'   containing the lists of limit points of each of the elements of `regions`.
#' @inheritParams limit_points
#' @export
limit_points.sequence_region <- function(x, endpoints) {
  lapply(x$regions, function(region) limit_points(region, endpoints))
}

#### Quantification

#' Domain iteration
#'
#' @param region A [Region] schema.
#' @returns A [Region] list-schema containing the restriction of `region`
#'   to each of the domains.
#'
#' @export
r_at_each_domain <- function(region) {
  make_region_alias("r_at_each_domain", region)
}

#' @describeIn r_at_each_domain An alias for `r_at_each_domain`.
#' @inheritParams at_each_domain
#' @export
at_each_domain.Region <- function(schema) {
  r_at_each_domain(schema)
}

#' @describeIn r_at_each_domain The type is "\[A\]", where
#'   "A" is the type of `region` (e.g. "\[up\]" or "\[down\]").
#' @inheritParams region_type
#' @export
region_type.r_at_each_domain <- function(x) {
  paste0("[", region_type(x$region), "]")
}

#' @describeIn r_at_each_domain The implementation
#' @inheritParams dealias
#' @param endpoints The meaning of this schema depends on the endpoints involved.
#' @export
dealias.r_at_each_domain <- function(x, endpoints, ...) {
  x.dealiased <- sequence_region(
    lapply(
      endpoints$domains$name,
      function(name) x$region %at_domain% name
    )
  )
  assertthat::are_equal(region_type(x), region_type(x.dealiased))
  x.dealiased
}

#' Endpoint iteration
#'
#' @param region A [Region] schema.
#' @returns A [Region] list-schema containing the restriction of `region`
#'   to each of the endpoints.
#'
#' @export
r_at_each_endpoint <- function(region) {
  make_region_alias("r_at_each_endpoint", region)
}

#' @describeIn r_at_each_endpoint An alias for `r_at_each_endpoint`.
#' @inheritParams at_each_endpoint
#' @export
at_each_endpoint.Region <- function(schema) {
  r_at_each_endpoint(schema)
}

#' @describeIn r_at_each_endpoint The type is "\[A\]", where
#'   "A" is the type of `region` (e.g. "\[up\]" or "\[down\]").
#' @export
region_type.r_at_each_endpoint <- function(x) {
  paste0("[", region_type(x$region), "]")
}

#' @describeIn r_at_each_endpoint The implementation
#' @inheritParams dealias
#' @param endpoints The concrete region depends on the endpoints.
#' @export
dealias.r_at_each_endpoint <- function(x, endpoints, ...) {
  x.dealiased <- sequence_region(
    lapply(
      endpoints$vars$name,
      function(name) {
        x$region %at_var% name
      }
    )
  )
  assertthat::are_equal(region_type(x), region_type(x.dealiased))
  x.dealiased
}

##### Region aliasing

#' Region aliases
#'
#' @param type Constructor name
#' @param ... Other arguments (passed through to [make_region()])
#' @returns A [Region] with classes `type` and `"Alias"`
#'
#' A region alias should implement the [dealias] method.
#'
#' @export
make_region_alias <- function(type, ...) {
  make_region(c(type, "Alias"), ...)
}

#' @describeIn make_region_alias By default, implemented as the type of
#'   the dealiased version of the region.
#' @param ... Other arguments (passed through both to  [dealias()] and to
#'   [region_type()]).
#' @inheritParams region_type
#' @export
region_type.Alias <- function(x, ...) region_type(dealias(x, ...), ...)

#' @describeIn make_region_alias By default, implemented as the limit points
#'   of the dealiased version of the region.
#' @param ... Other arguments (passed through both to  [dealias()] and to
#'   [limit_points()]).
#' @inheritParams limit_points
#' @export
limit_points.Alias <- function(x, ...) limit_points(dealias(x, ...), ...)

##### At least k

helper_limit_points_at_least_k <- function(dir, K, all.points, endpoints) {
  assertthat::assert_that(K >= 0)
  assertthat::assert_that(is(endpoints, "Endpoints"))
  rec <- function(k, points) {
    N <- length(points)
    if (k == 0) {
      whole_space(dir, endpoints)
    } else if (k > N) {
      list()
    } else if (k == 1) {
      do.call(base::c, points)
    } else {
      rest <- points[2:N]
      r_union(
        intersect_many(dir, points[[1]], rec(k - 1, rest)),
        rec(k, rest)
      )
    }
  }
  rec(K, all.points)
}

helper_region_type_at_least_k <- function(x) {
  if (region_type(x$list.region) == paste0("[", DIRECTION$Up, "]")) {
    DIRECTION$Up
  } else if (region_type(x$list.region) == paste0("[", DIRECTION$Down, "]")) {
    DIRECTION$Down
  } else {
    assertthat::assert_that(FALSE)
  }
}

#' Region of at least k
#'
#' @param k An integer constant
#' @param list.region A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @returns A [Region] containing those points that are present in at
#'   least `k` of the regions in `list.regions`.
#'
#' @export
r_at_least_k <- function(k, list.region) {
  make_region("r_at_least_k", k, list.region)
}

#' Alias for [r_at_least_k()].
#'
#' @inheritParams at_least_k
#'
#' @export
at_least_k.Region <- function(k, x) {
  r_at_least_k(k, x)
}

#' @describeIn r_at_least_k The region type coincides with the type of the elements
#'    in `list.region` (either `"up"` or `"down"`).
#' @inheritParams region_type
#' @export
region_type.r_at_least_k <- helper_region_type_at_least_k

#' @describeIn r_at_least_k The limit points are computed as all the possible
#'   intersections of the limit points of `k` regions in `list.region`.
#' @inheritParams limit_points
#' @export
limit_points.r_at_least_k <- function(x, endpoints) {
  all.points <- limit_points(x$list.region, endpoints)
  dir <- region_type(x)
  helper_limit_points_at_least_k(dir, x$k, all.points, endpoints)
}

#' Region of at least half
#'
#' @details
#' This keyword has a single implementation:
#' [at_least_half.Region]
#'
#' @inheritParams at_least_half.Region
#'
#' @export
at_least_half <- function(x) UseMethod("at_least_half")

#' Region of at least half
#'
#' Points that are in at least half of the concerned regions,
#' rounded up.
#'
#' @param x A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @returns A [Region] containing those points that are present in at
#'   least half of the regions in `list.region`, rounded up.
#'
#' @export
at_least_half.Region <- function(x) {
  make_region("r_at_least_half", list.region = x)
}

#' @describeIn at_least_half.Region The region type coincides with the type of the elements
#'    in `list.region` (either `"up"` or `"down"`).
#'
#' @inheritParams region_type
#'
#' @export
region_type.r_at_least_half <- helper_region_type_at_least_k

#' @describeIn at_least_half.Region The limit points are computed as all the possible
#'   intersections of the limit points of half the regions (rounded up) in `list.region`.
#'
#' @inheritParams limit_points
#'
#' @export
limit_points.r_at_least_half <- function(x, endpoints) {
  all.points <- limit_points(x$list.region, endpoints)
  K <- length(all.points)
  K.half <- ceiling(K / 2)
  dir <- region_type(x)
  helper_limit_points_at_least_k(dir, K.half, all.points, endpoints)
}

##########

#' Region of all but one of the regions
#'
#' @details
#' This keyword has a single implementation:
#' [at_least_all_but_1.Region].
#' @inheritParams at_least_all_but_1.Region
#'
#' @export
at_least_all_but_1 <- function(list.schema) UseMethod("at_least_all_but_1")


#' Region of at least all but one
#'
#' Points that are in at least all but one, or in all of the concerned regions.
#'
#' @param list.schema A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @returns A [Region] containing those points that are present in at
#'   least all but one, or more, of the regions in `list.region`.
#'
#' @export
at_least_all_but_1.Region <- function(list.schema) {
  list.region <- list.schema
  make_region("r_at_least_all_but_1", list.region)
}

#' @describeIn at_least_all_but_1.Region The region type coincides with the type of the elements
#'    in `list.region` (either `"up"` or `"down"`).
#' @export
region_type.r_at_least_all_but_1 <- helper_region_type_at_least_k

#' @describeIn at_least_all_but_1.Region The limit points are the union of the
#'   limit points of all possible intersections of K-1 regions of `list.region`,
#'   where K is the number of regions in `list.region`.
#' @inheritParams limit_points
#' @export
limit_points.r_at_least_all_but_1 <- function(x, endpoints) {
  all.points <- limit_points(x$list.region, endpoints)
  K <- length(all.points)
  dir <- region_type(x)
  helper_limit_points_at_least_k(dir, K - 1, all.points, endpoints)
}

##########


#' Region of at least all but k
#'
#' @details
#' This keyword has a single implementation:
#' [at_least_all_but_k.Region()].
#'
#' @inheritParams at_least_all_but_k.Region
#'
#' @export
at_least_all_but_k <- function(k, list.schema) UseMethod("at_least_all_but_k", list.schema)

#' Region of at least all but k
#'
#' @param k An integer constant
#' @param list.schema A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @returns A [Region()] containing those points that are present in at
#'   least K-`k` of the regions in `list.schema`, where K is the number
#'   of regions in `list.schema`.
#'
#' @export
at_least_all_but_k.Region <- function(k, list.schema) {
  make_region("r_at_least_all_but_k", k = k, list.region = list.schema)
}

#' @describeIn at_least_all_but_k.Region The region type coincides with the type of the elements
#'    in `list.region` (either `"up"` or `"down"`).
#' @inheritParams region_type
#' @export
region_type.r_at_least_all_but_k <- helper_region_type_at_least_k

#' @describeIn at_least_all_but_k.Region The limit points are the union of the
#'   limit points of all possible intersections of K-`k` regions of `list.region`,
#'   where K is the number
#'   of regions in `list.region`.
#' @inheritParams limit_points
#' @export
limit_points.r_at_least_all_but_k <- function(x, endpoints) {
  all.points <- limit_points(x$list.region, endpoints)
  K <- length(all.points)
  dir <- region_type(x)
  helper_limit_points_at_least_k(dir, K - x$k, all.points, endpoints)
}
##########



#' Region of at least all but k
#'
#' @param list.region A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @return A [Region] containing those points that are present in at
#'   least 1 of the regions in `list.regions`.
#'
#' @export
r_at_least_1 <- function(list.region) {
  make_region_alias("r_at_least_1", list.region)
}

#' An alias for [r_at_least_1()]
#' @inheritParams at_least_1
#' @export
at_least_1.Region <- function(list.schema) {
  r_at_least_1(list.schema)
}

#' @describeIn r_at_least_1 Implementation.
#' @inheritParams dealias
#' @export
dealias.r_at_least_1 <- function(x, ...) {
  r_at_least_k(1, x$list.region)
}



#' Region of all
#'
#' @param list.region A list-schema of type `"\[up\]"` or `"\[down\]"`,
#'   comprising a number of upwards (respectively downwards) regions.
#' @returns A [Region] containing those points that are present in
#'   all of the regions in `list.region`.
#'
#' @export
r_all <- function(list.region) {
  make_region("r_all", list.region)
}

#' @describeIn r_all The region type coincides with the type of the elements
#'    in `list.region` (either `"up"` or `"down"`).
#' @inheritParams region_type
#' @export
region_type.r_all <- helper_region_type_at_least_k

#' @describeIn r_all The limit points are those of the intersection of
#'   all the regions in `list.region`.
#' @inheritParams limit_points
#' @export
limit_points.r_all <- function(x, endpoints) {
  all.points <- limit_points(x$list.region, endpoints)
  dir <- region_type(x)
  helper_limit_points_at_least_k(dir, length(all.points), all.points, endpoints)
}

### Quantification aliases

#' For at least k of the endpoints
#'
#' @param k An integer constant.
#' @param schema A region defined on a single endpoint.
#' @returns A region containing those points such that, for at least k endpoints,
#'   when restricted
#'   to an endpoint, they are in the
#'   region corresponding to that endpoint.
#' @export
r_at_least_k_endpoints <- function(k, schema) {
  make_region_alias("r_at_least_k_endpoints", k, region = schema)
}

#' An alias of [r_at_least_k_endpoints()]
#' @inheritParams for_at_least_k_endpoints
#' @export
for_at_least_k_endpoints.Region <- function(k, schema) {
  r_at_least_k_endpoints(k, schema)
}

#' @describeIn r_at_least_k_endpoints Implementation
#' @inheritParams dealias
#' @export
dealias.r_at_least_k_endpoints <- function(x, ...) {
  r_at_least_k(x$k, r_at_each_endpoint(x$region))
}

#' For at least half of the endpoints
#'
#' Implemented as [for_at_least_half_of_endpoints.Region]
#' @inheritParams for_at_least_half_of_endpoints.Region
#' @export
for_at_least_half_of_endpoints <- function(schema) UseMethod("for_at_least_half_of_endpoints")

#' For at least half of the endpoints
#'
#' @param schema A region defined on a single endpoint.
#' @returns A region containing those points such that, for at least half of the endpoints, rounded up,
#'   when restricted
#'   to that endpoint, they are in the
#'   region corresponding to that endpoint.
#' @export
for_at_least_half_of_endpoints.Region <- function(schema) {
  make_region_alias("r_at_least_half_of_endpoints", region = schema)
}

#' @describeIn for_at_least_half_of_endpoints.Region Implementation
#' @inheritParams dealias
#' @export
dealias.r_at_least_half_of_endpoints <- function(x, ...) {
  at_least_half(r_at_each_endpoint(x$region))
}


#' For at least all but one of the endpoints
#'
#' Implemented as [for_at_least_all_but_1_endpoint.Region()]
#' @inheritParams for_at_least_all_but_1_endpoint.Region
#' @export
for_at_least_all_but_1_endpoint <- function(schema) UseMethod("for_at_least_all_but_1_endpoint")

#' For at least all but one of the endponts
#'
#' @param schema A region defined on a single endpoint.
#' @returns A region containing those points such that, for at least all but one of the endpoints,
#'   when restricted
#'   to that endpoint, they are in the
#'   region corresponding to that endpoint.
#' @export
for_at_least_all_but_1_endpoint.Region <- function(schema) {
  make_region_alias("r_at_least_all_but_1_endpoint", region = schema)
}

#' @describeIn for_at_least_all_but_1_endpoint.Region Implementation
#' @inheritParams dealias
#' @export
dealias.r_at_least_all_but_1_endpoint <- function(x, ...) {
  at_least_all_but_1(r_at_each_endpoint(x$region))
}

#####

#' For all endpoints
#'
#' @param region A region defined on a single endpoint.
#' @returns A region containing those points such that, for all of the the endpoints,
#'   when restricted
#'   to that endpoint, they are in the
#'   region corresponding to that endpoint.
#' @export
r_all_endpoints <- function(region) {
  make_region_alias("r_all_endpoints", region)
}

#' An alias of [r_all_endpoints()]
#' @inheritParams for_all_endpoints
#' @export
for_all_endpoints.Region <- function(x) {
  r_all_endpoints(x)
}

#' @describeIn r_all_endpoints Implementation
#' @inheritParams dealias
#' @export
dealias.r_all_endpoints <- function(x, ...) {
  r_all(r_at_each_endpoint(x$region))
}

#############

#' For at least 1 endpoint
#'
#' @param region A region defined on a single endpoint.
#' @returns A region containing those points such that, for all of the the endpoints,
#'   when restricted
#'   to that endpoint, they are in the
#'   region corresponding to that endpoint.
#' @export
r_at_least_1_endpoint <- function(region) {
  make_region_alias("r_at_least_1_endpoint", region)
}

#' An alias for [r_at_least_1_endpoint]
#' @inheritParams for_at_least_1_endpoint
#' @export
for_at_least_1_endpoint.Region <- function(schema) {
  r_at_least_1_endpoint(schema)
}

#' @describeIn r_at_least_1_endpoint Implementation
#' @inheritParams dealias
#' @export
dealias.r_at_least_1_endpoint <- function(x, ...) {
  r_at_least_1(r_at_each_endpoint(x$region))
}

## And, or

#' Intersection of two regions
#'
#' @param regions A list of [Region]s of the same type (either "up" or "down").
#' @returns A [Region] containing all the points that are in all the regions in `regions`.
#'
#' @export
r_and <- function(regions) {
  make_region_alias("r_and", regions)
}

#' @describeIn r_and Implementation
#' @inheritParams dealias
#' @export
dealias.r_and <- function(x, ...) {
  r_at_least_k(
    length(x$regions),
    sequence_region(x$regions)
  )
}

#' @describeIn grapes-and-grapes-.Region This is used for flattening nested calls to `%and%`.
`%and%.r_and` <- function(x, y) r_and(c(x$regions, list(y)))

#' Intersection of two regions
#'
#' @param x A [Region]
#' @param y A [Region] of the same type as `x` (either "up" or "down")
#'
#' @export
`%and%.Region` <- function(x, y) r_and(list(x, y))

#' Union of two regions
#'
#' @param regions A list of [Region]s of the same type (either "up" or "down").
#' @returns A [Region] containing all the points that are in any of the regions in `regions`.
#'
#' @export
r_or <- function(regions) {
  make_region_alias("r_or", regions)
}

#' @describeIn r_or Implementation
#' @inheritParams dealias
#' @export
dealias.r_or <- function(x, ...) {
  r_at_least_1(
    sequence_region(x$regions)
  )
}

#' @describeIn grapes-or-grapes-.Region This is used for flattening nested calls to `%or%`.
`%or%.r_or` <- function(x, y) r_or(c(x$regions, list(y)))


#' Union of two regions
#'
#' @param x A [Region]
#' @param y A [Region] of the same type as `x` (either "up" or "down")
#'
#' @export
`%or%.Region` <- function(x, y) r_or(list(x, y))

##### Applications

#' Check if a region is empty
#'
#' @param region A [Region] schema
#' @param endpoints An [Endpoints] object
#' @returns `TRUE` if the region schema, when specialized to those endpoints,
#'   contains no points; `FALSE` otherwise.
#'
#' @export
region_is_empty <- function(region, endpoints) {
  points <- limit_points(region, endpoints)
  if.Direction(
    region_type(region),
    down = {
      all(sapply(points, function(point) any(point <= -Inf)))
    },
    up = {
      all(sapply(points, function(point) any(point >= +Inf)))
    }
  )
}


#' Check if a region is lower bounded
#' (i.e. for up regions, this means that the
#' limit points have finite coordinates)
#'
#' @param region A [Region] schema
#' @param endpoints An [Endpoints] object
#' @returns `TRUE` if the region schema, when specialized to those endpoints,
#'   has a finite lower bound; `FALSE` otherwise.
#'
#' @export
is_lower_bounded <- function(region, endpoints) {
  points <- limit_points(region, endpoints)
  if.Direction(
    region_type(region),
    down = region_is_empty(region),
    up = {
      all(
        sapply(points, function(point) {
          all(point > -Inf)
        })
      )
    }
  )
}

#' Check if a region is upper bounded
#' (i.e. for down regions, this means that the
#' limit points have finite coordinates)
#'
#' @param region A [Region] schema
#' @param endpoints An [Endpoints] object
#' @returns `TRUE` if the region schema, when specialized to those endpoints,
#'   has a finite upper bound; `FALSE` otherwise.
#'
#' @export
is_upper_bounded <- function(region, endpoints) {
  points <- limit_points(region, endpoints)
  if.Direction(
    region_type(region),
    up = {
      # If the region is closed upwards,
      # then the only way for it to be upper
      # bounded is for it to be empty.
      region_is_empty(region)
    },
    down = {
      all(
        sapply(points, function(point) {
          all(point < +Inf)
        })
      )
    }
  )
}


#' Measures a region according to an integral measure
#'
#' @param region A [Region] schema
#' @param endpoints An [Endpoints] object to instantiate the [Region] schema
#' @param measure A function taking two arguments;
#'   the `lower`most, leftmost corner of a rectangle (or higher-dimensional
#'   equivalent) and the `upper`most, rightmost corner of a rectangle
#'   or equivalent, and returning the measure of that rectangle
#'   according to some probability measure.
#'
#' @returns The measure of `region`
#'
#' @examples
#' endpoints <- make_endpoints(n.domains = 1, domain.n.vars = 2)
#' # The following returns 0.25
#' measure_region(
#'   region = is_gt(0),
#'   endpoints = endpoints,
#'   measure = function(lower, upper) {
#'     mvtnorm::pmvnorm(
#'       lower = lower, upper = upper,
#'       sigma = endpoints$sigma
#'     )
#'   }
#' )
#' @export
measure_region <- function(region, endpoints, measure) {
  all.points <- limit_points(region, endpoints)
  dir <- region_type(region)
  cone.measure <- if.Direction(dir,
    up = function(a) {
      measure(lower = a, upper = +Inf)
    },
    down = function(a) {
      measure(lower = -Inf, upper = a)
    }
  )
  rec <- function(points) {
    N <- length(points)
    if (N == 0) {
      0
    } else if (N == 1) {
      cone.measure(points[[1]])
    } else {
      a <- points[[1]]
      X <- points[2:N]
      cone.measure(a) +
        rec(X) -
        rec(intersect_1_with_many(dir, a, X))
    }
  }
  rec(all.points)
}

#' Constraint matrices for a region
#'
#' @param x A [Region].
#' @param endpoints An [Endpoints] object.
#' @returns A list object with the following fields:
#' | Name | Type | Description |
#' |------|------|-------------|
#' | A.t  | Numeric matrix | Constraints LHS |
#' | op   | character | Always ">=" |
#' | b0   | Numeric vector | Constraints RHS |
#'
#' @details
#' This is intended for use with,
#' for example, the quadprog package.
#'
#' The constraints should be interpreted as A.t b >= b0
constraint_matrices <- function(x, endpoints) {
  V <- region_dimension(x, endpoints)
  points <- limit_points(x, endpoints)
  if (region_is_empty(x, endpoints)) {
    list()
  } else {
    if.Direction(
      region_type(x),
      up = lapply(points, function(a) {
        a.ii <- which(a > -Inf)
        list(
          A.t = diag(V)[a.ii, , drop = FALSE],
          op = ">=",
          b0 = a[a.ii]
        )
      }),
      down = lapply(points, function(a) {
        a.ii <- which(a < +Inf)
        list(
          A.t = -diag(V)[a.ii, , drop = FALSE],
          op = ">=",
          b0 = -a[a.ii]
        )
      })
    )
  }
}

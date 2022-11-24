`%>%` <- dplyr::`%>%`

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1 || !(args[1] %in% c("on", "off", "fail"))) {
  stop("Usage: Rscript lint.R [off|on|fail]")
}
dry.mode <- args[1]

styler::style_file("ci.R", dry = dry.mode)
styler::style_file("lint.R", dry = dry.mode)
styler::style_pkg(dry = dry.mode)
styler::style_dir(path = "extra", exclude_dirs = "preexisting", filetype = c("R", "Rmd"), dry = dry.mode)
# lintr::lint_package(cache = TRUE) %>% as.data.frame() %>%
#  # No good solution for evaluation in custom environments
#  # TODO: Perhaps exclude based on e.g. occurrence of "with" on the same line
#  # Alt. declare these variables as "globals"
#  dplyr::filter(!(linter == "object_usage_linter" &&
#    stringr::str_detect(message, "^no visible binding for global variable"))) %>%
#  ## Check that there are no lint errors
#  (function(d) {
#    stopifnot(nrow(d) == 0)
#  })
warnings()

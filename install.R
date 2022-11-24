install <- function(pkg = ".", tmp_pkg = "build", ...) {
  print(paste0("Package in ",tmp_pkg))
  rules <- readLines(file.path(pkg, ".Rbuildignore"))
  list.files(path = pkg, recursive = T) |>
    purrr::discard(function(f) purrr::map(rules, function(r) grepl(r, f)) |> purrr::reduce(sum) > 0) |>
    purrr::walk(function(f) {
      print(paste0("Copying ",f))
      new_file <-file.path(tmp_pkg, f)
      dir.create(dirname(new_file),  showWarnings = F, recursive = T)
      file.copy(f, new_file, overwrite = T)
    })
  devtools::install(tmp_pkg, ...)
}

install(tmp_pkg = tempdir())

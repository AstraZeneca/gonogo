library(tikzDevice)
options(tikzMetricsDictionary = "_tikzMetrics")
if (!dir.exists("report")) {
  stopifnot(dir.create("report"))
}

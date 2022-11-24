# Source: https://rpubs.com/Koundy/71792

# theme_Publication <- function() {
#   library(grid)
#   library(ggthemes)
#   (theme_gray()
#     + theme(#plot.title = element_text(face = "bold",
#             #                          size = rel(1.2), hjust = 0.5),
#             text = element_text(size=10),
#             panel.background = element_rect(colour = NA),
#             plot.background = element_rect(colour = NA),
#             panel.border = element_rect(colour = "#f0f0f0", size=0.5),
#             #axis.title = element_text(face = "bold",size = rel(1)),
#             #axis.title.y = element_text(angle=90,vjust =2),
#             #axis.title.x = element_text(vjust = -0.2),
#             axis.text = element_text(size=rel(0.8)),
#             axis.line = element_line(colour="black"),
#             axis.ticks = element_line(),
#             panel.grid.major = element_line(colour="#f0f0f0"),
#             panel.grid.minor = element_blank(),
#             legend.key = element_rect(colour = NA),
#             legend.position = "bottom",
#             legend.direction = "horizontal",
#             #legend.key.size= unit(0.2, "cm"),
#             #legend.margin = unit(0, "cm"),
#             #legend.title = element_text(face="italic"),
#             #plot.margin=unit(c(10,5,5,5),"mm"),
#             strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
#             #strip.text = element_text(face="bold")
#     ))
#
# }

theme_Publication <- function() {
  library(ggthemes)
  theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_line(colour = "gray92", size = rel(0.5)),
      panel.grid.major = element_line(colour = "gray82")
    )
}

# https://stackoverflow.com/questions/7455046/how-to-make-graphics-with-transparent-background-in-r-using-ggplot2
theme_Slides <- function() {
  theme_Publication() +
    theme(
      # panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      # panel.grid.major = element_blank(), # get rid of major grid
      # panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = NA), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent", colour = NA) # get rid of key legend fill, and of the surrounding
    )
}

theme_no_margins <- function() {
  theme(
    plot.margin = unit(c(0, 0, 0, 0), units = "pt"),
    panel.spacing = unit(c(0, 0, 0, 0), units = "pt")
  )
}

scale_shape_Publication <- function(...) {
  library(scales)
  discrete_scale("shape", "Publication", manual_pal(values = c(16, 9)), ...)
}

scale_fill_Publication <- function(...) {
  library(scales)
  discrete_scale("fill", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...)
}

scale_colour_Publication <- function(...) {
  library(scales)
  discrete_scale("colour", "Publication", manual_pal(values = c("#386cb0", "#fdb462", "#7fc97f", "#ef3b2c", "#662506", "#a6cee3", "#fb9a99", "#984ea3", "#ffff33")), ...)
}

# From https://stackoverflow.com/questions/20749444/converting-r-matrix-into-latex-matrix-in-the-math-or-equation-environment
pmatrix <- function(x, digits = NULL, ...) {
  library(xtable)
  default_args <- list(
    include.colnames = FALSE, only.contents = TRUE,
    include.rownames = FALSE, hline.after = NULL, comment = FALSE,
    print.results = FALSE
  )
  passed_args <- list(...)
  calling_args <- c(
    list(x = xtable(x, digits = digits)),
    c(
      passed_args,
      default_args[setdiff(names(default_args), names(passed_args))]
    )
  )
  cat(
    "\\begin{pmatrix}\n",
    do.call(print.xtable, calling_args),
    "\\end{pmatrix}\n"
  )
}

#' @import ggplot2
#' @export
theme_calendar <- function (base_size = 11, base_family = "") {
  theme(
    line = element_blank(),
    rect = element_blank(),
    text = element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.text = element_blank(),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(hjust = 0),
    strip.text = element_text(size = rel(0.8)),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    complete = TRUE
  )
}








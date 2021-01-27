#' A ggplot2 complete theme for make publication ready figures
#'
#' @param base_size font size, default 14
#' @param base_family font family, defaut 'sans'
#' 
#' @export
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, disp)) + geom_point() + theme_publication()
#' @author Koundinya Desiraju (https://github.com/koundy/ggplot_theme_Publication)
#' @import ggplot2
#' @import ggthemes
theme_publication <- function(base_size = 14, base_family = "sans") {
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
   + ggplot2::theme(
     plot.title = ggplot2::element_text(
       face = "bold",
       size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)
     ),
     text = ggplot2::element_text(),
     panel.background = ggplot2::element_rect(colour = NA),
     plot.background = ggplot2::element_rect(colour = NA),
     panel.border = ggplot2::element_rect(colour = NA),
     axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
     axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
     axis.title.x = ggplot2::element_text(vjust = -0.2),
     axis.text = ggplot2::element_text(),
     axis.line.x = ggplot2::element_line(colour = "black"),
     axis.line.y = ggplot2::element_line(colour = "black"),
     axis.ticks = ggplot2::element_line(),
     panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
     panel.grid.minor = ggplot2::element_blank(),
     legend.key = ggplot2::element_rect(colour = NA),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vetical",
     legend.key.size = unit(0.5, "cm"),
     # legend.margin = unit(0, "cm"),
     legend.title = ggplot2::element_text(face = "italic"),
     plot.margin = unit(c(10, 5, 5, 5), "mm"),
     strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
     strip.text = ggplot2::element_text(face = "bold")
   ))
}
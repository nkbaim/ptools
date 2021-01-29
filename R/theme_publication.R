#' A ggplot2 light theme for make publication ready figures
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
#' 
theme_publication <- function(base_size = 14, base_family = "sans") {
   (theme_foundation(base_size = base_size, base_family = base_family)
      + theme(
         plot.title = element_text(
            face = "bold",
            size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)
         ),
         text = element_text(),
         panel.background = element_rect(colour = NA),
         plot.background = element_rect(colour = NA),
         panel.border = element_rect(colour = NA),
         axis.title = element_text(face = "bold", size = rel(1)),
         axis.title.y = element_text(angle = 90, vjust = 2),
         axis.title.x = element_text(vjust = -0.2),
         axis.text = element_text(),
         axis.line.x = element_line(colour = "black"),
         axis.line.y = element_line(colour = "black"),
         axis.ticks = element_line(),
         panel.grid.major = element_line(colour = "#f0f0f0"),
         panel.grid.minor = element_blank(),
         legend.key = element_rect(colour = NA),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.box = "vetical",
         legend.key.size = unit(0.5, "cm"),
         # legend.margin = unit(0, "cm"),
         legend.title = element_text(face = "italic"),
         plot.margin = unit(c(10, 5, 5, 5), "mm"),
         strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
         strip.text = element_text(face = "bold")
      ))
}

#' A ggplot2 dark theme for make publication ready figures.
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
#' 
#' 
theme_dark_blue <- function(base_size = 14, base_family = "sans") {
   (ggthemes::theme_foundation(base_size = base_size, base_family = base_family)
      + ggplot2::theme(
         plot.title = ggplot2::element_text(
            face = "bold", colour = "#ffffb3",
            size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)
         ),
         text = ggplot2::element_text(),
         panel.background = ggplot2::element_rect(colour = NA, fill = "#282C33"),
         plot.background = ggplot2::element_rect(colour = NA, fill = "#282C33"),
         panel.border = ggplot2::element_rect(colour = NA),
         axis.title = ggplot2::element_text(face = "bold", size = rel(1), colour = "white"),
         axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
         axis.title.x = ggplot2::element_text(vjust = -0.2),
         axis.text = ggplot2::element_text(colour = "grey70"),
         axis.line.x = ggplot2::element_line(colour = "grey70"),
         axis.line.y = ggplot2::element_line(colour = "grey70"),
         axis.ticks = ggplot2::element_line(colour = "grey70"),
         panel.grid.major = ggplot2::element_line(colour = "#343840"),
         panel.grid.minor = ggplot2::element_blank(),
         legend.background = ggplot2::element_rect(fill = "#282C33"),
         legend.text = ggplot2::element_text(color = "white"),
         legend.key = ggplot2::element_rect(colour = NA, fill = "#282C33"),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.box = "vetical",
         legend.key.size = unit(0.5, "cm"),
         # legend.margin = unit(0, "cm"),
         legend.title = ggplot2::element_text(face = "italic", colour = "white"),
         plot.margin = unit(c(10, 5, 5, 5), "mm"),
         strip.background = ggplot2::element_rect(colour = "#2D3A4C", fill = "#2D3A4C"),
         strip.text = ggplot2::element_text(face = "bold", colour = "white")
      ))
}



#' scale fill colors 
#' @description scale fill colors (barplot,boxplot...) with manually specified colors
#' @param colors a vector of color codes
#' @param ... other parameters pass to discrete_scale() function
#'
#' @export
#'
#' @import scales
#' @import ggplot2
#' @seealso [scale_colour_publication()] [scale_fill_publication_dark()] [scale_colour_publication_dark()]
scale_fill_publication <- function(colors = NULL, ...) {
   if(is.null(colors)){
      colors <- light_bg_colors()
   }
   ggplot2::discrete_scale("fill", "Publication", scales::manual_pal(values = colors), ...)
}

#' Title
#'
#' @inheritParams scale_fill_publication
#'
#' @export
#'
#' @import scales
#' @import ggplot2
scale_colour_publication <- function(colors = NULL, ...) {
   if(is.null(colors)){
      colors <- light_bg_colors()
   }
   ggplot2::discrete_scale("colour", "Publication", scales::manual_pal(values = colors), ...)
}

#' Title
#'
#' @inheritParams scale_fill_publication
#' @export
#'
#' @import scales
#' @import ggplot2
scale_fill_publication_dark <- function(colors = NULL, ...) {
   if(is.null(colors)){
      colors <- dark_bg_colors()
   }
   ggplot2::discrete_scale("fill", "Publication", scales::manual_pal(values = colors), ...)
}

#' Title
#'
#' @inheritParams scale_fill_publication
#' @export
#'
#' @import scales
#' @import ggplot2
#' 
scale_colour_publication_dark <- function(colors = NULL, ...) {
   if(is.null(colors)){
      colors <- dark_bg_colors()
   }
   ggplot2::discrete_scale("colour", "Publication", scales::manual_pal(values = colors), ...)
}


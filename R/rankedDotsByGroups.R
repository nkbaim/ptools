

#' Draw dot plot with ranked order
#' @description This function draws dot plot with ranked order, if multiple 
#' groups exists, dots is ranked within each groups.
#' 
#' @param x a numeric vector or a list of numeric vectors
#' @param col a vectors of color codes for each groups
#' @param alpha the opacity of color
#' @param args.legend list of additional arguments to pass to \code{legend()}.
#' @param ... arguments to be passed to \code{plot()}.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' z <- list(
#'   NAT = round(rnorm(200, mean = 7, sd = 1) * 1000),
#'   Tumor = round(rnorm(200, mean = 9, sd = 1) * 1000)
#' )
#' rankedDotsByGroups(z, xlab = "Sample Number", pch=19, main = "Number of identified proteins")
rankedDotsByGroups <- function(x, col = NULL, alpha = 1, args.legend = NULL, ...) {
  if (is.null(col)) {
    col <- c(
      "#427FC1", "#EE67A3", "#5E803F", "#F7C756", "#94AAD8", "#CADEB8",
      "#F19737", "#F2D1B7", "#FADBF1", "#ADC9E7"
    )
  }
  
  col <- adjustcolor(col, alpha.f = alpha)
  
  if (!is.list(x)) {
    xls <- list(x)
  } else {
    xls <- x
  }
  args <- list(...)
  
  if (is.null(args$main)) args$main <- ""
  if (is.null(args$xlab)) args$xlab <- ""
  if (is.null(args$ylab)) args$ylab <- ""
  if (is.null(args$pch)) args$pch <- 17
  
  # ylim
  ylims <- sapply(xls, function(x) {
    range(x)
  })
  args$ylim <- c(min(ylims[1, ]), max(ylims[2, ]))
  
  # xlim
  args$xlim <- c(0, xlim <- length(unlist(xls)))
  args.points <- args
  
  # groups
  groups <- names(xls)
  if (is.null(groups)) {
    groups <- paste0("Group", seq_along(xls))
  }
  
  args$y <- sort(xls[[1]])
  args$x <- 1:length(xls[[1]])
  index <- length(xls[[1]])
  args$col <- col[1]
  args$frame.plot <- FALSE
  args$las <- 1
  
  do.call(plot, args = args)
  
  if (length(xls) > 1) {
    for (i in seq_along(xls)[-1]) {
      y <- sort(xls[[i]])
      x <- 1:length(y) + index
      
      args.points$x <- x
      args.points$y <- y
      args.points$col <- col[i]
      do.call(points, args = args.points)
      index <- index + length(y)
    }
    
    for (i in seq_along(xls)) {
      groups[i] <- paste0(names(xls)[i], " (n = ", length(x), ")")
    }
    
    args.legend.default <- list(
      "bottomright",
      yjust = 1,
      xjust = 0.2, legend = groups, pch = args$pch, col = col, bty = "n"
    )
    args.legend <- modifyList(args.legend.default, as.list(args.legend))
    par(xpd = T)
    do.call(legend, args = args.legend)
  }
  
#  axis(1, lwd = 2, las = 1)
#  axis(2, lwd = 2, las = 1)
}
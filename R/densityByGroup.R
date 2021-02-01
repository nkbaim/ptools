#' classic density plot
#'
#' Plot several density plot in one figure.
#'
#' @param x a numeric vector (representing one group), or a list containing
#' several numeric vectors, with each representing a group.
#' If it is a list, its names determines the group names
#' @param col colors filled under density curve.
#' @param alpha a number modifying the opacity alpha; typically in \code{[0,1]}.
#' @param args.density list of additional arguments to pass to \code{density()}.
#' names of the list are used as argument names.
#' @param args.legend list of additional arguments to pass to \code{legend()}.
#' names of the list are used as argument names.
#' @param ... arguments to be passed to \code{plot()}.
#'
#' @export
#'
#' 
#' @importFrom grDevices adjustcolor
#' @importFrom stats density runif
#' 
#' @examples
#' xls <- list(
#'     male = rnorm(100, 170, 5),
#'     female = rnorm(110, 160, 5)
#' )
#' print(str(xls))
#' #densityByGroup.classic(xls)
#' 
densityByGroup.classic <- function(x, col = NULL, alpha = .5,
                                   args.density = NULL, args.legend = NULL,...) {
  if (is.null(col)) {
    col <- c("#5E803F", "#94AAD8", "#CADEB8",
             "#F19737", "#F2D1B7", "#EE67A3", "#FADBF1",
             "#427FC1", "#ADC9E7", "#F7C756")
    col <- adjustcolor(col, alpha.f = alpha)
  }
  if (!is.list(x)) {
    xls <- list(x)
  } else xls <- x
  x <- unlist(xls)
  args0 <- list(...)
  args <- args0
  args$x <- x
  args.density <- as.list(args.density)
  args.density$x <- x
  args$x <- do.call(density, args = args.density)
  if (is.null(args$main)) args$main <- ""
  if (is.null(args$bty)) args$bty <- "l"
  if (length(xls) == 1) {
    if (!is.null(xls)) {
      args$xlab <- paste0(names(xls), " (n = ", length(x), ")")
    }
    args$col <- col[1]
    do.call(plot, args = args)
    polygon(args$x, col = col[1])
    return(NULL)
  }
  if (is.null(args$xlab)) args$xlab <- ""
  args$type <- "n"
  mar <- par("mar")
  op <- par(mar = mar)
  ylims <- sapply(xls, function(x) {
    range(density(x)$y)
  })
  print(ylims)
  args$ylim <- c(min(ylims[1, ]), max(ylims[2, ]))
  print(args)
  do.call(plot, args = args)
  groups <- names(xls)
  if (is.null(groups)) {
    groups <- paste0("Group", seq_along(xls))
  }
  for (i in seq_along(xls)) {
    x <- xls[[i]]
    args.density$x <- x
    d <- do.call(density, args = args.density)
    groups[i] <- paste0(names(xls)[i], " (n = ", length(x), ")")
    lines(d)
    colindex <- i %% length(col)
    polygon(d, col = col[ifelse(colindex == 0, length(col), colindex)])
  }
  args.legend.default <- list(
    x = par("usr")[2], y = par("usr")[4], yjust = 1,
    xjust = 0.2, legend = groups, fill = col
  )
  args.legend <- modifyList(args.legend.default, as.list(args.legend))
  par(xpd = T)
  do.call(legend, args = args.legend)
  on.exit(par(op))
}


#' Advanced density plot
#'
#' Add boxplot below density plot for each group.
#'
#' @param x a numeric vector (representing one group), or a list containing
#' several numeric vectors, with each representing a group.
#' If it is a list, its names determines the group names
#' @param col colors filled under density curve.
#' @param alpha a number modifying the opacity alpha; typically in \code{[0,1]}.
#' @param main figure title.
#' @param xlab x axis label.
#' @param args.density list of additional arguments to pass to \code{density()}.
#' names of the list are used as argument names.
#'
#' @export
#'
#' @examples
#' xls <- list(
#'     male = rnorm(100, 170, 5),
#'     female = rnorm(110, 160, 5)
#' )
#' print(str(xls))
#' densityByGroup.advance(xls, xlab = "Height (cm)", main = "Female vs Male")
#' @importFrom grDevices adjustcolor
#' @importFrom stats density
densityByGroup.advance <- function(x, col = NULL, alpha = 1, main = "", xlab = "",
                                   args.density = NULL) {
  if (is.null(col)) {
    col <- c("#5E803F", "#94AAD8", "#CADEB8",
             "#F19737", "#F2D1B7", "#EE67A3", "#FADBF1",
             "#427FC1", "#ADC9E7", "#F7C756")
    col <- adjustcolor(col, alpha.f = alpha)
  }
  if (!is.list(x)) {
    xls <- list(x)
  } else xls <- x
  groups <- names(xls)
  ng <- length(groups)
  if (is.null(groups)) {
    groups <- paste0("Group", seq_along(xls))
  }
  density.h <-2; box.h <- 2; header <- 1; tail <- 2
  hs <- c(header, rep(c(density.h, box.h), times = ng), tail)
  print(hs)
  layout.matrix <- matrix(seq(ng * 2 + 2), ncol = 1)
  print(layout.matrix)
  layout(mat = layout.matrix, heights = hs)
  mar <- par("mar")
  mar[c(1, 3)] <- 0
  op <- par(mar = mar)
  # header
  plot(1, 1, ann = F, type = "n", axes = F)
  text(1, 1, labels = main, cex = 1.5)
  print("gdsa")
  # for loop
  xrange <- range(unlist(xls))
  
  for (i in seq(ng)) {
    print(i)
    x <- xls[[i]]
    lab <- paste0(names(xls)[i], " (n = ", length(x), ")")
    # density plot
    args.density$x <- x
    den <- do.call(density, args.density)
    plot(den, xlim = xrange, ann = F, axes = F, bty = "n")
    colindex <- i %% length(col)
    col0 <- col[ifelse(colindex == 0, length(col), colindex)]
    polygon(den, col = col0)
    axis(side = 2, at = 0, labels = lab)
    abline(v = par("usr")[1], lwd = 2)
    # add jitter points
    plot(x = x, y = runif(length(x), min = 0.1, max = 1.9),
         ylim = c(0, 2), pch = 19, col =col0,
         xlim = xrange, ann = F, axes = F, bty = "n")
    # boxplot
    boxplot(x, horizontal = T, ann = F, axes = F, bty = "n", col = col0, add = T)
    abline(v = par("usr")[1], lwd = 2)
  }
  # x axis
  mar[1] <- 5.1
  par(mar = mar)
  plot(1, 1, type = "n", xlim = xrange, bty = "l", yaxt = "n", ylab = "", xlab = xlab)
  on.exit(par(op))
}


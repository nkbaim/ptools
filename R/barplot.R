#' Stack Barplot by Group
#'
#' Creates a bar plot with vertical or horizontal stacked bars clustered according to
#'  group information.
#'
#' @author Yuhao Xie
#' 
#' @param data A matrix with the sample ids as colnames and the cell types as rownames.
#' @param groupinfo A vector/factor or list specifies which group the samples belong to.
#' @param horiz a logical value. If FALSE, the bars are drawn vertically with the first
#' bar to the left. If TRUE, the bars are drawn horizontally with the first at the bottom
#' @param args.group list of additional arguments to pass to text(), which adds group labels
#'  on plot. names of the list are used as argument names.
#' @param args.legend list of additional arguments to pass to legend().
#' names of the list are used as argument names.
#' @param ... arguments to be passed to/from other methods,
#' such as \code{barplot()} or \code{par()}.
#'
#' @return A numeric vector, say mp, giving the coordinates of all the bar midpoints drawn,
#'  useful for adding to the graph.
#' @export
#' @examples
#' cellprop <- matrix(runif(15 * 6), ncol = 6)
#' rownames(cellprop) <- paste0("S", seq_len(nrow(cellprop)))
#' colnames(cellprop) <- c("B", "CD4", "CD8", "Mono/Macro", "NK", "Neutro")
#' cellprop <- cellprop/rowSums(cellprop)
#' cellprop <- t(cellprop)
#' print(cellprop)
#' groupinfo <- sample(c("Group1", "Group2", "Group3"), ncol(cellprop), replace = TRUE)
#' barplotStack(cellprop, groupinfo = groupinfo, horiz = TRUE)
#' barplotStack(cellprop, groupinfo = groupinfo)
#' @import graphics
#' @importFrom stats median
#' @importFrom utils modifyList
barplotStack <- function(data, groupinfo = NULL, horiz = F,
                          args.group = NULL, args.legend = NULL, ...) {
  if(is.null(rownames(data)) | is.null(colnames(data))) {
    stop("The row/column names should not be NULL")
  }
  if (!is.null(groupinfo)) {
    if (!is.vector(groupinfo) & !is.factor(groupinfo) & !is.list(groupinfo)) {
      stop("groupinfo should be a vector/factor or list")
    }
    if (is.vector(groupinfo)) {
      groupinfo <- factor(groupinfo)
    }
    if (is.factor(groupinfo)) {
      if (!is.null(names(groupinfo))) {
        if (length(setdiff(colnames(data), names(groupinfo))) != 0) {
          stop("All sample ids should be included in the names of groupinfo.")
        }
        groupinfo <- groupinfo[match(colnames(data), names(groupinfo))]
      }
      groupinfo <- split(colnames(data), groupinfo)
    }
    if (is.null(names(groupinfo))) {
      names(groupinfo) <- paste0("Group", seq_along(groupinfo))
    }
    nsams <- sapply(groupinfo, length)
    groups <- names(groupinfo)
    groupinfo <- unlist(groupinfo)
    if (length(groupinfo) != ncol(data)) {
      stop("The total items in groupinfo should be identical to the column number of data.")
    }
    data <- data[, groupinfo]
    space <- rep(.1, ncol(data))
    space[cumsum(nsams[-length(nsams)]) + 1] <- 1
  }
  args <- list(...)
  col <- args$col
  if (is.null(col)) {
    col <- c("#5E803F", "#94AAD8", "#CADEB8",
             "#F19737", "#F2D1B7", "#EE67A3", "#FADBF1",
             "#427FC1", "#ADC9E7", "#F7C756")
  }
  args$col <- col
  args$height <- data
  args$space <- space
  args$beside <- F
  args$legend.text <- F
  args$horiz <- horiz
  args$beside <- F
  mar <- args$mar; las <- args$las
  if (is.null(mar)) {
    mar <- par("mar")
    mar[4] <- 12
  }
  if (is.null(las)) las <- 2
  op <- par(mar = mar, xpd = T, las = las)
  bp <- do.call(barplot, args = args)
  
  # add group labels
  xs <- split(bp, f = rep(groups, nsams))
  xs <- sapply(xs, median)
  if (horiz) {
    args.group.default <- list(
      x = par("usr")[2], y = xs, labels = groups,
      cex = 1.2, srt = 270, adj = c(.5, - 1)
    )
  } else {
    args.group.default <- list(
      x = xs, y = par("usr")[4], pos = 3,
      labels = groups, cex = 1.2
    )
  }
  args.group <- modifyList(args.group.default, as.list(args.group))
  do.call(text, args = args.group)
  
  # add legend
  args.legend.default <- list(
    x = par("usr")[2], y = par("usr")[4],
    legend = rownames(data), fill = col,
    xjust = ifelse(horiz, -.5, -.2)
  )
  args.legend <- modifyList(args.legend.default, as.list(args.legend))
  do.call(legend, args = args.legend)
  on.exit(par(op))
  return(bp)
}

#barplot.stack.pdf <- function(file, width = 6, height = 6, ...) {
#  pdf(file = file, width = width, height = height)
#  do.call(barplotStack, args = list(...))
#  dev.off()
#}


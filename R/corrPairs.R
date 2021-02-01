
#' A pairs visualization of a correlation matrix.
#' @description A graphical display of a correlation matrix, based on \code{pairs()} function
#'
#' @param m a data matrix, each sample is represent by a column.
#' @param cor.method a character string indicating which correlation coefficient (or covariance) is to be computed. 
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param colors colors that mapped to the coffeicient of correlation
#' @param cor.use an optional character string giving a method for computing covariances in the presence of missing values. 
#' This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or 
#' "pairwise.complete.obs".
#' @param min.col a numeric between 0-1, specifiy the minimal correlation coffeicient, used to scale the color.
#' @param max.col a numeric between 0-1, specifiy the maximal correlation coffeicient, used to scale the color.
#' @param axt logical. Should the xaxis and yaxis be drawn
#' @param cex.coeff graphics parameters for the correlation coefficient plot panel, defines the font size
#' @param cex.point graphics parameters for the scatter plot panel, defines the point size
#' @param cex.labels graphics parameters for the text panel, defines the font size
#' @param cex.main graphics parameters for the title, defines the font size
#' @param line.main if \code{main} is specified, \code{line.main} gives the line argument to [mtext()] which draws the title. 
#' You may want to specify oma when changing line.main.
#' @param ... other parameters pass to \code{pairs()} function
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cor
#' @importFrom graphics pairs
#' @export
#'
#' @examples
#' x <- rnorm(n = 100, mean = 10, sd = 2)
#' df <- data.frame(x = x + rnorm(100))
#' for (i in 1:9) {
#'   df <- cbind(df, x + rnorm(100))
#' }
#' colnames(df) <- paste0(LETTERS[1:10], 1:10)
#'
#' corrPairs(df, axt = FALSE)
corrPairs <- function(m, cor.method = c("pearson", "kendall", "spearman"), colors = colorRampPalette(c("#CCCCCC", "#EF3B3C"))(50),
                      cor.use = c("all.obs", "complete.obs", "pairwise.complete.obs", "everything", "na.or.complete"), max.col = NULL,
                      min.col = NULL, axt = FALSE, cex.coeff = NULL, cex.point = 1, cex.labels = 1, cex.main = 1, line.main = 3, ...) {
  cor.method <- match.arg(cor.method)
  cor.use <- match.arg(cor.use)

  m.cor <- cor(m, method = cor.method, use = cor.use)
  cor.median <- round(median(rmDiag(m.cor)), digits = 2)
  cor.min <- round(min(rmDiag(m.cor)), digits = 2)
  cor.max <- round(max(rmDiag(m.cor)), digits = 2)
  
  if (is.null(min.col)) {
    min.col <- cor.min - 0.01
  }
  
  if (is.null(max.col)) {
    max.col <- cor.max + 0.01
  }
  
  usr <- par("usr")
  xaxt <- par("xaxt")
  yaxt <- par("yaxt")
  on.exit(par(list(usr = usr, xaxt = xaxt, yaxt = yaxt)))
  
  panel.cor <- function(x, y) {
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y, method = cor.method, use = cor.use), digits = 2)
    col.temp <- colors[round((r - min.col) / (max.col - min.col) * 50 + 1)]
    rect(0, 0, 1, 1, col = col.temp)
    txt <- sprintf("%.2f", r)
    if (is.null(cex.coeff)) cex.coeff <- 0.6 / strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.coeff)
  }
  
  upper.panel <- function(x, y) {
    r <- round(cor(x, y, method = cor.method, use = cor.use), digits = 2)
    col.temp <- colors[round((r - min.col) / (max.col - min.col) * 50 + 1)]
    reg <- par("usr")
    rect(reg[1], reg[3], reg[2], reg[4], col = col.temp)
    points(x, y, pch = 19, cex = cex.point)
  }
  
  if (!axt) {
    par(xaxt = "n", yaxt = "n")
    line.main <- line.main - 1
  }
  
  pairs(m,
        lower.panel = panel.cor, upper.panel = upper.panel, gap = 0, cex.labels = cex.labels,
        main = paste0(
          "Median correlation:", sprintf("%.2f", cor.median),
          " (range ", sprintf("%.2f", cor.min), "-", sprintf("%.2f", cor.max), ")"
        ), cex.main = cex.main, line.main = line.main, ...
  )
}
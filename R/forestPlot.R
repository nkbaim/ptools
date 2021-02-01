


#' Draw forest plot for differences betweeen cohorts.
#' 
#' @description Draw forest plot for mutation differences analysis betweeen two cohorts.
#' @details The  x-axis is log10 converted odds ratio and y-axis is differentially mutated genes 
#'
#' @param m1 first MAF object
#' @param m2 second MAF object
#' @param m1Name optional name for first cohort
#' @param m2Name optional name for second cohort
#' @param minMut Consider only genes with minimum this number of samples mutated in atleast one of the cohort for analysis. Helful to ignore single mutated genes. Default 5.
#' @param pVal p-value threshold. Default 0.05.
#' @param fdr fdr threshold. Default NULL. If provided uses adjusted pvalues (fdr).
#' @param color vector of colors for cohorts. Default NULL.
#' @param geneFontSize Font size for gene symbols. Default 1.2
#' @param titleSize font size for titles. Default 1.2
#' @param lineWidth line width for CI bars. Default 2.2
#'
#' @export 
#' @importFrom maftools mafCompare read.maf forestPlot
#' @examples
#' primary.apl <- system.file("extdata", "APL_primary.maf.gz", package = "maftools")
#' relapse.apl <- system.file("extdata", "APL_relapse.maf.gz", package = "maftools")
#' forestDMG(primary.apl, relapse.apl, m1Name = "Primary", m2Name = "Relapse", lineWidth = 3)
forestDMG <- function(m1, m2, m1Name = NULL, m2Name = NULL, minMut = 5, pVal = 0.05, 
                       fdr = NULL, color = NULL,geneFontSize = 1.2, titleSize = 1.2,
                       lineWidth = 2.2) {
  
  maf1 <- maftools::read.maf(maf = m1)
  maf2 <- maftools::read.maf(maf = m2)
  ##Perform analysis and draw forest plot.
  group1.vs.group2 <- maftools::mafCompare(m1 = maf1, m2 = maf2, m1Name = m1Name,
                                           m2Name = m2Name, minMut = minMut)
  maftools::forestPlot(mafCompareRes = group1.vs.group2, 
                       pVal = pVal, 
                       fdr = fdr, 
                       color = color,
                       geneFontSize = geneFontSize,
                       titleSize = titleSize,
                       lineWidth = lineWidth)
}
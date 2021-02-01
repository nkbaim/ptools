light_bg_colors <- function() {
  return(c("#386cb0", "#f87f01", "#7fc97f", "#ef3b2c", 
           "#feca01", "#a6cee3", "#fb9a99", "#984ea3", "#8C591D"))
}

dark_bg_colors <- function() {
  return(c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", 
           "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2"))

}

# remove diagonal of a matrix
rmDiag <- function(x) {
  v <- vector()
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      if (i == j) {
        next
      }
      v <- c(v, x[i, j])
    }
  }
  return(v)
}
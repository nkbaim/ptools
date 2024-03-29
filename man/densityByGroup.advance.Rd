% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/densityByGroup.R
\name{densityByGroup.advance}
\alias{densityByGroup.advance}
\title{Advanced density plot}
\usage{
densityByGroup.advance(
  x,
  col = NULL,
  alpha = 1,
  main = "",
  xlab = "",
  args.density = NULL
)
}
\arguments{
\item{x}{a numeric vector (representing one group), or a list containing
several numeric vectors, with each representing a group.
If it is a list, its names determines the group names}

\item{col}{colors filled under density curve.}

\item{alpha}{a number modifying the opacity alpha; typically in \code{[0,1]}.}

\item{main}{figure title.}

\item{xlab}{x axis label.}

\item{args.density}{list of additional arguments to pass to \code{density()}.
names of the list are used as argument names.}
}
\description{
Add boxplot below density plot for each group.
}
\examples{
xls <- list(
    male = rnorm(100, 170, 5),
    female = rnorm(110, 160, 5)
)
print(str(xls))
densityByGroup.advance(xls, xlab = "Height (cm)", main = "Female vs Male")
}
\author{
Yuhao Xie
}

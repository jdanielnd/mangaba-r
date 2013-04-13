\name{mangaba-package}
\alias{mangaba-package}
\alias{mangaba}
\docType{package}
\title{
Provides an enseble forecast function
}
\description{
Provides an enseble forecast function
}
\details{
\tabular{ll}{
Package: \tab mangaba\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-03-11\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Joao Daniel <jdanielnd@gmail.com>

Maintainer: Joao Daniel <jdanielnd@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\keyword{ package }
\examples{
  x = (1:12)*100 + rnorm(12)*100
  start = c(2010,1)
  ens.forecast(x, start, 12, 95)
}

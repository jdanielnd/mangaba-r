\name{ens.forecast}
\alias{ens.forecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ensembled forecast
}
\description{
  Generate ensembled forecast
}
\usage{
ens.forecast(x, start, frequency, level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of time series data
}
  \item{start}{
start date of the time series
}
  \item{frequency}{
time series frequency
}
  \item{level}{
confidence level for forecast
}
}
\value{
\item{upper }{Upper forecast band}
\item{point }{Point forecast}
\item{lower }{Lower forecast band}
}
\author{
Joao Daniel <jdanielnd@gmail.com>
}

\examples{
  x = (1:12)*100 + rnorm(12)*100
  start = c(2010,1)
  ens.forecast(x, start, 12, 95)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

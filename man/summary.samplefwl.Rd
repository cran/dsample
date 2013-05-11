\name{summary.samplefwl}
\alias{summary.samplefwl}

\title{
Generating Basic Summary Statistics of Marginal Distributions
}

\description{
Producing basic summary statistics (the mean, the standard deviation and the first five modes) from the sample drawn via either the Fu-Wang algorithm or the Wang-Lee algorithm, for all marginal distributions of the target distribution.
}

\usage{
\method{summary}{samplefwl}(object, ...)
}

\arguments{
  \item{object}{
a \code{data.frame}, contains the sample drawn via either the Fu-Wang algorithm or the Wang-Lee algorithm 
}
  \item{\dots}{
dot-dot-dot arguments
}
}

\author{
Liqun Wang and Chel Hee Lee
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    cat("\nMeans: \n")
    print(round(sapply(object, mean), 4))
    cat("\nStandard deviations: \n")
    print(round(sapply(object, mean), 4))
    cat("\nFirst five modes: \n")
    print(round(object[1:5, ], 4))
  }
}


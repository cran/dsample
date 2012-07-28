\name{dsample-package}
\alias{dsample-package}
\docType{package}

\title{
Discretization-based Direct Random Sample Generation
}

\description{
The package provides two discretization-based Monte Carlo algorithms, namely the Fu-Wang algorithm and the Wang-Lee algorithm, for random sample generation from a high dimensional distribution of complex structure. The normalizing constant of the target distribution needs not to be known.
}

\details{
\tabular{ll}{
Package: \tab dsample\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2012-07-28\cr
License: \tab GPL-3\cr
}
}

\author{
Liqun Wang and Chel Hee Lee

Maintainer: Chel Hee Lee <gnustats@gmail.com>
}

\references{
Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.

Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48, 631-653.

Wang, L. and Lee, C.H. (2008). A discretization-based efficient random sampling algorithm. Working Paper, Department of Satistics, University of Manitoba.
}

\keyword{ package }

\examples{
## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the 
## Royal Statistical Society - B, 55, 409-422.

## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours.  The default value for the number of discretization points 
## is 1e7 and for the number of contours is 1e5

x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

## Applying the Fu-Wang algorithm
out1 <- sample.fw(X=support, nc=1e4, n=1e3)
output(out1)

## Applying the Wang-Lee algorithm
out2 <- sample.wl(X=support, nc=1e4, n=1e3)
output(out2)
}

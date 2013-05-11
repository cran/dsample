\name{sample.wl}
\alias{sample.wl}

\title{
Random Samples Generation Through The Wang-Lee Algorithm
}

\description{
\code{sample.wl} generates a sample of specified size \code{n} from the target density funciton (up to a normalizing constant) based on the Wang-Lee algorithm
}

\usage{
sample.wl(X, nc = 10000, n = 1000, wconst = NULL)
}

\arguments{
  \item{X}{
a \code{data.frame} or a \code{matrix}.  See \sQuote{Details}.
}
  \item{nc}{
a positive integer, the number of contours.  See \sQuote{Details}.
}
  \item{n}{
a non-negative integer, the desired sample size. 
}
  \item{wconst}{
a real number between 0 and 1.  See \sQuote{Details}.
}
}

\details{
\code{X} has the number of rows equals to the number of discrete base points. In each row, the first element contians the funcitonal value of the target density and the rest elements are the coordinates at which the density is evaluated.

\code{wconst} is a constant for adjusting the volumn of the last contour.
}

\value{
\code{sample.wl} gives the drawn sample as a \code{data.frame} with number of rows equals the specified size \code{n} and number of columns equals \code{ncol(x)-1}.
}

\references{
Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.

Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48(4):631-653.

Wang, L. and Lee, C.H. (2008). A discretization-based efficient random sampling algorithm (Working paper), Department of Satistics, University of Manitoba, Canada
}

\author{
Liqun Wang and Chel Hee Lee 
}

\note{
\code{\link{sample.fw}}
}

\examples{
## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the Royal
##   Statistical Society - B, 55, 409-422.


## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours.  The default value of the number of discretization points 
## is 1e7 and for the number of contours is 1e5


x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

summary(sample.fw(X=support, nc=1e4, n=1e3))
}


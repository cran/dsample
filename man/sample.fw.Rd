\name{sample.fw}
\alias{sample.fw}

\title{
Random Sample Generation Through The Fu-Wang Algorithm
}

\description{
\code{sample.fw} generates a sample of size \code{n} from the target density funciton (up to a normalizing constant) using the Fu-Wang algorithm
}

\usage{
sample.fw(X, nc = 10000, n = 1000)
}

\arguments{
  \item{X}{
a \code{data.frame} or a \code{matrix}.  See \sQuote{Details}.
}
  \item{nc}{
a positive integer, the number of contours.  See \sQuote{Details}.
}
  \item{n}{
a non-negative integer giving the user specified sample size.
}
}

\details{
\code{X} has the number of rows equals to the number of discrete base points. In each row, the first element contians the funcitonal value of the target density and the rest elements are the coordinates at which the density is evaluated.
}

\value{
\code{sample.fw} gives the drawn sample as a \code{data.frame} with the number of rows equals to the specified size \code{n} and number of columns equals to \code{ncol(x)-1}.
}

\references{
Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.

Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48(4):631-653.

Wang, L. and Lee, C.H. (2008). A discretization-based efficient random sampling algorithm (Working paper), Department of Satistics, University of Manitoba, Canada
}

\author{
Liqun Wang and Chel Hee Lee 
}

\seealso{
\code{\link{sample.wl}}
}

\examples{
## The following example is taken from West (1993, page 414).
## West, M. (1993). Approximating posterior distributions by mixture. Journal of the Royal
##   Statistical Society - B, 55, 409-422.

## More accurate results can be achieved by increasing the number of dicretization points 
## and the number of contours. The default value for the number of discretization points 
## is 1e7 and for the number of contours is 1e5.


x1 <- runif(1e5)
x2 <- runif(1e5)
val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
support <- as.data.frame(cbind(val, x1, x2))

summary(sample.fw(X=support, nc=1e4, n=1e3))
}


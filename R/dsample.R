# Created on March 18, 2009
# Modified on July 01, 2015
# 
# This work is based on the Chel Hee Lee's MSc thesis work at the 
# Department of Statistics, University of Manitoba in 2009
#

#' @rdname dsample
#'
#' @title Random Samples Generation Through The Wang-Lee and Fu-Wang Algorithms
#' @description \code{sample.wl} generates a sample of specified size \code{n} from the target density funciton (up to a normalizing constant) based on the Wang-Lee algorithm
#'
#' @param  X      must be a \code{data.frame}.  See \sQuote{Details}.
#' @param method wl (Wang-Lee), fw (Fu-Wang)
#' @param  nc     a positive integer, the number of contours.  See \sQuote{Details}.
#' @param  n      a non-negative integer, the desired sample size. 
#' @param  wconst a real number between 0 and 1.  See \sQuote{Details}.
#' 
#' @details  \code{X} has the number of rows equals to the number of discrete base points. In each row, the first element contians the funcitonal value of the target density and the rest elements are the coordinates at which the density is evaluated.  
#'
#' \code{wconst} is a constant for adjusting the volumn of the last contour.
#'
#' @return \code{sample.wl} gives the drawn sample as a \code{data.frame} with number of rows equals the specified size \code{n} and number of columns equals \code{ncol(x)-1}.
#'
#' @references
#' Wang, L. and Lee, C.H. (2014). Discretization-based direct random sample generation. Computational Statistics and Data Analysis, 71, 1001-1010. 
#'
#' Lee, C.H. (2009). Efficient Monte Carlo Random Sample Generation through Discretization, MSc thesis, Department of Satistics, University of Manitoba, Canada
#'
#' Wang, L. and Fu, J. (2007). A practical sampling approach for a bayesian mixture model with unknown number of components. Statistical Papers, 48(4):631-653.
#'
#' Fu, J. C. and Wang, L. (2002). A random-discretization based Monte Carlo sampling method and its application. Methodology and Computing in Applied Probability, 4, 5-25.
#' 
#' @author 
#' Chel Hee Lee \email{chl948@@mail.usask.ca}, Liqun Wang \email{liqun.wang@@umanitoba.ca}
#' 
#' @keywords sampling, discretization
#'
#' @examples 
#' ## The following example is taken from West (1993, page 414).
#' ## West, M. (1993). Approximating posterior distributions by mixture.
#' ##   Journal of the Royal Statistical Society - B, 55, 409-422.
#' 
#' x1 <- runif(1e5)
#' x2 <- runif(1e5)
#' val <- (x1*(1-x2))^5 * (x2*(1-x1))^3 * (1-x1*(1-x2)-x2*(1-x1))^37
#' support <- as.data.frame(cbind(val, x1, x2))
#' 
#' summary(dsample(X=support, method="wl", nc=1e4, n=1e3))
#'
#' summary(dsample(X=support, method="fw", nc=1e4, n=1e3))
#' 
#' ##
#' ## More accurate results can be achieved by increasing the number 
#' ## of dicretization points and the number of contours.  
#' @export 

dsample <- function(X, method=c("wl", "fw"), nc=1e4, n=1e3, wconst=NULL) {

	stopifnot(is.data.frame(X), !missing(X), nrow(X) > nc)
	
	if (method == "wl") {
		X <- X[which(X[,1]>0),]
		measure <- graphics::hist(X[,1], breaks=seq(from=min(X[,1]), to=max(X[,1]), length.out=nc+1), plot=FALSE)
		gpdf <- rev(measure$counts * measure$mids)
		if(!is.null(wconst)) gpdf[nc] <- wconst*gpdf[nc]
		cdf <- c(0, cumsum(gpdf)/sum(gpdf))
		pps <- graphics::hist(stats::runif(n), breaks=cdf, plot=FALSE)$counts
		scnt <- mapply(sample, MoreArgs=list(replace=TRUE), rev(measure$counts), pps);
		sind <- unlist( mapply("+", as.list( c(0, cumsum(rev(measure$counts)))[-(nc+1)] ), scnt) )
		X <- X[order(X[,1], decreasing=TRUE)[sind], -1]
	}
	
	if (method == "fw") {
		ndp <- nrow(X)
		X <- X[order(X[,1], decreasing=TRUE), ]
		X$g <- gl(nc, ndp/nc)	
		clvls <- tapply(X[,1], X$g, mean)
		cdf <- c(0, cumsum(clvls)/sum(clvls))
		pps <- graphics::hist(stats::runif(n), breaks=cdf, plot=FALSE)$counts	
		scnt <- mapply(sample, MoreArgs=list(replace=TRUE), rep(ndp/nc, nc), pps)
		sind <- unlist( mapply("+", as.list( c(0, cumsum(rep(ndp/nc, nc)))[-(nc+1)] ), scnt) )
		X <- X[sind, c(-1, -length(names(X)))]
	}
	
	X <- as.data.frame(X)
	row.names(X) <- NULL
	
	class(X) <- c("dsample", "data.frame")
	invisible(X)
}


#' @rdname summary.dsample
#' 
#' @title Generating Basic Summary Statistics of Marginal Distributions
#'
#' @description  Producing basic summary statistics (the mean, the standard deviation and the first five modes) from the sample drawn via either the Fu-Wang algorithm or the Wang-Lee algorithm, for all marginal distributions of the target distribution.
#'
#' @param object a \code{data.frame}, contains the sample drawn via either the Fu-Wang algorithm or the Wang-Lee algorithm 
#' @param n the first n samples
#' @param digits a length of valid numbers
#' @param ... more arguments
#'
#' @author Chel Hee Lee \email{chl948@@mail.usask.ca}, Liqun Wang \email{liqun.wang@@umanitoba.ca}
#'
#' @export 
summary.dsample <- function(object, n=5, digits=4, ...) {

	stopifnot(is.data.frame(object))
	
	cat("\nMeans: \n")
	print(round(sapply(object, mean), digits))

	cat("\nStandard deviations: \n")
	print(round(sapply(object, mean), digits))

	cat("\nFirst five modes: \n")
	print(round(object[1:n, ], digits))
}


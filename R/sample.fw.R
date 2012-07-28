sample.fw <-
function(X, nc=1e4, n=1e3)
{
	cat("Note: \n")
	cat("This sampling algorithm may take from a few minutes to a few hours \n")

	if(is.data.frame(X) | is.matrix(X)) X <- as.data.frame(X)
	else stop("\'X\' must be either a matrix or a data.frame.\n")
	
	ndp <- nrow(X)
	X <- X[order(X[,1], decreasing=TRUE), ]
	X$g <- gl(nc, ndp/nc)	
	clvls <- tapply(X[,1], X$g, mean)
	cdf <- c(0, cumsum(clvls)/sum(clvls))
	pps <- hist(runif(n), breaks=cdf, plot=FALSE)$counts	
	scnt <- mapply(sample, MoreArgs=list(replace=TRUE), rep(ndp/nc, nc), pps)
	sind <- unlist( mapply("+", as.list( c(0, cumsum(rep(ndp/nc, nc)))[-(nc+1)] ), scnt) )
	X <- X[sind, c(-1, -length(names(X)))]
	row.names(X) <- NULL
	return(X)
}

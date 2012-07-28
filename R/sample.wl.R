sample.wl <-
function(X, nc=1e4, n=1e3, wconst=NULL)
{
	cat("Note: \n")
	cat("This sampling algorithm may take from a few minutes to a few hours \n")

	if(is.data.frame(X) | is.matrix(X)) X <- as.data.frame(X)
	else stop("\'X\' must be either a matrix or a data.frame.\n")

	X <- X[which(X[,1]>0),]
	measure <- hist(X[,1], breaks=seq(from=min(X[,1]), to=max(X[,1]), length.out=nc+1), plot=FALSE)
	gpdf <- rev(measure$counts * measure$mids)
	if(!is.null(wconst)) gpdf[nc] <- wconst*gpdf[nc]
	cdf <- c(0, cumsum(gpdf)/sum(gpdf))
	pps <- hist(runif(n), breaks=cdf, plot=FALSE)$counts
	scnt <- mapply(sample, MoreArgs=list(replace=TRUE), rev(measure$counts), pps);
	sind <- unlist( mapply("+", as.list( c(0, cumsum(rev(measure$counts)))[-(nc+1)] ), scnt) )
	X <- X[order(X[,1], decreasing=TRUE)[sind], -1]
	row.names(X) <- NULL
	return(X)
}

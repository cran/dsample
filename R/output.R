output <-
function(x)
{
	if(!is.data.frame(x)) x <- as.data.frame(x)
	line.stars <- paste(rep("*", 50), collapse="")
	cat(line.stars, "\n")
	cat("MEANS \n")
	print(formatC(sapply(x, mean), format="fg", digits=4), quote=FALSE)

	cat(line.stars, "\n")
	cat("STANDARD DEVIATIONS \n")
	print(formatC(sapply(x, sd), format="fg", digits=4), quote=FALSE)

	cat(line.stars, "\n")
	cat("The FIRST MODES \n")
	print(x[1:5, ])
	cat(line.stars, "\n")
}

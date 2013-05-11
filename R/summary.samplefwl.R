summary.samplefwl <-
function(object, ...)
{
	cat("\nMeans: \n")
	print(round(sapply(object, mean), 4))

	cat("\nStandard deviations: \n")
	print(round(sapply(object, mean), 4))

	cat("\nFirst five modes: \n")
	print(round(object[1:5, ],4))
}

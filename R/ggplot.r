#' ggplot.PandaLog
#'
#' Plot data from a single Panda log file
#'
#' @param	data	PandaLog object (obtained from \link{PandaLog})
#'
#' @export
ggplot.PandaLog <- function(data, tz='GMT', duration=ddays(7), ...) {

	# Extract data.frame with clock values in appropriate time zone
	long <- as.data.frame(data, tz=tz)

	# Select only the most recent values
	most_recent <- max(long$clock)
	selected <- subset(long, clock >= most_recent - duration)

	# Scale light values from 0-1023 to 0-100%
	i <- which(long$variable == 'light')
	long[i,'value'] <- long[i,'value'] / 1023
	
	# Match units to variables
	unit_lookup <- list(
		humid = "% RH",
		light = "%",
		shinyei = "%",
		temp = "Celsius"
	)
	selected$units <- factor(unlist(unit_lookup[selected$variable]))

	require(ggplot2)
	fig <- ggplot(selected, aes(clock, value))
	fig <- fig + scale_clock()
	fig <- fig + scale_y_continuous('')
	fig <- fig + geom_point(aes(color=variable))
	fig <- fig + facet_grid(units + variable ~ ., scales='free_y')
	fig <- fig + theme(legend.position='none')
	return(fig)
}
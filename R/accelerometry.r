#' accelerometry
#'
#' Extract accelerometry data from PandaLod
#'
#' @param	x	PandaLog object
#' @export
accelerometry <- function(x, ...) {
	raw <- subset(x$records, variable == 'accel')
	require(stringr)
	samples <- str_extract_all(raw$value, '-?\\d+,-?\\d+,-?\\d+')
	collapsed <- sapply(samples, paste, sep='', collapse=',')
	tokens <- str_split(collapsed, pattern=',')
	values <- lapply(tokens, as.integer)
	matrices <- lapply(values, matrix, byrow=TRUE, ncol=3)
	ADC <- do.call(rbind, matrices)
	colnames(ADC) <- c('x', 'y', 'z')
	
	full_scale <- as.integer(sub('# accel: ADC - (\\d+)', '\\1', x$header[3]))
	voltage <- ADC / 1023 * 5.0
	voltage <- as.data.frame(voltage)
	voltage$magnitude <- sqrt(rowSums(voltage^2))

	ticks <- seq(0, 10, by=1/3)[1:30]
	voltage$elapsed <- as.vector(outer(ticks, raw$elapsed, FUN="+"))
		
	attr(voltage, 'full_scale') <- full_scale
	attr(voltage, 'units') <- 'volts'
	return(voltage)
	
}
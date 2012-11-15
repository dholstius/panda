#' PandaLog
#'
#' Convert log file from SD card to Log object
#'
#' @param	file	path to file
#' @export
PandaLog <- function(file, ...) {
	header <- readLines(file, 12)
	records <- read.csv(file, header=FALSE, sep='\t', comment.char='#', as.is=3)
	names(records) <- c('elapsed', 'variable', 'value')
	records <- subset(records, variable != 'wait')
	object <- list(header = header, records = records)
	class(object) <- 'PandaLog'
	return(object)
}

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

#' as.data.frame.PandaLog
#'
#' Convert Log to data.frame in long format
#'
#' @param	x	PandaLog object
#' @export
as.data.frame.PandaLog <- function(x, ...) {

	# Drop accelerometry data (TODO: include these)
	long <- subset(x$records, variable != 'accel', drop = TRUE)
	
	# Parse timestamps
	require(stringr)
	require(fasttime)
	clock <- long[long$variable == 'clock', 'value']
	clock <- str_replace(clock, ' GMT$', '')
	clock <- fastPOSIXct(clock)
	
	# Reshape to wide format
	require(reshape2)
	long$index <- rep(clock, each=5)
	wide <- dcast(long, index ~ variable, value.var='value')
	
	# Drop 'clock' column and rename 'index' column to 'clock'
	wide$clock <- NULL
	names(wide)[names(wide)=='index'] <- 'clock'
	
	# Convert all other columns to numeric
	wide[,-1] <- lapply(wide[,-1], as.numeric)

	# Return in wide format
	return(wide)
	
	# Reshape back to long format
	# return(long <- melt(wide, id.vars=c('index')))
}
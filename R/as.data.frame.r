#' as.data.frame.PandaLog
#'
#' Convert Log to data.frame in long format
#'
#' @param	x	PandaLog object
#' @export
as.data.frame.PandaLog <- function(x, tz, ...) {

	# Drop accelerometry data (TODO: include these)
	long <- subset(x$records, variable != 'accel', drop = TRUE)
	
	# Parse timestamps
	require(stringr)
	require(fasttime)
	require(lubridate)
	i <- which(long$variable == 'clock')
	clock <- long[i, 'value']
	clock <- str_replace(clock, ' GMT$', '')
	clock <- fastPOSIXct(clock)
	clock <- with_tz(clock, tz=tz)
	
	# For each record, use the most recent clock time as the index.
	# This ignores the fact that sensors are read sequentially, but
	# leads to a tidy "wide" format.
	long$clock <- rep(clock, times=diff(c(i, nrow(long) + 1)))
	long <- long[-i,]

	# Cast variable column to character
	long <- within(long, {
		variable <- as.character(variable)
	})
	
	# Filter invalid records
	suppressWarnings({
		decimal_pattern <-  '^[-+]?[0-9]*\\.?[0-9]+$'
		valid <- str_detect(long$value, decimal_pattern)
		valid <- valid & str_detect(long$variable, '[a-z]+')
		valid[is.na(valid)] <- FALSE
	})
	cleaned <- subset(
		long, 
		valid & variable %in% c('temp', 'humid', 'light', 'shinyei', 'accel'),
		select = c('clock', 'variable', 'value'), 
		drop = TRUE
	)		
	
	# Cast variable column to factor and value column to numeric
	cleaned <- within(cleaned, {
		variable <- as.factor(variable)
		value <- as.numeric(value)
	})

}
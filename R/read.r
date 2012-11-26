#' PandaLog
#'
#' Convert log file from SD card to Log object
#'
#' @param	file	path to file
#' @export
PandaLog <- function(file, ...) {
	header <- readLines(file, 12)
	records <- read.csv(file, header=FALSE, sep='\t', comment.char='#', as.is=3, stringsAsFactors=FALSE)
	names(records) <- c('elapsed', 'variable', 'value')
	records <- subset(records, variable != 'wait')
	object <- list(header = header, records = records)
	class(object) <- 'PandaLog'
	return(object)
}



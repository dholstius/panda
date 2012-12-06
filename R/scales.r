#' seq_range
#'
#' Like \link{seq}, but takes a duple instead of two values
#'
#' @param	x		a vector of the form c(from, to)
#' @param	...		further arguments to \link{seq}
#' @export
seq_range <- function(x, ...) {
	seq(from=min(x), to=max(x), ...)
}

#' time_breaks
#'
#' Workaround: date_breaks() doesn't seem to support "mins" or "secs" 
#'
#' @param	width   days, hours, minutes, etc.
#' @export
time_breaks <- function(width) {
  function(limits) seq_range(as.POSIXct(limits), by=width)
}

#' scale_x_hours
#'
#' Subclass of sorts for \link[ggplot2]{scale_x_datetime}, giving breaks in hours
#'
#' @param	...				arguments to \link[ggplot2]{scale_x_datetime}
#' @param	breaks			breaks
#' @param	minor_breaks	minor_breaks
#' @param	labels			labels
#' @export
scale_x_hours <- function(
  ..., 
  breaks = date_breaks(width='1 hour'), 
  minor_breaks = time_breaks(width='15 mins'),
  labels = date_format('%I%p\n%b %d')
) {
	scale_x_datetime(..., breaks=breaks, minor_breaks=minor_breaks, labels=labels)
}

#' scale_x_days
#'
#' Subclass of sorts for \link[ggplot2]{scale_x_datetime}, giving breaks in days
#'
#' @param	...				arguments to \link[ggplot2]{scale_x_datetime}
#' @param	breaks			breaks
#' @param	minor_breaks	minor_breaks
#' @param	labels			labels
#' @export
scale_x_days <- function(
  ..., 
  breaks = date_breaks(width='1 day'), 
  minor_breaks = time_breaks(width='3 hours'),
  labels = date_format('%I%p\n%b %d')
) {
	scale_x_datetime(..., breaks=breaks, minor_breaks=minor_breaks, labels=labels)
}
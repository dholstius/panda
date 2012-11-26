daily_breaks <- function(by) {
  function (limits) {
    lower <- trunc(min(limits), 'day')
    upper <- trunc(max(limits) + 60 * 60 * 24, 'day')
    seq(from=lower, to=upper, by=by)
  }
}

require(scales)
scale_clock <- function(...) {
	labels <- date_format('%I%p\n%b %d')
	breaks <- daily_breaks('6 hours')
	minor_breaks <- '1 hour'
	scale_x_datetime(name='', ..., breaks=breaks, minor_breaks=minor_breaks, labels=labels)
}
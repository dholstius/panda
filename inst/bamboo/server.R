library(shiny)
library(panda)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  log <- reactive(function() {
    if (!is.null(input$filename)) {
      path <- input$filename$datapath
    } else {
      path <- system.file('extdata', '20121113-103.log', package='panda')
    }
    PandaLog(path)
  })
    
  long_data <- reactive(function() {
    as.data.frame(log(), tz=tz())
  })

  tz <- reactive(function() {
    input$tz
  })

  window <- reactive(function() {
    clock <- long_data()$clock
    parse_date <- function(x) as.POSIXct(strptime(x, format='%Y-%m-%d', tz=tz()))
    start <- parse_date(input$start)
    finish <- parse_date(input$finish) + 24 * 60 * 60
    if (is.na(start)) start <- min(clock)
    if (is.na(finish)) finish <- max(clock)
    c(start, finish)
  })

  wide_data <- reactive(function() {
    require(reshape2)
    dcast(long_data(), clock ~ variable)
  })

  major_breaks <- reactive(function() {
    input$major_breaks
  })

  minor_breaks <- reactive(function() {
    input$minor_breaks
  })

  selected <- reactive(function() {
    w <- window()
	subset(long_data(), clock >= w[1] & clock < w[2])  
  })

  annotate_units <- function(long_data) {
  	unit_lookup <- list(humid="% RH", light="%", shinyei="%", temp="Celsius")
	long_data$units <- factor(unlist(unit_lookup[long_data$variable]))
	return(long_data)
  }

  output$tsPlot <- reactivePlot(function() {
	require(ggplot2)
	require(scales)
	require(panda)
	dat <- annotate_units(selected())
	fig <- ggplot(dat, aes(clock, value))
	fig <- fig + scale_x_hours('', breaks=date_breaks(major_breaks()), minor_breaks=time_breaks(minor_breaks()))
	fig <- fig + scale_y_continuous('')
	fig <- fig + geom_point(aes(color=variable), size=1.5, alpha=0.7)
	fig <- fig + facet_grid(units + variable ~ ., scales='free_y')
	fig <- fig + theme(legend.position='none')
	print(fig)
  })
 
  output$dailyMeansTable <- reactiveTable(function() {
    dat <- transform(long_data(), date=as.Date(clock))
    daily_means <- dcast(dat, date ~ variable, mean, margins='date')
    format(daily_means, digits=3)
  })
  
})

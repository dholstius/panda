\name{scale_x_hours}
\alias{scale_x_hours}
\title{scale_x_hours}
\usage{
  scale_x_hours(...,
    breaks = date_breaks(width = "1 hour"),
    minor_breaks = time_breaks(width = "15 mins"),
    labels = date_format("\%I\%p\n\%b \%d"))
}
\arguments{
  \item{...}{arguments to \link[ggplot2]{scale_x_datetime}}

  \item{breaks}{breaks}

  \item{minor_breaks}{minor_breaks}

  \item{labels}{labels}
}
\description{
  Subclass of sorts for \link[ggplot2]{scale_x_datetime},
  giving breaks in hours
}


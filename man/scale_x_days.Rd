\name{scale_x_days}
\alias{scale_x_days}
\title{scale_x_days}
\usage{
  scale_x_days(..., breaks = date_breaks(width = "1 day"),
    minor_breaks = time_breaks(width = "3 hours"),
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
  giving breaks in days
}


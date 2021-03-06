% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations.R
\name{get_locations}
\alias{get_locations}
\title{Get location ids from Senaps}
\usage{
get_locations(groups = NULL, near = NULL, radius = NULL)
}
\arguments{
\item{groups}{group ids}

\item{near}{A WKT string eg "POINT (lat lon)"}

\item{radius}{The distance (in m) around the near point to search}
}
\value{
A vector of location ids
}
\description{
Get location ids from Senaps
}

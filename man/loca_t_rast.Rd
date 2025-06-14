% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loca_t_rast.R
\name{loca_t_rast}
\alias{loca_t_rast}
\title{Load LOCA2 tmax and tmin files into rasters}
\usage{
loca_t_rast(filepath)
}
\arguments{
\item{filepath}{Character file path where files are located. This should
be the folder in which the climate scenario folders are contained (historical,
ssp585, etc.). For example, "ACCESS-CM2/0p0625deg/r1i1p1f1" would be
valid for this argument.}
}
\value{
A list of length two - the tmax raster stack and the tmin raster
stack.
}
\description{
Load LOCA2 tmax and tmin files into rasters
}

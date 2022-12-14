% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{baltimore_blocks}
\alias{baltimore_blocks}
\title{U.S. Census Blocks - 2020}
\format{
A data frame with 13,598 rows and 9 variables:
\describe{
\item{\code{tractce10}}{Tract FIPS}
\item{\code{blockce10}}{Block FIPS}
\item{\code{geoid10}}{Block GeoID}
\item{\code{name10}}{Block name}
\item{\code{aland10}}{Land area}
\item{\code{awater10}}{Water area}
\item{\code{intptlat10}}{Interior center point latitude}
\item{\code{intptlon10}}{Interior center point longitude}
\item{\code{geometry}}{Multipolygon with block boundary}
}
}
\source{
\url{https://www.census.gov/geo/maps-data/data/tiger-line.html}
}
\usage{
baltimore_blocks
}
\description{
U.S. Census Blocks for Baltimore city, Maryland downloaded from the U.S.
Census Bureau API with the tigris package.
}
\keyword{datasets}

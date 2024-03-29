% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{combine_acs_geography_levels}
\alias{combine_acs_geography_levels}
\title{Combine objects or rda file paths for area, county, and msa into a single
data.frame}
\usage{
combine_acs_geography_levels(data, levels = NULL, .col = "name", ...)
}
\description{
data.frame must include "table_title", "column_title", "variable" as column
names. Filtered to values of levels in .col (defaults to "name")
}
\keyword{internal}

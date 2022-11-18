#' U.S. Census Blocks - 2020
#'
#' U.S. Census Blocks for Baltimore city, Maryland downloaded from the U.S.
#' Census Bureau API with the tigris package.
#'
#' @format A data frame with 13,598 rows and 9 variables:
#' \describe{
#'   \item{`tractce10`}{Tract FIPS}
#'   \item{`blockce10`}{Block FIPS}
#'   \item{`geoid10`}{Block GeoID}
#'   \item{`name10`}{Block name}
#'   \item{`aland10`}{Land area}
#'   \item{`awater10`}{Water area}
#'   \item{`intptlat10`}{Interior center point latitude}
#'   \item{`intptlon10`}{Interior center point longitude}
#'   \item{`geometry`}{Multipolygon with block boundary}
#' }
#' @source <https://www.census.gov/geo/maps-data/data/tiger-line.html>
"baltimore_blocks"

#' U.S. Census Block Groups - 2020
#'
#' U.S. Census Block Groups for Baltimore city, Maryland downloaded from the
#' U.S. Census Bureau API with the tigris package.
#'
#' @format A data frame with 618 rows and 9 variables:
#' \describe{
#'   \item{`tractce`}{census tract code}
#'   \item{`blkgrpce`}{block group number}
#'   \item{`geoid`}{Census block group identifier; a concatenation of the state FIPS code, county FIPS code, census tract code, and block group number}
#'   \item{`namelsad`}{translated legal/statistical area description and the block group number}
#'   \item{`aland`}{land area (square meters)}
#'   \item{`awater`}{water area (square meters)}
#'   \item{`intptlat`}{latitude of the internal point}
#'   \item{`intptlon`}{longitude of the internal point}
#'   \item{`geometry`}{Polygon with block group boundary}
#' }
#' @source <https://www.census.gov/geo/maps-data/data/tiger-line.html>
"baltimore_block_groups"

#' U.S. Census Tracts - 2020
#'
#' U.S. Census Tracts for Baltimore city, Maryland downloaded from the
#' U.S. Census Bureau API with the tigris package.
#'
#' @format A data frame with 199 rows and 9 variables:
#' \describe{
#'   \item{`tractce`}{census tract code}
#'   \item{`geoid`}{nation-based census tract identifier; a concatenation of state FIPS code, county FIPS code, and census tract number}
#'   \item{`name`}{Variable length geographic area name}
#'   \item{`namelsad`}{name and the translated legal/statistical area description code for census tract}
#'   \item{`aland`}{land area (square meters)}
#'   \item{`awater`}{water area (square meters)}
#'   \item{`intptlat`}{latitude of the internal point}
#'   \item{`intptlon`}{longitude of the internal point}
#'   \item{`geometry`}{Polygon with tract boundary}
#' }
#' @source <https://www.census.gov/geo/maps-data/data/tiger-line.html>
"baltimore_tracts"

#'  Baltimore PUMAS (Public Use Microdata Areas) - 2010
#'
#'  The U.S. Census Bureau explains that "Public Use Microdata Areas
#'  (PUMAs) are non-overlapping, statistical geographic areas that partition
#'  each state or equivalent entity into geographic areas containing no fewer
#'  than 100,000 people each... The Census Bureau defines PUMAs for the
#'  tabulation and dissemination of decennial census and American Community
#'  Survey (ACS) Public Use Microdata Sample (PUMS) data."
#'
#' @format A data frame with 5 rows and 8 variables:
#' \describe{
#'  \item{`pumace10`}{PUMA code}
#'  \item{`geoid10`}{GeoID}
#'  \item{`namelsad10`}{name and the translated legal/statistical area description code for census tract}
#'  \item{`aland10`}{land area (square meters)}
#'  \item{`awater10`}{water area (square meters)}
#'  \item{`intptlat10`}{latitude of the internal point}
#'  \item{`intptlon10`}{longitude of the internal point}
#'  \item{`geometry`}{Polygon with PUMA boundary}
#'  }
#' @source <https://www.census.gov/geo/maps-data/data/tiger-line.html>
"baltimore_pumas"

#' County boundaries for the Baltimore–Columbia–Towson MSA
#'
#' Counties boundaries in the Baltimore–Columbia–Towson Metropolitan Statistical Area (MSA)
#' include Baltimore City, Baltimore County, Carroll County, Anne Arundel County,
#' Howard County, Queen Anne's County, and Harford County.
#'
#' @format A data frame with 7 rows and 13 variables:
#' \describe{
#'   \item{`countyfp`}{County FIPS code}
#'   \item{`countyns`}{County GNIS code}
#'   \item{`geoid`}{Unique county FIPS code (concatenation of state and county FIPS codes)}
#'   \item{`name`}{County name}
#'   \item{`namelsad`}{Concatenated variable length geographic area name and legal/statistical area description (LSAD)}
#'   \item{`lsad`}{Legal/statistical area description (LSAD)}
#'   \item{`classfp`}{FIPS class code}
#'   \item{`funcstat`}{Functional status}
#'   \item{`aland`}{Land area (square meters)}
#'   \item{`awater`}{Water area (square meters)}
#'   \item{`intptlat`}{Latitude of the internal point}
#'   \item{`intptlon`}{Longitude of the internal point}
#'   \item{`geometry`}{Multipolygon with the county boundary}
#' }
#' @source <https://www.census.gov/geo/maps-data/data/tiger-line.html>
"baltimore_msa_counties"

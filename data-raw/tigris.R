options(tigris_use_cache = TRUE)

year <- 2020
xwalk_year <- 2020
state <- "MD"
state_fips <- "24"
county <- "Baltimore city"
county_fips <- "510"
msa_county_names <- c("Baltimore", "Anne Arundel", "Carroll", "Harford", "Howard", "Queen Anne's")
county_pumace10 <- c("00801", "00802", "00803", "00804", "00805")


# https://github.com/r-spatial/sf/issues/1341#issuecomment-1120284345
escape_crs <- function(x) {
  sf::st_crs(x)$wkt <- gsub(
    "°|º", "\\\u00b0",
    sf::st_crs(x)$wkt
  )

  x
}

prep_data <- function(x, ..., case = "snake", crs = 3857) {
  x <- janitor::clean_names(x, case)
  x <- janitor::remove_empty(x, which = "cols")
  x <- janitor::remove_constant(x, na.rm = TRUE)

  if (!inherits(x, "sf")) {
    return(x)
  }

  x <- sf::st_transform(x, crs)

  escape_crs(x)
}

# U.S. Census blocks ----
baltimore_blocks <-
  tigris::blocks(state = state, county = county, year = year) %>%
  prep_data()

usethis::use_data(baltimore_blocks, overwrite = TRUE)

# U.S. Census block groups ----
baltimore_block_groups <-
  tigris::block_groups(state = state, county = county, year = year) %>%
  prep_data()

usethis::use_data(baltimore_block_groups, overwrite = TRUE)

# U.S. Census tracts ----
baltimore_tracts <-
  tigris::tracts(state = state, county = county) %>%
  prep_data()

usethis::use_data(baltimore_tracts, overwrite = TRUE)

# PUMAs ----
baltimore_pumas <-
  tigris::pumas(state = state, year = year) %>%
  prep_data() %>%
  dplyr::filter(pumace10 %in% county_pumace10)

usethis::use_data(baltimore_pumas, overwrite = TRUE)

baltimore_msa_counties <-
  tigris::counties(state = state, year = year) %>%
  prep_data() %>%
  dplyr::filter(name %in% msa_county_names) %>%
  dplyr::select(-c(csafp, cbsafp, metdivfp))

usethis::use_data(baltimore_msa_counties, overwrite = TRUE)

#' Load metadata from Census Reporter
#'
#' See <https://github.com/censusreporter/census-table-metadata>
#'
#' @param survey Survey using options from [tidycensus::get_acs()]
#' @param year Year of sample to return.
#' @param metadata Type of metadata to return, "table" or "column"
#' @noRd
load_census_reporter_metadata <- function(survey = "acs5",
                                          year = 2020,
                                          metadata = "table",
                                          ...) {
  if (str_detect(survey, "^acs")) {
    survey_type <- "acs"
    sample <- str_extract(survey, "[0-9]$")
    folder <- glue::glue("{survey_type}{year}_{sample}yr")
  } else {
    cli::cli_abort("{.arg survey} must start with acs.")
  }

  filename <- glue::glue("census_{metadata}_metadata.csv")

  if (year == 2020 && survey == "acs5") {
    url <- system.file("inst/extdata/", filename, package = "baltimorecensus")
  } else {
    url <-
      paste0(
      "https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed/",
      folder, "/", filename
    )
  }

  readr::read_csv(
    url,
    ...
  )
}

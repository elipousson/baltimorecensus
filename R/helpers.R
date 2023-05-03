# @staticimports pkg:stringstatic
#   str_extract str_detect str_remove

#' Label acs data using table and column metadata from Census Reporter
#'
label_acs <-
  function(data,
           survey = "acs5",
           year = 2021,
           perc = TRUE) {
    table_metadata <-
      load_census_reporter_metadata(survey, year, metadata = "table")

    column_metadata <-
      load_census_reporter_metadata(survey, year, metadata = "column")

    data <- data %>%
      dplyr::mutate(
        table_id = stringr::str_extract(variable, ".+(?=_)"),
        column_id = stringr::str_remove(variable, "_")
      ) %>%
      dplyr::left_join(table_metadata) %>%
      dplyr::left_join(column_metadata) %>%
      dplyr::rename(
        column_id_denom = denominator_column_id
      ) %>%
      dplyr::mutate(
        race_category = stringr::str_extract(table_title, "(?<=\\().+(?=\\))")
      )

    if (perc && rlang::has_name(data, "geoid")) {
      calc_perc(data)
    } else {
      data
    }
  }

calc_perc <- function(x, denom_id = "geoid") {
  denom_x <- x %>%
    dplyr::filter(column_id %in% x$column_id_denom) %>%
    dplyr::select(
      {{ denom_id }},
      estimate_denom = estimate,
      moe_denom = moe,
      column_id_denom = column_id,
      column_title_denom = column_title
    )

  x <- x %>%
    dplyr::left_join(denom_x) %>%
    dplyr::mutate(
      perc_estimate = round(estimate / estimate_denom, digits = 2),
      moe_perc = tidycensus::moe_prop(estimate, estimate_denom, moe, moe_denom),
      .after = moe
    )

  x
}

add_tract_geometry <- function(x) {
  x %>%
    dplyr::left_join(mapbaltimore::baltimore_tracts, by = "geoid") %>%
    sf::st_as_sf()
}


simple_money_table <- function(x) {
  table_title <- unique(x$table_title)

  multi_column <- length(unique(x$column_title)) > 1

  if (multi_column) {
    x <- x %>%
      dplyr::select(name, column_title, estimate, moe)
  } else {
    x <- x %>%
      dplyr::select(name, estimate, moe)
  }

  x %>%
    dplyr::group_by(name) %>%
    gt::gt() %>%
    gt::tab_header(
      title = table_title
    )
}

pull_table <- function(x, meta = FALSE, table = NULL, drop_vars = NULL, column = NULL, vars = NULL, geography = NULL) {
  x <- x %>%
    dplyr::filter(table_id %in% table)

  if (meta) {
    return(x)
  }

  if (!is.null(drop_vars)) {
    x <- x %>%
      dplyr::filter(!(variable %in% drop_vars))
  }


  if (!is.null(vars)) {
    x <- x %>%
      dplyr::filter(variable %in% vars)
  }

  if (!is.null(column)) {
    x <- x %>%
      dplyr::filter(column_title %in% column)
  }

  if (!is.null(geography)) {
    x <- x %>%
      dplyr::filter(geography %in% geography)
  }

  x
}



#' Load metadata from Census Reporter
#'
#' See <https://github.com/censusreporter/census-table-metadata>
#'
#' @param survey Survey using options from [tidycensus::get_acs()]
#' @param year Year of sample to return.
#' @param metadata Type of metadata to return, "table" or "column"
#' @noRd
load_census_reporter_metadata <- function(survey = "acs5",
                                          year = 2021,
                                          metadata = "table",
                                          ...) {
  if (stringr::str_detect(survey, "^acs")) {
    survey_type <- "acs"
    sample <- stringr::str_extract(survey, "[0-9]$")
    folder <- glue::glue("{survey_type}{year}_{sample}yr")
  } else {
    cli::cli_abort("{.arg survey} must start with acs.")
  }

  filename <- glue::glue("census_{metadata}_metadata.csv")

  if (year >= 2020 && survey == "acs5") {
    url <- system.file("extdata", paste0(year, "_", filename), package = "baltimorecensus")
  } else {
    url <-
      paste0(
        "https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed/",
        folder, "/", filename
      )
  }
  return(url)

  readr::read_csv(
    url,
    ...
  )
}

#' Defunct: prior version of gt table helper
#'
#' @noRd
make_acs_tbl <- function(x) {
  x %>%
    select(name, column_title, estimate, moe, perc_estimate, moe_perc) %>%
    # mutate(
    #   perc_estimate,	moe_perc
    # )
    gt::gt(
      groupname_col = "name"
    ) %>%
    gt::fmt_number(
      columns = all_of(c("estimate", "moe")),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    gt::cols_merge_uncert(
      col_val = "estimate",
      col_uncert = "moe"
    ) %>%
    gt::cols_merge_uncert(
      col_val = "perc_estimate",
      col_uncert = "moe_perc"
    ) %>%
    gt::fmt_percent(contains("perc"), decimals = 0) %>%
    gt::cols_label(
      column_title = "",
      estimate = "Estimate (MOE)",
      perc_estimate = "% of Total"
    )
}


try_cols_merge_uncert <- function(gt_object,
                                  cols = NULL) {
  if (is_character(cols, 2)) {
    gt_object |>
      gt::cols_merge_uncert(
        col_val = cols[[1]],
        col_uncert = cols[[2]]
      )
  } else {
    gt_object
  }
}


#' Format estimate and MOE columns in a gt table
#'
#' @noRd
fmt_est_moe <- function(gt_object,
                        cols = c("estimate", "moe"),
                        est_col_label = "Est.",
                        spanner = NULL,
                        decimals = 0,
                        use_seps = TRUE) {
  gt_object <- gt_object |>
    gt::fmt_number(
      columns = any_of(cols),
      decimals = decimals,
      use_seps = use_seps
    ) |>
    gt::cols_label(
      .list = set_names(est_col_label, cols[[1]])
    )

  if (is.null(spanner)) {
    gt_object |>
      try_cols_merge_uncert(cols)
  } else {
    gt_object |>
      gt::tab_spanner(spanner, cols = cols)
  }
}

#' Format % estimate and % MOE columns in a gt table
#'
#' @noRd
fmt_perc_moe <- function(gt_object,
                         cols = c("perc_estimate", "moe_perc"),
                         perc_col_label = "% share",
                         spanner = NULL,
                         decimals = 0,
                         use_seps = TRUE) {
  gt_object <- gt_object |>
    gt::fmt_percent(
      any_of(cols),
      decimals = decimals
    ) |>
    gt::cols_label(
      .list = set_names(perc_col_label, cols[[1]])
    )

  if (is.null(spanner)) {
    gt_object |>
      try_cols_merge_uncert(cols)
  } else {
    gt_object |>
      gt::tab_spanner(spanner, cols = cols)
  }
}

#' Format Census estimate and percent estimate columns
#'
#' @noRd
gt_census_cols <- function(gt_object,
                           est_cols = c("estimate", "moe"),
                           est_col_label = "Est.",
                           perc_cols = c("perc_estimate", "moe_perc"),
                           perc_col_label = "% share",
                           est_spanner = NULL,
                           perc_spanner = NULL,
                           column_title = NULL,
                           source = "Source: 2017-2021 ACS 5-year Estimates",
                           tables = NULL,
                           decimals = 0) {
  stopifnot(
    rlang::has_name(gt_object[["_data"]], "column_title")
  )

  gt_object |>
    fmt_est_moe(est_cols, est_col_label, decimals = decimals, spanner = est_spanner) |>
    fmt_perc_moe(perc_cols, perc_col_label, decimals = decimals, spanner = perc_spanner) |>
    gt::cols_label(
      "column_title" = column_title %||% ""
    ) |>
    gt_census_source(source, tables)
}

#' Add a source note to a table
#'
#' @noRd
gt_census_source <- function(gt_object,
                             source = "Source: 2017-2021 ACS 5-year Estimates",
                             tables = NULL) {
  if (!is_null(tables)) {
    table_label <- "Table"
    if (length(tables) > 1) {
      table_label <- "Tables"
    }

    source <- glue::glue("{source}, {table_label} {knitr::combine_words(tables)}.")
  } else {
    source <- glue::glue("{source}.")
  }

  gt::tab_source_note(gt_object, source)
}

#' Helper for recoding based on a named list
#'
#' @noRd
fct_recode_with_list <- function(x, list = NULL, in_order = TRUE) {
  x <- forcats::fct_recode(x, !!!list)

  if (!in_order) {
    return(x)
  }

  forcats::fct_inorder(x)
}

#' Helper for checking name availability
#'
#' @noRd
has_name_ext <- function(x,
                         name = NULL,
                         starts_with = NULL,
                         ends_with = NULL,
                         contains = NULL) {
  nm <- names(x)
  has_nm <- rlang::has_name(x, name %||% nm)

  if (!is.null(starts_with)) {
    has_nm <- has_nm & stringr::str_detect(nm, paste0("^", starts_with, collapse = "|"))
  }

  if (!is.null(ends_with)) {
    has_nm <- has_nm & stringr::str_detect(nm, paste0(ends_with, "$", collapse = "|"))
  }

  if (!is.null(contains)) {
    has_nm <- has_nm & stringr::str_detect(nm, paste0(contains, collapse = "|"))
  }

  has_nm
}


#' Combine objects or rda file paths for area, county, and msa into a single
#' data.frame
#'
#' data.frame must include "table_title", "column_title", "variable" as column
#' names. Filtered to values of levels in .col (defaults to "name")
#'
#' @noRd
combine_acs_geography_levels <- function(data,
                                         levels = NULL,
                                         .col = "name",
                                         ...) {
  data <-
    dplyr::bind_rows(
      purrr::map(
        data,
        ~ readr::read_rds(here::here(.x))
      )
    )

  stopifnot(
    rlang::has_name(data, c(.col, c("table_title", "column_title", "variable")))
  )

  data <- dplyr::filter(
    data,
    .data[[.col]] %in% levels
  )

  data[[.col]] <- fct_recode_with_list(data[[.col]], levels)

  dplyr::relocate(
    data,
    tidyselect::all_of(c("table_title", "column_title")),
    .after = tidyselect::all_of("variable")
  )
}

#' @noRd
plot_theme <- function(...) {
  pilot::theme_pilot(...)
}

#' @noRd
plot_fill_scale <- function(palette = "tol.bright", ..., type = "discrete") {
  switch(type,
    "discrete" = cols4all::scale_fill_discrete_c4a_cat(palette = palette, ...)
  )
}

#' @noRd
labs_acs <- function(...,
                     caption = "Source: 2017-2021 ACS 5-year Estimates") {
  ggplot2::labs(
    ...,
    caption = caption
  )
}


#' @noRd
bind_race_category_col <- function(data, col = "table_title") {
  dplyr::mutate(
    data,
    race_category = dplyr::case_when(
      stringr::str_detect(.data[[col]], "White Alone") ~ "White",
      stringr::str_detect(.data[[col]], "Black or African American") ~ "Black",
      stringr::str_detect(.data[[col]], "Hispanic or Latino") ~ "Latino",
      .default = "Total"
    )
  )
}

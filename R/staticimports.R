# Generated by staticimports; do not edit by hand.
# ======================================================================
# Imported from pkg:isstatic
# ======================================================================

#' Does string contain the specified file type or any file extension?
#'
#' Check if string contains any filetype or the provided filetype. If string is
#' `NULL`, returns `FALSE`.
#'
#' @param string String to be tested with or without filetype. Defaults to
#'   `NULL`.
#' @param fileext File type to test against. Optional.
#' @param ignore.case If `FALSE`, the pattern matching is case sensitive. If
#'   `TRUE`, case is ignored.
#' @seealso [isstatic::is_fileext_path()]
#' @noRd
has_fileext <- function(string = NULL, fileext = NULL, ignore.case = FALSE) {
  if (is.null(string)) {
    return(FALSE)
  }

  if (is.null(fileext)) {
    fileext <- "[a-zA-Z0-9]+"
  }

  is_fileext_path(string, fileext, ignore.case)
}

#' Does this text end in the provided file extension?
#'
#' @param x A character vector to check for matches, or an object which can be
#'   coerced by [as.character()] to a character vector.
#' @param fileext A file extension to compare to x. Required. If a vector of
#'   multiple extensions are provided, returns `TRUE` for any match.
#' @inheritParams base::grepl
#' @seealso [isstatic::has_fileext()]
#' @noRd
is_fileext_path <- function(x, fileext, ignore.case = TRUE) {
  grepl(
    paste0("\\.", paste0(fileext, collapse = "|"), "$(?!\\.)"),
    x,
    ignore.case = ignore.case, perl = TRUE
  )
}
# Generated by staticimports; do not edit by hand.
# ======================================================================
# Imported from pkg:stringstatic
# ======================================================================

#' Detect the presence or absence of a pattern in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_detect()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param negate If `TRUE`, return non-matching elements.
#'
#' @return A logical vector.
#' @noRd
str_detect <- function(string, pattern, negate = FALSE) {
	if (length(string) == 0 || length(pattern) == 0) return(logical(0))

	is_fixed <- inherits(pattern, "stringr_fixed")

	indices <- Vectorize(grep, c("pattern", "x"), USE.NAMES = FALSE)(
		pattern,
		x = string,
		perl = !is_fixed,
		fixed = is_fixed,
		invert = negate
	)

	result <- as.logical(lengths(indices))
	result[is.na(string)] <- NA
	result
}

#' Extract matching patterns from a string
#'
#' Dependency-free drop-in alternative for `stringr::str_extract()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character matrix.
#'   The first column is the complete match,
#'   followed by one column for each capture group.
#' @noRd
str_extract <- function(string, pattern) {
	if (length(string) == 0 || length(pattern) == 0) return(character(0))

	is_fixed <- inherits(pattern, "stringr_fixed")

	result <- Map(
		function(string, pattern) {
			if (is.na(string) || is.na(pattern)) return(NA_character_)

			regmatches(
				x = string,
				m = regexpr(
					pattern = pattern, text = string, perl = !is_fixed, fixed = is_fixed
				)
			)
		},
		string, pattern, USE.NAMES = FALSE
	)

	result[lengths(result) == 0] <- NA_character_
	unlist(result)
}

#' Remove matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_remove()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @return A character vector.
#' @noRd
str_remove <- function(string, pattern) {
	if (length(string) == 0 || length(pattern) == 0) return(character(0))
	is_fixed <- inherits(pattern, "stringr_fixed")
	Vectorize(sub, c("pattern", "x"), USE.NAMES = FALSE)(
		pattern, replacement = "", x = string, perl = !is_fixed, fixed = is_fixed
	)
}

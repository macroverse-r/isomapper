#' Convert ISO code to country name
#' 
#' Internal helper function that converts ISO3 codes to their corresponding country names
#' using a hierarchical lookup across multiple tables.
#'
#' @param iso_code Character vector of ISO3 country codes
#' @param verbose Logical, whether to print information about matches found in special or historical
#'   code lists (default: TRUE)
#'   
#' @return Character vector of country names. Returns NA for ISO codes not found in any lookup table.
#'   When verbose is TRUE, prints informative messages for:
#'   \itemize{
#'     \item Matches found in special/regional codes list
#'     \item Matches found in historical codes list
#'     \item ISO codes not found in any list
#'   }
#'
#' @details The function performs lookups in the following order:
#'   1. Current ISO codes (list_iso2country)
#'   2. Special and regional codes (list_iso2country_others)
#'   3. Historical codes (list_iso2country_old)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Current ISO code
#' .get_ctry_name("FRA")  # Returns "France"
#' 
#' # Special regional code
#' .get_ctry_name("EMU")  # Returns "Euro Area" with note if verbose=TRUE
#' 
#' # Historical code
#' .get_ctry_name("CSK")  # Returns "Czechoslovakia" with note if verbose=TRUE
#' 
#' # Multiple codes
#' .get_ctry_name(c("USA", "EMU", "CSK"))  
#' # Returns c("United States", "Euro Area", "Czechoslovakia")
#' # with appropriate notes for EMU and CSK if verbose=TRUE
#' }
.get_ctry_name <- function(iso_code, verbose = TRUE) {
  # Function to look up the country name for a single ISO code
  lookup_name <- function(code) {
    # First check in main current ISO list
    name <- list_iso2country[[code]]
    if (!is.null(name)) {
      return(name)
    }
    
    # If not found, check in others list
    name <- list_iso2country_others[[code]]
    if (!is.null(name)) {
        if (verbose) {
            mvcommon::mv_debug(sprintf("ISO code '%s' (%s) found in special/regional pseudo-ISO codes list", code, name), 
                        verbose, FALSE, "info", "Special")
        }
        return(name)
    }
    
    # If still not found, check in historical list
    name <- list_iso2country_old[[code]]
    if (!is.null(name)) {
        if (verbose) {
        mvcommon::mv_debug(sprintf("ISO code '%s' (%s) found in historical ISO codes list", code, name), 
                    verbose, FALSE, "info", "Archive")
        }
        return(name)
    }
    
    if (verbose) {
        # If no match found in any list
        mvcommon::mv_debug(sprintf("ISO not in any lists: %s", code),
                    verbose, FALSE, "warning")
    }
    return(NA)
  }
  
  # If input is a vector, apply the lookup function to each element
  if (length(iso_code) > 1) {
    return(sapply(iso_code, lookup_name))
  } else {
    return(lookup_name(iso_code))
  }
}


#' Get all categories that contain specific countries
#'
#' @param country_codes Character vector of ISO3 country codes
#'
#' @return Character vector of category names
#'
#' @keywords internal
.get_ctry_categories <- function(country_codes) {
  if (!is.character(country_codes) || any(nchar(country_codes) != 3)) {
    stop("country_codes must be ISO3 codes")
  }
  
  result <- character(0)
  for (cat in list_categories) {
    category_countries <- im_get_category(cat, verbose = FALSE)
    if (all(country_codes %in% category_countries)) {
      result <- c(result, cat)
    }
  }
  return(result)
}


#' Get overlapping countries between two categories
#' 
#' @param category1 Character, first category name
#' @param category2 Character, second category name
#' @return Character vector of ISO3 codes present in both categories
#' @examples
#' eu_brics_overlap <- get_category_overlap("EU", "BRICS")
#' 
#' @keywords internal
#' @noRd
.get_category_overlap <- function(category1, category2) {
  countries1 <- im_get_category(category1, verbose = FALSE)
  countries2 <- im_get_category(category2, verbose = FALSE)
  return(intersect(countries1, countries2))
}


#' Get countries that are in category1 but not in category2
#' 
#' @param category1 Character, first category name
#' @param category2 Character, second category name
#' @return Character vector of ISO3 codes present in category1 but not in category2
#' @examples
#' \dontrun{
#' # Find advanced economies (CTR) that are not in the European Union (EU)
#' ctr_not_eu <- .get_category_difference("CTR", "EU")
#' # Returns: c("USA", "GBR", "JPN", "AUS", "CAN", "CHE", "ISL", "NOR", "NZL")
#' }
#' @keywords internal
#' @noRd
.get_category_difference <- function(category1, category2) {
  countries1 <- im_get_category(category1, verbose = FALSE)
  countries2 <- im_get_category(category2, verbose = FALSE)
  return(setdiff(countries1, countries2))
}
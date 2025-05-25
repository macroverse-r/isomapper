#' Get Countries Belonging to Specified Categories
#' 
#' @description Returns ISO3 codes for countries belonging to specified categories. 
#' Supports complex queries including category exclusions.
#'
#' @param input Character vector of category codes or ISO3 country codes. Can include
#'   exclusion patterns (e.g., "EUROPE-DEU" for European countries excluding Germany).
#' @param verbose Logical, whether to print reminder messages about specific countries 
#'   (default: TRUE)
#'
#' @return Character vector of ISO3 country codes belonging to the specified categories
#'
#' @details Categories include:
#'   \itemize{
#'     \item Economic: CTR_LDR, CTR_FOL, SMP_WLD, SMP_RLD, SMP_FOL, PERI
#'     \item Regional: AFRICA, AMERICAS, ASIA, EUROPE, OCEANIA
#'     \item Subregional: NORTH_AFRICA, WEST_ASIA, etc.
#'     \item Groups: BRICS, EU, ASEAN, etc.
#'     \item Resources: NRS_REN, HYD_EXP, HYD_IMP
#'   }
#'
#' @examples
#' # Get all center countries
#' center_countries <- im_get_category("CTR")
#' 
#' # Get European countries excluding Germany
#' europe_except_germany <- im_get_category("EUROPE-DEU")
#' 
#' # Get multiple categories
#' brics_and_eu <- im_get_category(c("BRICS", "EU"))
#'
#' @family country classification functions
#' @seealso 
#'   \code{\link{im_from_iso}} for getting specific information about countries,
#'   \code{\link{im_ctry2iso}} for converting country names to ISO codes
#'
#' @export
im_get_category <- function(input, verbose = TRUE) {
  # Input validation
  if (!is.character(input)) {
    stop("Input must be a character vector")
  }
  
  # Helper function to expand a single group
  expand_group <- function(group_name) {
    # Remove any whitespace
    group_name <- trimws(group_name)
    
    # Check if it's an exclusion pattern (contains "-")
    if (grepl("-", group_name)) {
      # Split the string into group and exclusions
      parts <- strsplit(gsub("\\s+", "", group_name), "-")[[1]]
      group <- parts[1]
      exclusions <- parts[-1]
      
      # Get the base group's ISO codes
      base_codes <- list_category2iso[[group]]
      if (!is.null(base_codes)) {
        return(setdiff(base_codes, exclusions))
      } else {
        return(character(0))
      }
    }
    
    # If not an exclusion pattern, check if it's a group name
    codes <- list_category2iso[[group_name]]
    if (!is.null(codes)) {
      return(codes)
    }
    
    # If not a group, then check if it's a direct ISO code
    if (nchar(group_name) == 3) {
      return(group_name)
    }
    
    # Return empty vector if group not found
    return(character(0))
  }
  
  # Print reminders for specific countries if verbose is TRUE
  if (verbose && any(input %in% c("CTR", "CTR_FOL", "SMP", "SMP_FOL"))) {
    cat("Note: IRL, HKG and SGP are in CTR and ESP, ISR, TWN, KOR and CHN are in SMP\n")
  }
  
  # Process each input element and combine results
  result <- character(0)
  for (item in input) {
    result <- c(result, expand_group(item))
  }
  
  # Remove duplicates and return
  return(unique(result))
}
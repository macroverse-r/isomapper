
#' Convert Country Name to ISO3 Code
#' 
#' @description Converts country names to their corresponding ISO3 codes. The function
#' uses a hierarchical matching strategy: first trying exact match, then prefix and
#' partial matches if the input is long enough, while avoiding matches on common words.
#'
#' @param country_name Character vector containing the country names to convert.
#'   The function accepts various formats and spellings, including:
#'   \itemize{
#'     \item Official names (e.g., "United States of America")
#'     \item Common names (e.g., "United States", "USA")
#'     \item Historical names (e.g., "Burma" for Myanmar)
#'     \item Names with special characters (e.g., "Côte d'Ivoire")
#'     \item Names with abbreviations (e.g., "Rep." for Republic)
#'   }
#' @param verbose Logical, whether to print information about matching method used 
#'   when exact match fails (default: TRUE)
#' @param min_letter Integer, minimum number of letters required for prefix and 
#'   partial matching attempts (default: 5). Only used if exact match fails.
#'
#' @return Character vector of the same length as country_name containing ISO3 codes.
#'   Returns NA for entries where no match is found, with a warning message if verbose=TRUE.
#'
#' @details The function performs matching in the following order:
#'   1. Normalizes input name (converts to lowercase, removes special characters,
#'      standardizes spacing)
#'   2. Attempts exact match with normalized names
#'   3. If exact match fails and input length >= min_letter:
#'      - Tries prefix match (input matches start of country name)
#'      - Tries partial match (input is contained within country name)
#'      - Skips matching on common words like "United", "Republic", etc.
#'      - If verbose=TRUE, informs user about successful prefix/partial match
#'
#' @examples
#' # Single country
#' im_ctry2iso("France")  # Returns "FRA"
#' 
#' # Multiple countries
#' im_ctry2iso(c("India", "China", "Italy"))  # Returns c("IND", "CHN", "ITA")
#' 
#' # Mixed cases with some partial matches, short abbreviations, and errors
#' im_ctry2iso(c("franc", "Deutsche", "Kingdom", "us", "UAE", "Jamai")) 
#' 
#' @family country code conversion functions
#' @seealso 
#'   \code{\link{im_from_iso}} for converting ISO codes back to country names,
#'   \code{\link{im_get_category}} for getting countries in specific categories
#'
#' @export
im_ctry2iso <- function(country_name, verbose = TRUE, min_letter = 5) {
  
  # Define common words to exclude from partial/prefix matching
  stop_words <- c(
    "united", "republic", "democratic", "state", "states", "kingdom",
    "islamic", "federal", "federation", "new", "northern", "southern",
    "eastern", "western", "central", "people", "saint", "san", "union",
    "great", "grand", "independent", "congo", "korea", "germany"
  )
  
  # Helper function to normalize country names
  normalize_name <- function(name) {
    # Convert to lowercase
    name <- tolower(name)

    # Remove other special characters and standardize spacing
    name <- gsub("[._,\\-\\(\\)''']", " ", name) # Remove some special chars
    name <- gsub("\\s+", " ", name)           # Standardize spaces
    name <- trimws(name)                      # Remove leading/trailing whitespace

    # Handle special characters and diacritics
    name <- gsub("[\u00e8\u00e9\u00ea\u00eb]", "e", name)  # e variations
    name <- gsub("[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5]", "a", name)  # a variations
    name <- gsub("[\u00ec\u00ed\u00ee\u00ef]", "i", name)  # i variations
    name <- gsub("[\u00f2\u00f3\u00f4\u00f5\u00f6]", "o", name)  # o variations
    name <- gsub("[\u00f9\u00fa\u00fb\u00fc]", "u", name)  # u variations
    name <- gsub("[\u00fd\u00ff]", "y", name)  # y variations
    name <- gsub("\u00e6", "ae", name)  # ae ligature
    name <- gsub("\u0153", "oe", name)  # oe ligature
    name <- gsub("\u00f1", "n", name)   # n with tilde
    name <- gsub("\u00df", "ss", name)  # sharp s
    name <- gsub("\u00e7", "c", name)   # c cedilla

    # Replace common abbreviations
    name <- gsub("\\bdem\\.?\\b", "democratic", name)
    name <- gsub("\\brep\\.?\\b", "republic", name)
    name <- gsub("\\bst\\.?\\b", "saint", name)
    name <- gsub("\\bfed\\.?\\b", "federal", name)
    name <- gsub("\\bgovt\\.?\\b", "government", name)

    # Return normalized name
    return(name)
  }
  
  # Helper function to check if string contains only stop words
  is_only_stop_words <- function(str) {
    words <- strsplit(str, "\\s+")[[1]]
    all(words %in% stop_words)
  }
  
  # Main matching function for a single country name
  find_match <- function(one_country) {
    # Normalize the input country name
    normalized_input <- normalize_name(one_country)

    if (normalized_input == "korea") {
        mvcommon::mv_debug(sprintf("ISO code for country name '%s' (%s) was associated with South Korea (KOR)", one_country, normalize_name(one_country)), verbose, FALSE, "warning")
        return("KOR")

    }
    
    # Step 1: Try exact match first
    for (name in names(list_ctry2iso)) {
      if (normalize_name(name) == normalized_input) {
        return(list_ctry2iso[[name]])
      }
    }
    
    # Only proceed with prefix/partial matching if input is long enough
    # and input is not just stop words
    if (nchar(normalized_input) >= min_letter && !is_only_stop_words(normalized_input)) {
      # Step 2: Try prefix match
      for (name in names(list_ctry2iso)) {
        normalized_name <- normalize_name(name)
        if (grepl(paste0("^", normalized_input), normalized_name)) {
            mvcommon::mv_debug(sprintf("Found prefix match: input '%s' matched with country '%s'", one_country, name),
                    verbose, FALSE, "info", "PREFIX")
            return(list_ctry2iso[[name]])
        }
      }
      
      # Step 3: Try partial match
      for (name in names(list_ctry2iso)) {
        normalized_name <- normalize_name(name)
        if (grepl(normalized_input, normalized_name)) {
            mvcommon::mv_debug(sprintf("Found prefix match: input '%s' matched with country '%s'", one_country, name),
                    verbose, FALSE, "info", "PARTIAL")
            return(list_ctry2iso[[name]])
        }
      }
    }
    
    # If no match is found, return NA with warning
    mvcommon::mv_debug(sprintf("ISO code for country name '%s' (%s) not found. NA value returned.", one_country, normalize_name(one_country)),
            verbose, FALSE, "warning")
    return(NA_character_)
  }
  
  # Apply the matching function to each country name in the input vector
  vapply(country_name, find_match, character(1))
}
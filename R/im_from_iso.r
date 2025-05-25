#' Convert ISO Code to Country Information
#' 
#' @description Returns specified information about countries based on their ISO3 codes.
#' The function accepts a vector of ISO codes and returns corresponding information based
#' on the specified option.
#'
#' @param iso Character vector of ISO3 country codes
#' @param opt Character string specifying the type of information to return. Options:
#'   \itemize{
#'     \item "Name": Country names
#'     \item "ISO2": ISO2 country codes
#'     \item "Region": Major geographical regions (AFRICA, AMERICAS, ASIA, EUROPE, OCEANIA)
#'     \item "Subregion": Detailed geographical regions (e.g., NORTH_AFRICA, WEST_ASIA)
#'     \item "Center-Periphery": Economic category (CTR_LDR, CTR_FOL, SMP_LDR, SMP_FOL, PERI)
#'     \item "Oil": Hydrocarbon status (HYD_EXP, HYD_IMP, or NA)
#'     \item "NaturalRent": Natural resource status (NRS_REN or NA)
#'     \item "Category": All categories the country belongs to
#'   }
#' @param verbose Logical, whether to print informative messages about special cases 
#'   (default: TRUE)
#' 
#' @return A character vector of the same length as the input iso vector containing the requested information.
#'         For "Category" option, returns a character vector of categories for each ISO code.
#'         For "ISO2" option, returns ISO2 codes (e.g., "US" for "USA").
#'         Returns NA for countries not found in the specified category or for invalid ISO codes.
#' 
#' @examples
#' # Get country names
#' im_from_iso(c("USA", "FRA", "DEU"), "Name")
#' 
#' # Get ISO2 codes
#' im_from_iso(c("USA", "GBR", "FRA"), "ISO2")  # Returns c("US", "GB", "FR")
#' 
#' # Get regions
#' im_from_iso(c("CHN", "BRA"), "Region")
#' 
#' # Get oil status
#' im_from_iso(c("SAU", "USA", "RUS"), "Oil")  # Returns c("HYD_EXP", NA, "HYD_EXP")
#' 
#' # Get economic categories
#' im_from_iso(c("USA", "CHN", "ETH"), "Center-Periphery")
#'
#' @family country code conversion functions
#' @family country classification functions
#' @seealso 
#'   \code{\link{im_ctry2iso}} for converting country names to ISO codes,
#'   \code{\link{im_get_category}} for getting all countries in specific categories
#'
#' @export
im_from_iso <- function(iso, 
                       opt = "Name",  # Alternative options: "ISO2", "Region", "Subregion", "Center-Periphery", "Oil", "NaturalRent", "Category"
                       verbose = TRUE
                        ) {
    # Input validation
    if (!is.character(iso)) {
        stop("iso must be a character vector")
    }
    
    # Convert input ISO codes to uppercase for case-insensitive matching
    iso <- toupper(iso)
    iso <- trimws(iso)

    # Helper function to check if a country is in a category
    in_check_category <- function(iso_code, category) {
        return(iso_code %in% list_category2iso[[category]])
    }
    
    # Helper function to get region/subregion
    in_get_region <- function(iso_code, detailed = FALSE) {
        # Define region categories
        regions <- c("AFRICA", "AMERICAS", "ASIA", "EUROPE", "OCEANIA")
        subregions <- c("NORTH_AFRICA", "EAST_AFRICA", "MIDDLE_AFRICA", "WEST_AFRICA", "SOUTH_AFRICA",
                       "NORTH_AMERICA", "CARIBBEAN", "CENTRAL_AMERICA", "SOUTH_AMERICA",
                       "EAST_ASIA", "SOUTHEAST_ASIA", "SOUTH_ASIA", "WEST_ASIA", "CENTRAL_ASIA",
                       "WESTERN_EUROPE", "EASTERN_EUROPE",
                       "AUSTRALIA_NZ", "OTHER_OCEANIA")
        
        categories_to_check <- if(detailed) subregions else regions
        
        for (cat in categories_to_check) {
            if (in_check_category(iso_code, cat)) {
                return(cat)
            }
        }
        return(NA_character_)
    }
    
    # Helper function to get center-periphery classification
    in_get_center_periphery <- function(iso_code) {
        if (in_check_category(iso_code, "CTR_LDR")) return("CTR_LDR")
        if (in_check_category(iso_code, "CTR_FOL")) return("CTR_FOL")
        if (in_check_category(iso_code, "SMP_WLD") || in_check_category(iso_code, "SMP_RLD")) return("SMP_LDR")
        if (in_check_category(iso_code, "SMP_FOL")) return("SMP_FOL")
        if (in_check_category(iso_code, "PERI")) return("PERI")
        return(NA_character_)
    }
    
    # Convert opt to make the function cas insensitive
    opt <- tolower(opt)

    # Process based on option
    if (opt == "name") {
        out <- .get_ctry_name(iso, verbose = verbose)
    } else if (opt == "category") {
        out <- .get_ctry_categories(iso)
    } else if (opt == "region") {
        out <- sapply(iso, in_get_region, detailed = FALSE, USE.NAMES = FALSE)
    } else if (opt == "subregion") {
        out <- sapply(iso, in_get_region, detailed = TRUE, USE.NAMES = FALSE)
    } else if (opt == "center-periphery") {
        out <- sapply(iso, in_get_center_periphery, USE.NAMES = FALSE)
    } else if (opt == "oil") {
        out <- sapply(iso, function(x) {
            if (in_check_category(x, "HYD_EXP")) return("HYD_EXP")
            if (in_check_category(x, "HYD_IMP")) return("HYD_IMP")
            return(NA_character_)
        }, USE.NAMES = FALSE)
    } else if (opt == "naturalrent") {
        out <- sapply(iso, function(x) {
            if (in_check_category(x, "NRS_REN")) return("NRS_REN")
            return(NA_character_)
        }, USE.NAMES = FALSE)
    } else if (opt == "iso2") {
        # Get ISO2 codes by looking up the reverse mapping in list_iso2_to_iso3
        out <- sapply(iso, function(x) {
            # Find the ISO2 code that maps to this ISO3 code
            iso2 <- names(list_iso2_to_iso3)[list_iso2_to_iso3 == x]
            if (length(iso2) == 0) return(NA_character_)
            return(iso2[1])  # Return the first match if multiple exist
        }, USE.NAMES = FALSE)
    } else {
        stop("Invalid opt value. Must be one of: 'Name', 'ISO2', 'Region', 'Subregion', 'Center-Periphery', 'Oil', 'NaturalRent', 'Category'")
    }
    
    if (opt != "Category") {
        names(out) <- iso
    }

    return(out)
}
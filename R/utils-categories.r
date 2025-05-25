#' Get Display Labels for Categories
#' 
#' @description
#' Internal function that provides display names for category codes.
#' Handles both standard categories and custom group labels.
#' 
#' Used by: in_create_color_mapping, in_create_scatter_colors
#' Uses: No direct function calls
#' 
#' Label types:
#' - Region names
#' - Subregion names
#' - Economic classifications
#' - Custom group names
#' - Default "Other" category
#'
#' @keywords internal
.get_category_labels <- function(color = NULL) {       # list/NULL: custom color grouping list
    
    # If no color argument or not a list, return category labels
    if (is.null(color) || !is.list(color)) {
        return(category_labels)
    }
    
    # Create group names (Group A, Group B, etc.)
    group_names <- paste0("Group ", LETTERS[1:length(color)])
    
    # Create list labels with descriptive names from the color list
    list_labels <- names(color)
    names(list_labels) <- group_names
    
    # Add list labels to base labels
    category_labels <- c(category_labels, list_labels)
    
    return(category_labels)
}
# Internal code in im_get_category, im_from_iso, im_ctry2iso, utils-country
# and utils-categories references the components of ISO_DATA as bare globals
# (list_category2iso, list_ctry2iso, list_iso2country, list_iso2_to_iso3,
# list_categories, category_labels, ...). Unpack the list at load time so
# those bare references resolve at runtime.
#
# Note: `utils::data("ISO_DATA", ...)` is not redundant with `LazyData: true`.
# During `.onLoad`, the lazy-loaded promise has not yet been forced, so the
# bare symbol `ISO_DATA` is not available in the namespace until `data()`
# writes it there explicitly.
#
# See TODO.md for the long-term migration to per-list .rda files.
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  utils::data("ISO_DATA", package = pkgname, envir = ns)
  collisions <- intersect(names(ISO_DATA), ls(ns))
  if (length(collisions) > 0) {
    stop("ISO_DATA components collide with isomapper namespace: ",
         paste(collisions, collapse = ", "),
         call. = FALSE)
  }
  list2env(ISO_DATA, envir = ns)
}

# Silence `R CMD check`'s "no visible binding for global variable" NOTEs:
# the names below are injected into the namespace by `.onLoad` above, so the
# static analyzer cannot see where they come from.
utils::globalVariables(c(
  "ISO_DATA",
  "list_ctry2iso",
  "list_iso2country",
  "list_iso2country_others",
  "list_iso2country_old",
  "list_iso_old2new",
  "list_iso2_to_iso3",
  "list_category2iso",
  "list_categories",
  "category_labels"
))

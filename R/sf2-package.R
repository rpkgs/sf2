#' @importFrom magrittr `%>%` `%<>%` set_names set_colnames set_rownames subtract
#' @importFrom data.table as.data.table fread
#' @importFrom methods as
#' @importFrom graphics grid
#' @importFrom stats median setNames
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
      "."
    ))
  }
  # options("rgdal_show_exportToProj4_warnings" = "none")
}

#' Aggregate Spatial Polygons
#'
#' A replacement for \code{rgeos::unaryUnion}.
#' @author Roger Bivand (thanks)
#' @note Source: \url{https://github.com/r-spatial/sf/issues/2563}
#' @param sp an \code{\link[sp]{SpatialPolygonsDataFrame}} object
#' @param id a grouping vector, as required by \code{\link[sf]{aggregate}}
#' @export
#' @import sf sp
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom stats aggregate
sfAggregate <- function(sp, id) {

  # sanity check: `sp` is a SpatialPolygons* object
  stopifnot(inherits(sp, "SpatialPolygons"))

  dupl <- sum(duplicated(sp[[ id ]]))

  if (dupl > 0) {

    message("Merging ", dupl, " SpatialPolygons with duplicate IDs")

    sp <- sf::st_as_sf(sp[, id])
    sp <- aggregate(sp, list(sp[[ id ]]), utils::head, n = 1)

    # sanity check: `sp` contains no duplicated `id` values
    stopifnot(!duplicated(sp[[ id ]]))

    sp <- sf::as_Spatial(dplyr::select(sp, -.data$Group.1))

  }

  # make sure SpatialPolygonsDataFrame `id` values are okay
  return(sp::spChFIDs(sp, as.character(sp@data[[ id ]])))

}

# kthxbye

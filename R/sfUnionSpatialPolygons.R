# Full copy of `maptools::unionSpatialPolygons` for reference
# Please check `maptools::unionSpatialPolygons` for the full documentation
# @author Roger Bivand
#
# unionSpatialPolygons <- function(SpP, IDs, threshold=NULL, avoidGEOS=FALSE, avoidUnaryUnion=FALSE) {
#   if (!is(SpP, "SpatialPolygons")) stop("not a SpatialPolygons object")
#   if (storage.mode(IDs) != "character") IDs <- as.character(IDs)
#   if (missing(IDs)) stop("IDs required")
#   if (length(slot(SpP, "polygons")) != length(IDs))
#     stop("input lengths differ")
#   rgeosI <- rgeosStatus()
#   if (rgeosI && !avoidGEOS) {
#     # require(rgeos)
#     if (!requireNamespace("rgeos", quietly = TRUE))
#       stop("package rgeos required for unionSpatialPolygons")
#     if (avoidUnaryUnion || rgeos::version_GEOS0() < "3.3.0")
#       res <- rgeos::gUnionCascaded(spgeom=SpP, id=IDs)
#     else
#       res <- rgeos::gUnaryUnion(spgeom=SpP, id=IDs)
#   }
#   res
# }

# Minimal copy of `maptools::unionSpatialPolygons` as called in `spReapportion`
# Please check `maptools::unionSpatialPolygons` for the full function
# @author Roger Bivand
#
# maptoolsUnionSpatialPolygons <- function(SpP, IDs) {
#   if (!is(SpP, "SpatialPolygons")) stop("not a SpatialPolygons object")
#   if (storage.mode(IDs) != "character") IDs <- as.character(IDs)
#   if (missing(IDs)) stop("IDs required")
#   if (length(slot(SpP, "polygons")) != length(IDs))
#     stop("input lengths differ")
#   res <- rgeos::gUnaryUnion(spgeom=SpP, id=IDs)
#   res
# }

#' Aggregate Polygons in a SpatialPolygons object (using sf)
#'
#' An attempt at merging polygons without \code{rgeos}, using \code{sf}, and
#' without losing too much speed. Very experimental, and very slow despite my
#' best efforts: see \url{https://github.com/r-spatial/sf/issues/2563} for
#' details on what I tried.
#' @author François Briatte
#' @param sp a `SpatialPolygonsDataFrame` object
#' @param id a string identifying the polygons
#' @export
#' @import sf
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#'
sfUnionSpatialPolygons <- function(sp, id) {

  # note to self, this is method [3] in my draft code
  # it is MUCH slower than the {rgeos} method used in {spReapportion}...
  # see https://github.com/r-spatial/sf/issues/2563

  zz <- sf::st_as_sf(sp)

  # slice 1, duplicate-ID polygons
  z1 <- dplyr::group_by(zz, .data[[id]])
  z1 <- dplyr::filter(z1, dplyr::n() > 1)
  # very much slower than `rgeos` if there are many polygons...
  z1 <- dplyr::summarise(z1, geometry = sf::st_union(geometry))

  # slice 2, single-ID polygons
  # line below is slightly faster than having `group_by` separately
  z2 <- dplyr::filter(zz, dplyr::n() == 1, .by = !!id)
  # drop non-grouping attributes to allow for `rbind` to work
  z2 <- dplyr::select(z2, !!id, geometry)

  return(sf::as_Spatial(rbind(z1, z2))) # ?sf::rbind.sf

}

# very very draft

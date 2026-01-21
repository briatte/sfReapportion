#' Reapportion data from one geography to another
#'
#' This function allows to reapportion data from one geography to another, for
#' example in the context of working with different administrative units.
#' @author Joël Gombin (initial \code{sp} version),
#' François Briatte (\code{sf} port)
#' @note Inspiration from \url{https://stackoverflow.com/a/17703903} and
#' \url{https://rstudio-pubs-static.s3.amazonaws.com/6577_3b66f8d8f4984fb2807e91224defa854.html}. All mistakes are mine, obviously.
#' @param old_geom a `SpatialPolygonsDataFrame` or `sf` object
#' representing the initial geometry.
#' @param new_geom a `SpatialPolygonsDataFrame` or `sf` object
#' representing the geometry you want to reapportion data to.
#' @param data a `data.frame` containing the data to reapportion, and an ID
#' allowing to match it to the `old_geom`.
#' @param old_ID a string, the name of the ID variable in the `old_geom`.
#' @param new_ID a string, the name of the ID variable in the `new_geom`.
#' @param data_ID a string, the name of the ID variable in the `data`.
#' @param variables a character vector, representing the names of the variables
#' in the `data` set to reapportion. By default, all data variables except for
#' the ID.
#' @param mode either `"count"` or `"proportion"`. `"count"` is for absolute
#' values, `"proportion"` is for, well, proportions (expressed between 0 and 1).
#' If `"proportion"`, you need to provide a `weights` variable.
#' @param weights In case the variables are proportions, the name of the
#' variable containing weights (i.e. the total number of observations per unit
#' in the `old_geom`).
#' @param weight_matrix \strong{(optional, untested)} a `SpatialPointsDataFrame`
#' indicating where are the observations (inhabitants, voters, etc.).
#' @param weight_matrix_var \strong{(optional, untested)} the name of the
#' variable in \code{weight_matrix} containing the weights.
#' @export
#' @import sp sf
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom dplyr summarise_at
#' @importFrom purrr map_int
#'
sfReapportion <- function(old_geom, new_geom, data, old_ID, new_ID, data_ID,
                          variables = names(data)[-which(names(data) %in% data_ID)],
                          mode = "count", weights = NULL,
                          weight_matrix = NULL, weight_matrix_var = NULL) {


  if (!mode %in% c("count", "proportion")) {
    stop('`mode` should be "count" or "proportion"')
  }

  old_geom_name <- deparse(substitute(old_geom))
  new_geom_name <- deparse(substitute(new_geom))
  data_name <- deparse(substitute(data))

  # convert sf objects to Spatial format ------------------------------------

  ###
  ### untested, but should work, right?
  ###
  if (inherits(old_geom, "sf")) {
    # if (is.na(sf::st_crs(old_geom)))
    #   warning("Missing CRS found in ", old_geom_name,
    #           " might cause errors\nRepair first by assigning one with ",
    #           "sf::st_set_crs(", old_geom_name, ", ...)")
    # if (!sf::st_is_longlat(old_geom))
    #   warning("Non-planar CRS found in ", old_geom_name,
    #           " might cause errors\nRepair first by reprojecting it with ",
    #           "sf::st_transform(", old_geom_name, ", ...)")
    if (any(!sf::st_is_valid(old_geom)))
      warning("Invalid geometries found in ", old_geom_name,
              " might cause errors\nRepair first with ",
              "sf::st_make_valid(", old_geom_name, ")")
    old_geom <- sf::as_Spatial(old_geom)
  }
  if (inherits(new_geom, "sf")) {
    # if (is.na(sf::st_crs(new_geom)))
    #   warning("Missing CRS found in ", new_geom_name,
    #           " might cause errors\nRepair first by assigning one with ",
    #           "sf::st_set_crs(", new_geom_name, ", ...)")
    # if (!sf::st_is_longlat(new_geom))
    #   warning("Non-planar CRS found in ", new_geom_name,
    #           " might cause errors\nRepair first by reprojecting it with ",
    #           "sf::st_transform(", new_geom_name, ", ...)")
    if (any(!sf::st_is_valid(new_geom)))
      warning("Invalid geometries found in ", new_geom_name,
              " might cause errors\nRepair first with ",
              "sf::st_make_valid(", new_geom_name, ")")
    new_geom <- sf::as_Spatial(new_geom)
  }

  if (inherits(weight_matrix, "sf"))
    weight_matrix <- sf::as_Spatial(weight_matrix)
  if (!is.null(weight_matrix) &
      !inherits(weight_matrix, "SpatialPointsDataFrame"))
    stop("`weight_matrix` should be a `SpatialPointsDataFrame` or `sf` object")

  # check IDs and variables -------------------------------------------------


  if (!(old_ID %in% names(old_geom@data)))
    stop(paste(old_ID, "is not a variable from", old_geom_name))
  if (!(new_ID %in% names(new_geom@data)))
    stop(paste(new_ID, "is not a variable from", new_geom_name))

  # check variables
  if (sum(!(variables %in% names(data))) > 0)
    stop(paste(variables[ !(variables %in% names(data)) ],
               "is not a variable from", data_name))

  ###
  ### exclude non-numeric variables from reapportionment
  ###
  excl <- base::setdiff(variables, names(Filter(is.numeric, data[, variables])))
  if (length(excl) > 0) {
    variables <- variables[ !variables %in% excl ]
    warning("non-numeric variable(s) ignored: ", paste(excl, collapse = ", "))
  }


  # check weights (if requested) --------------------------------------------


  if (mode %in% "proportion" & is.null(weights))
    stop('`weights` required when `mode` = "proportion"')
  if (!mode %in% "proportion" & !is.null(weights))
    stop('`mode` should be set to "proportion" when `weights` are provided')
  if (mode %in% "proportion")
    if (!(weights %in% names(data)))
      stop(paste(weights, "is not a variable from", data_name))


  # check polygons and IDs --------------------------------------------------


  # if several polygons with the same ID, merge them

  # if (length(old_geom@data[,old_ID]) > length(unique(old_geom@data[,old_ID]))) {
  #   df <- old_geom@data[match(unique(old_geom@data[,old_ID]),old_geom@data[,old_ID]),]
  #   old_geom <- unionSpatialPolygons(old_geom, old_geom@data[,old_ID])
  #   old_geom <- SpatialPolygonsDataFrame(old_geom, df, old_ID)
  # }
  ###
  ### faster method, using {sf} (thanks to Roger Bivand)
  ###
  old_geom <- sfAggregate(old_geom, old_ID)

  # if (length(new_geom@data[,new_ID]) > length(unique(new_geom@data[,new_ID]))) {
  #   df <- new_geom@data[match(unique(new_geom@data[,new_ID]),new_geom@data[,new_ID]),]
  #   new_geom <- unionSpatialPolygons(new_geom, new_geom@data[,new_ID])
  #   new_geom <- SpatialPolygonsDataFrame(new_geom, df, new_ID)
  # }
  ###
  ### faster method, using {sf} (thanks to Roger Bivand)
  ###
  new_geom <- sfAggregate(new_geom, new_ID)

  ###
  ### now done above after merging polygons with duplicated IDs
  ###
  # # make sure SPDF IDs are OK
  # old_geom <- sp::spChFIDs(old_geom, as.character(old_geom@data[,old_ID]))
  # new_geom <- sp::spChFIDs(new_geom, as.character(new_geom@data[,new_ID]))

  names(data)[names(data) %in% data_ID] <- "old_ID"
  data$old_ID <- as.character(data$old_ID) # for compatibility with intdf later

  # make sure both SPDFs have the same projection
  ###
  ### performed with {sf}
  ###
  # if (sf::st_crs(old_geom) != sf::st_crs(new_geom)) {
  if (!sp::identicalCRS(old_geom, new_geom)) {
    message("Reprojecting ", new_geom_name, " to the same projection as ",
            old_geom_name, "...")
    ###
    ### performed with {sf}
    ###
    # new_geom <- sf::st_transform(new_geom, crs = sf::st_crs(old_geom))
    new_geom <- sp::spTransform(new_geom, old_geom@proj4string)
  }


  # apply weights -----------------------------------------------------------


  # use weight matrix if provided
  if (!is.null(weight_matrix)) {
    ###
    ### warn about lack of testing
    ###
    warning("Use of `weight_matrix` not yet tested (sorry), use carefully.")

    if (old_ID %in% names(weight_matrix@data)) {
      weight_matrix@data <- weight_matrix@data[, -match(old_ID, names(weight_matrix@data)) ]
    }
    ###
    ### switch to sf::st_within
    ###
    # weight_matrix <- weight_matrix[colSums(rgeos::gWithin(weight_matrix, old_geom, byid = TRUE)) > 0,]
    weight_matrix <- weight_matrix[ colSums(sf::st_within(weight_matrix, old_geom)) > 0, ]
    weight_matrix_total <- sum(weight_matrix@data[, weight_matrix_var], na.rm = TRUE)
    ###
    ### note: `sp::over` should be replaceable with `sf::st_intersects`
    ###
    weight_matrix@data <- cbind(weight_matrix@data, sp::over(weight_matrix, old_geom))
  }

  ###
  ### switch (or switch back) to {sf} version of the objects
  ###
  new_geom <- sf::st_as_sf(new_geom)
  old_geom <- sf::st_as_sf(old_geom)


  # find intersection -------------------------------------------------------


  # start by trimming out areas that don't intersect

  ###
  ### switch to `st_intersects`
  ###
  # old_geom_sub <- rgeos::gIntersects(old_geom, new_geom, byid = TRUE) # test for areas that don't intersect
  old_geom_sub <- sf::st_intersects(old_geom, new_geom) # test for areas that don't intersect
  old_geom_sub2 <- apply(old_geom_sub, 2, sum) # test across all polygons in the SpatialPolygon whether it intersects or not
  old_geom_sub3 <- old_geom[ old_geom_sub2 > 0, ] # keep only the ones that actually intersect

  ###
  ### switch to `st_intersection`
  ###
  # perform the intersection. This takes a while since it also calculates area and other things, which is why we trimmed out irrelevant areas first
  # int <- rgeos::gIntersection(old_geom_sub3, new_geom, byid = TRUE, drop_lower_td = TRUE) # intersect the polygon and your administrative boundaries
  int <- suppressWarnings(sf::st_intersection(old_geom_sub3, new_geom)) # intersect the polygon and your administrative boundaries

  ###
  ### emulate how `rgeos::gIntersection` handled row names
  ###
  rownames(int) <- paste(sf::st_drop_geometry(int)[, old_ID ],
                         sf::st_drop_geometry(int)[, new_ID ])

  ###
  ### crucial difference with {rgeos} -- get IDs from `rownames` instead of
  ### getting them from `names`
  ###
  # intdf <- data.frame(intname = names(int)) # make a data frame for the intersected SpatialPolygon, using names from the output list from int
  intdf <- data.frame(intname = rownames(int)) # make a data frame for the intersected SpatialPolygon, using names from the output list from int
  intdf$intname <- as.character(intdf$intname) # convert the name to character
  splitid <- strsplit(intdf$intname, " ", fixed = TRUE) # split the names
  splitid <- do.call("rbind", splitid) # rbind those back together
  colnames(splitid) <- c("old_ID", "new_ID") # now you have the administrative area ID and the polygonID as separate variables in a dataframe that correspond to the int SpatialPolygon.
  intdf <- data.frame(intdf, splitid) # make that into a dataframe
  intdf$old_ID <- as.character(intdf$old_ID) # convert to character
  intdf$new_ID <- as.character(intdf$new_ID) # convert to character.

  # now you have a dataframe corresponding to the intersected SpatialPolygon object

  if (!is.null(weight_matrix)) {
    ###
    ### warn about lack of testing (already done earlier)
    ###
    # warning("use of weight matrix not yet tested")

    # # check in which intersected polygon each point stands
    weight_matrix_int <- sp::over(weight_matrix, int)
    # use points weights to reapportion
    intdf$polyarea <- purrr::map_int(1:length(int), ~ sum(weight_matrix@data[ weight_matrix_int %in% .x, weight_matrix_var ]))
    data$departarea <- purrr::map_int(old_geom@data[, old_ID], ~ sum(weight_matrix@data[ weight_matrix@data[, old_ID ] %in% .x, weight_matrix_var ]))[ match(data[[ "old_ID" ]], old_geom@data[[ old_ID ]]) ]
  } else {
    ###
    ### switch to `st_area`
    ###
    # if we don't have weights we just use areas
    # intdf$polyarea <- rgeos::gArea(int, byid = TRUE) # get area from the polygon SP object and put it in the df
    intdf$polyarea <- sf::st_area(int) # get area from the polygon SP object and put it in the df
    ###
    ### switch to `st_area` (again)
    ###
    # data$departarea <- rgeos::gArea(old_geom, byid = TRUE)[match(data$old_ID, old_geom@data[, old_ID])]
    data$departarea <- sf::st_area(old_geom)[ match(data$old_ID, old_geom[[ old_ID ]]) ]
  }

  ###
  ### note: `left_join` is much faster than `base::merge`
  ###
  # join together the two dataframes by the administrative ID
  intdf2 <- dplyr::left_join(intdf, data, by = c("old_ID" = "old_ID"))
  if (mode %in% "count") {
    ###
    ### note: {dplyr} code roughly 1.5x faster than older {plyr} code
    ###
    ## changed for {tidyselect} 1.2.0
    # intpop <- dplyr::mutate_at(intdf2, variables, ~ .x * (.data$polyarea /
    #                                                         .data$departarea))
    # intpop <- dplyr::mutate_at(intdf2, variables,
    #                            ~ .x * (dplyr::all_of(polyarea) /
    #                                      dplyr::all_of(departarea)))
    ## new variable creation forced to avoid using `all_of`
    intdf2$weights <- (intdf2$polyarea / intdf2$departarea)
    ## next line replaces the `mutate_at` line commented out above
    intpop <- dplyr::mutate_at(intdf2, variables, ~ .x * weights)
    # remove `units` type (drastically speeds up the `sum` operation below)
    intpop <- dplyr::mutate_at(intpop, variables, as.double)
    # subset to target variables
    intpop <- dplyr::select(intpop, new_ID, dplyr::all_of(variables))
    # aggregate by sum (as Joël wrote, other functions might be suitable)
    intpop <- dplyr::group_by(intpop, new_ID)
    intpop <- dplyr::summarise_at(intpop, variables, sum, na.rm = TRUE)

  } else if (mode %in% "proportion") {
    ###
    ### note: {dplyr} code roughly 1.5x faster than older {plyr} code
    ###
    # subset to target variables (incl. weights)
    ## changed for {tidyselect} 1.2.0
    # intpop <- dplyr::select(intdf2, new_ID, dplyr::all_of(variables),
    #                         .data$polyarea, .data$departarea,
    #                         weights = dplyr::all_of(weights))
    intpop <- dplyr::select(intdf2, new_ID, dplyr::all_of(variables),
                            dplyr::all_of("polyarea"),
                            dplyr::all_of("departarea"),
                            weights = dplyr::all_of(weights))
    ## changed for {tidyselect} 1.2.0
    # intpop <- dplyr::mutate_at(intpop, variables,
    #                            ~ .x * weights * (.data$polyarea /
    #                                                .data$departarea))
    ## before {tidyselect} 1.2.0, the code started with `mutate_at` and then
    ## continued with the line below -- the new code works the other way round
    intpop$weights <- intpop$weights * (intpop$polyarea / intpop$departarea)
    ## next line replaces the `mutate_at` line commented out above
    intpop <- dplyr::mutate_at(intpop, variables, ~ .x * weights)
    # remove `units` type (drastically speeds up the `sum` operation below)
    intpop <- dplyr::mutate_at(intpop, c(variables, "weights"), as.double)
    # aggregate by sum (as Joël wrote, other functions might be suitable)
    intpop <- dplyr::group_by(intpop, new_ID)
    intpop <- dplyr::summarise_at(intpop, c(variables, "weights"),
                                  sum, na.rm = TRUE)
    intpop <- dplyr::mutate_at(intpop, variables, ~ .x / weights)
    names(intpop)[length(names(intpop))] <- weights

  }

  names(intpop)[1] <- new_ID

  ###
  ### return data.frame instead of a tibble, for strict compatibility with the
  ### output of {spReapportion}
  ###
  return(as.data.frame(intpop)) # done!

}

# kthxbye

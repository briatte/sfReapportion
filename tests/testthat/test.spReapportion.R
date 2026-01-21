##
## TODO: sort out which tests should really be skipped on CRAN
##

test_that("sfReapportion reapportions data correctly", {

  skip_on_cran()

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  CS_ParisPollingStations <- sfReapportion(ParisIris,
                                           ParisPollingStations2012,
                                           RP_2011_CS8_Paris,
                                           "DCOMIRIS", "ID", "IRIS")
  expect_equal_to_reference(CS_ParisPollingStations,
                            "CS_ParisPollingStations.rds",
                            update = FALSE)

})

test_that("sfReapportion works with `sf` objects", {

  skip_on_cran()

  library(sf)

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  # convert both geoms to `sf`
  ParisPollingStations2012 <- sf::st_as_sf(ParisPollingStations2012)
  ParisIris <- sf::st_as_sf(ParisIris)

  CS_ParisPollingStations <- sfReapportion(ParisIris,
                                           ParisPollingStations2012,
                                           RP_2011_CS8_Paris,
                                           "DCOMIRIS", "ID", "IRIS")
  expect_equal_to_reference(CS_ParisPollingStations,
                            "CS_ParisPollingStations.rds",
                            update = FALSE)

})

test_that("sfReapportion can merge polygons", {

  skip_on_cran()

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  # create duplicated IDs
  x <- seq(42, 42 + 150, by = 2)
  ParisIris@data[["DCOMIRIS"]][ x ] <- ParisIris@data[["DCOMIRIS"]][ x + 1 ]

  testthat::expect_message(sfReapportion(ParisIris,
                                         ParisPollingStations2012,
                                         RP_2011_CS8_Paris,
                                         "DCOMIRIS", "ID", "IRIS"),
                           "^Merging 76 SpatialPolygons with duplicate IDs")

})

###
### this test did not work in {spReapportion} due to the omission of
### `mode = "proportion` -- the reference data was re-saved in v0.1.3
###
test_that("sfReapportion reapportions data correctly with proportions data", {

  skip_on_cran()

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  RP_2011_CS8_Paris[, paste0("CS", 1:8, "pc")] <- RP_2011_CS8_Paris[, paste0("C11_POP15P_CS", 1:8)] / RP_2011_CS8_Paris$C11_POP15P * 100
  CS_ParisPollingStationsProp <- sfReapportion(ParisIris,
                                               ParisPollingStations2012,
                                               RP_2011_CS8_Paris,
                                               "DCOMIRIS", "ID", "IRIS",
                                               variables = paste0("CS", 1:8, "pc"),
                                               weights = "C11_POP15P",
                                               mode = "proportion")
  expect_equal_to_reference(CS_ParisPollingStationsProp,
                            "CS_ParisPollingStationsprop.rds",
                            update = FALSE)

})

test_that("NA values are handled correctly", {

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  CS_ParisPollingStations <- sfReapportion(ParisIris[ParisIris@data$DEPCOM %in% "75104",], ParisPollingStations2012[ParisPollingStations2012@data$arrondisse %in% "4",], RP_2011_CS8_Paris[substr(RP_2011_CS8_Paris$IRIS, 1, 5) %in% "75104",], "DCOMIRIS", "ID", "IRIS")
  expect_equal(sum(is.na(CS_ParisPollingStations)), 0)

  RP_2011_CS8_Paris[RP_2011_CS8_Paris$IRIS %in% "751041399", -1] <- NA

  CS_ParisPollingStations <- sfReapportion(ParisIris[ParisIris@data$DEPCOM %in% "75104",], ParisPollingStations2012[ParisPollingStations2012@data$arrondisse %in% "4",], RP_2011_CS8_Paris[substr(RP_2011_CS8_Paris$IRIS, 1, 5) %in% "75104",], "DCOMIRIS", "ID", "IRIS")
  expect_equal(sum(is.na(CS_ParisPollingStations)), 0)

})

###
### test against {areal}
###

test_that("sfReapportion and areal results match (our data example)", {

  skip_on_cran()

  skip_if_not_installed("areal")
  library(areal)
  library(dplyr)
  library(sf)

  data(ParisPollingStations2012)
  data(ParisIris)
  data(RP_2011_CS8_Paris)

  CS_ParisPollingStations <- sfReapportion(ParisIris,
                                           ParisPollingStations2012,
                                           RP_2011_CS8_Paris,
                                           "DCOMIRIS",
                                           "ID",
                                           "IRIS")

  # with {areal}
  ParisPollingStations2012_sf <- sf::st_as_sf(ParisPollingStations2012)
  ParisIris_sf <- sf::st_as_sf(ParisIris) %>%
    left_join(RP_2011_CS8_Paris, by = c("DCOMIRIS" = "IRIS"))

  areal_equiv <- areal::aw_interpolate(ParisPollingStations2012_sf, tid = ID,
                                       source = ParisIris_sf, sid = DCOMIRIS,
                                       weight = "total", output = "tibble",
                                       extensive = c("C11_POP15P",
                                                     "C11_POP15P_CS1",
                                                     "C11_POP15P_CS2"))

  testthat::expect_equal(as_tibble(arrange(CS_ParisPollingStations[, 1:4], ID)),
                         select(areal_equiv, ID, C11_POP15P,
                                C11_POP15P_CS1, C11_POP15P_CS2) %>%
                           arrange(ID))

})

test_that("sfReapportion and areal results match (areal data example)", {

  skip_on_cran()

  with_areal <- areal::aw_interpolate(areal::ar_stl_wards, tid = WARD,
                                      source = areal::ar_stl_race, sid = GEOID,
                                      weight = "total",
                                      output = "tibble",
                                      extensive = c("TOTAL_M", "TOTAL_E")) %>%
    mutate(WARD = as.character(WARD)) %>%
    select(WARD, TOTAL_E, TOTAL_M) %>%
    arrange(WARD)

  with_ours <- sfReapportion(sf::st_transform(areal::ar_stl_race, "WGS84"),
                             sf::st_transform(areal::ar_stl_wards, "WGS84"),
                             sf::st_drop_geometry(areal::ar_stl_race),
                             variables = c("TOTAL_M", "TOTAL_E"),
                             "GEOID",
                             "WARD",
                             "GEOID") %>%
    select(WARD, TOTAL_E, TOTAL_M) %>%
    arrange(WARD) %>%
    as_tibble()

  testthat::expect_equal(with_areal, with_ours, tolerance = 1e-06)

})

# done

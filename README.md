<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/briatte/sfReapportion/graph/badge.svg)](https://app.codecov.io/gh/briatte/sfReapportion)
<!-- badges: end -->

A quick port of Joël Gombin's [`spReapportion`](https://github.com/joelgombin/spReapportion) package, without the dependencies to the [`maptools`][maptools] and [`rgeos`][rgeos] packages, which were [both retired][retired] in 2023, and with additional support for [`sf`][sf] objects.

[maptools]: https://cran.r-project.org/package=maptools
[rgeos]: https://cran.r-project.org/package=rgeos
[retired]: https://r-spatial.org/r/2022/04/12/evolution.html
[sf]: https://r-spatial.github.io/sf/index.html

The `sfReapportion` function is intended as a drop-in replacement for Joël's `spReapportion` function: it takes exactly the same arguments, and outputs exactly the same kind of results, also as a data frame.

__Very much work in progress:__

- use of `weights` with `mode = "proportion"` has only been lightly tested
- use of `weight_matrix` and `weight_matrix_var` has not been tested

The package was ported in order to be used in [this project](https://github.com/briatte/selection-bv).

## Installation

```r
install.packages("remotes")
remotes::install_github("briatte/sfReapportion")
```

## Example

```r
library(sfReapportion)

# Joël's example
data(ParisPollingStations2012)
data(ParisIris)
data(RP_2011_CS8_Paris)

CS_ParisPollingStations <- sfReapportion(ParisIris, 
                                         ParisPollingStations2012, 
                                         RP_2011_CS8_Paris, 
                                         "DCOMIRIS", 
                                         "ID", 
                                         "IRIS")
```
```
# our results
> head(CS_ParisPollingStations)[, 1:4]
         ID C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2
1 750010001   1385.539    0.007534526       88.64129
2 750010002   1389.989    1.158961646       65.49323
3 750010003   1921.008    3.410431283       85.39031
4 750010004   1577.544    2.504085472       62.79095
5 750010005   1802.787    1.803202193       68.64467
6 750010006   1619.144    5.790202710       83.05042
```
```
# compare to Joël's test file
> head(readRDS("tests/testthat/CS_ParisPollingStations.rds"))[, 1:4]
         ID C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2
1 750010001   1385.539    0.007534526       88.64129
2 750010002   1389.989    1.158961646       65.49323
3 750010003   1921.008    3.410431283       85.39031
4 750010004   1577.544    2.504085472       62.79095
5 750010005   1802.787    1.803202193       68.64467
6 750010006   1619.144    5.790202710       83.05042
```

The results match those of the [`areal`][areal] package:

```r
library(areal)
library(dplyr)
library(sf)

ParisPollingStations2012_sf <- sf::st_as_sf(ParisPollingStations2012)
ParisIris_sf <- sf::st_as_sf(ParisIris) %>%
  left_join(RP_2011_CS8_Paris, by = c("DCOMIRIS" = "IRIS"))

areal_equiv <- areal::aw_interpolate(ParisPollingStations2012_sf, tid = ID,
                                     source = ParisIris_sf, sid = DCOMIRIS,
                                     weight = "total", output = "sf",
                                     extensive = c("C11_POP15P",
                                                   "C11_POP15P_CS1",
                                                   "C11_POP15P_CS2"))
```
```
# match display of previous results
> select(areal_equiv, ID, C11_POP15P, C11_POP15P_CS1, C11_POP15P_CS2) %>%
+   arrange(ID) %>%
+   sf::st_drop_geometry() %>%
+   head()
         ID C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2
1 750010001   1385.539    0.007534526       88.64129
2 750010002   1389.989    1.158961646       65.49323
3 750010003   1921.008    3.410431283       85.39031
4 750010004   1577.544    2.504085472       62.79095
5 750010005   1802.787    1.803202193       68.64467
6 750010006   1619.144    5.790202710       83.05042
```

[areal]: https://cran.r-project.org/package=areal

The package contains further tests against the [`st_interpolate_aw`][st_interpolate_aw] function of the [`sf`][sf] package.

[st_interpolate_aw]: https://r-spatial.github.io/sf/reference/interpolate_aw.html

## See also

- [Areal Interpolation in R][prener2] in the [`areal`][areal] package
- [Areal Weighted Interpolation][prener1] in the [`areal`][areal] package
- ["Area-weighted interpolation"][sds53] (in [_Spatial Data Science_][sds], 2023)
- [Population Downscaling Using Areal Interpolation - A Comparative Analysis][batsaris] in the [`populR`][populR] package
- [R-spatial evolution: retirement of rgdal, rgeos and maptools][retired] (2022)
- [r-spatial/sf#2563][issue2563] for a technical discussion on merging polygons

I also wrote a [very short blog post](https://f.briatte.org/r/sfReapportion) to illustrate what the package does.

[batsaris]: https://cran.r-project.org/web/packages/populR/vignettes/areal-interpolation-comparison.html
[issue2563]: https://github.com/r-spatial/sf/issues/2563
[populR]: https://cran.r-project.org/package=populR
[prener1]: https://cloud.r-project.org/web/packages/areal/vignettes/areal-weighted-interpolation.html
[prener2]: https://cloud.r-project.org/web/packages/areal/vignettes/areal.html
[sds53]: https://r-spatial.org/book/05-Attributes.html#sec-area-weighted
[sds]: https://r-spatial.github.io/sf/index.html

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/briatte/sfReapportion/graph/badge.svg)](https://app.codecov.io/gh/briatte/sfReapportion)
<!-- badges: end -->

A quick port of Joël Gombin's [`spReapportion`](https://github.com/joelgombin/spReapportion) package, without the dependencies to the [`maptools`][maptools] and [`rgeos`][rgeos] packages, which were [both retired][retired] in 2023, and with additional support for [`sf`][sf] objects.

[maptools]: https://CRAN.R-project.org/package=maptools
[rgeos]: https://CRAN.R-project.org/package=rgeos
[retired]: https://r-spatial.org/r/2022/04/12/evolution.html
[sf]: https://r-spatial.github.io/sf/index.html

The `sfReapportion` function is intended as a drop-in replacement for Joël's `spReapportion` function: it takes exactly the same arguments, and outputs exactly the same kind of results, also as a data frame.

__Use the following options with caution:__

- use of `weights` with `mode = "proportion"` has only been lightly tested
- use of `weight_matrix` and `weight_matrix_var` has only been lightly tested

The package was ported in order to be used in [this project](https://github.com/briatte/selection-bv).

## Installation

If the package is not available from CRAN, install from GitHub instead:

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

[areal]: https://CRAN.R-project.org/package=areal

The package contains further tests against the [`st_interpolate_aw`][st_interpolate_aw] function of the [`sf`][sf] package.

[st_interpolate_aw]: https://r-spatial.github.io/sf/reference/interpolate_aw.html

## Using a weights matrix

The package contains an example weights matrix that contains voter addresses located in the 20th arrondissement of Paris. The weighting variable, `nb_adresses`, only approximately counts voters at a given location (the number of voters at each address is unknown).

The data were obtained by subsetting from the _[Répertoire électoral unique][reu]_ (REU):

```r
library(arrow)
library(dplyr)
library(sf)

# subset polling stations
bv20 <- arrow::read_parquet("table-bv-reu.parquet") %>%
  dplyr::filter(code_commune %in% c("75120")) %>%
  dplyr::select(id_brut_reu, libelle_reu, nb_adresses)

# subset voter addresses (slow)
addr20 <- arrow::read_parquet("table-adresses-reu.parquet") %>%
    dplyr::filter(id_brut_bv_reu %in% unique(bv20$id_brut_reu))

# convert to spatial points
Paris20eAddresses <- addr20 %>% 
  dplyr::group_by(geo_adresse, longitude, latitude) %>%
  dplyr::summarise(nb_adresses = sum(nb_adresses)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(4326)

# save to .rda (LazyData: true)
save(Paris20eAddresses, file = "Paris20eAddresses.rda", compress = "xz")
```

[reu]: https://www.data.gouv.fr/datasets/bureaux-de-vote-et-adresses-de-leurs-electeurs

The results can be tested by subsetting the rest of the test data included in the package to the 20th arrondissement of Paris:

```r
library(dplyr)
library(ggplot2)
library(sf)
library(sfReapportion)

# spatial points of voter addresses in Paris 20th district
data(Paris20eAddresses)

data(ParisPollingStations2012)
data(ParisIris)
data(RP_2011_CS8_Paris)

# subset geometry of polling stations (new geom, 76 polling stations)
ParisPollingStations2012 <- sf::st_as_sf(ParisPollingStations2012) %>%
  dplyr::filter(arrondisse %in% c(20)) %>%
  dplyr::mutate(id_brut_bv_reu = paste("75020_", num_bv))

# subset geometry of census tracts (old geom, 356 polygons)
# will throw a warning about polygons expected to be spatially constant
ParisIris <- sf::st_as_sf(ParisIris) %>%
  sf::st_intersection(ParisPollingStations2012)

# this is what we're reapportioning
ggplot(ParisIris) +
  geom_sf() +
  geom_sf(data = Paris20eAddresses, aes(size = nb_adresses), alpha = 1/4) +
  scale_size_area(max_size = 10) +
  theme_void()

# subset census data to reapportion (93 distinct census tracts)
RP_2011_CS8_Paris <- dplyr::filter(RP_2011_CS8_Paris,
                                   IRIS %in% ParisIris$DCOMIRIS)
```

Comparing weighted and unweighted results is probably a good idea:

```r
# unweighted
r1 <- sfReapportion(ParisIris, ParisPollingStations2012, RP_2011_CS8_Paris,
                    "DCOMIRIS", "ID", "IRIS")

# weighted
# will throw a warning about `weight_matrix` having only been lightly tested
r2 <- sfReapportion(ParisIris, ParisPollingStations2012,
                    RP_2011_CS8_Paris,
                    "DCOMIRIS", "ID", "IRIS",
                    weight_matrix = Paris20eAddresses,
                    weight_matrix_var = "nb_adresses")
```

The differences are non-trivial, at least when looking at adult population and socio-professional categories as we do in this example:

```
> # unweighted
> head(r1[, 1:4])
         ID C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2
1 750200001   2026.819              0       56.46644
2 750200002   1465.669              0       34.65999
3 750200003   2308.119              0       60.81294
4 750200004   1934.005              0       53.48857
5 750200005   1886.533              0       45.53472
6 750200006   1873.657              0       44.32671
>
> # weighted
> head(r2[, 1:4])
         ID C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2
1 750200001   2006.434              0       54.75713
2 750200002   1543.915              0       35.65698
3 750200003   1930.616              0       51.33953
4 750200004   2135.882              0       60.72449
5 750200005   1925.147              0       46.37813
6 750200006   1953.638              0       47.51444
>
> # correlations
> round(diag(cor(r1[,-1], r2[,-1])), 2)
    C11_POP15P C11_POP15P_CS1 C11_POP15P_CS2 C11_POP15P_CS3 C11_POP15P_CS4 
          0.30           0.75           0.58           0.46           0.30 
C11_POP15P_CS5 C11_POP15P_CS6 C11_POP15P_CS7 C11_POP15P_CS8 
          0.48           0.70           0.26           0.52
```

## See also

- [Areal Interpolation in R][prener2] in the [`areal`][areal] package
- [Areal Weighted Interpolation][prener1] in the [`areal`][areal] package
- ["Area-weighted interpolation"][sds53] (in [_Spatial Data Science_][sds], 2023)
- [Guidelines on areal interpolation methods][do20]
- [Population Downscaling Using Areal Interpolation - A Comparative Analysis][batsaris] in the [`populR`][populR] package
- [R-spatial evolution: retirement of rgdal, rgeos and maptools][retired] (2022)
- [r-spatial/sf#2563][issue2563] for a technical discussion on merging polygons

I also wrote a [very short blog post](https://f.briatte.org/r/sfReapportion) to illustrate what the package does.

[batsaris]: https://CRAN.R-project.org/package=populR/vignettes/areal-interpolation-comparison.html
[do20]: http://www.thibault.laurent.free.fr/code/areal/
[issue2563]: https://github.com/r-spatial/sf/issues/2563
[populR]: https://CRAN.R-project.org/package=populR
[prener1]: https://CRAN.R-project.org/package=areal/vignettes/areal-weighted-interpolation.html
[prener2]: https://CRAN.R-project.org/package=areal/vignettes/areal.html
[sds53]: https://r-spatial.org/book/05-Attributes.html#sec-area-weighted
[sds]: https://r-spatial.github.io/sf/index.html

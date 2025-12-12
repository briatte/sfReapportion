A quick port of Joël Gombin's 
[`spReapportion`](https://github.com/joelgombin/spReapportion) package, 
without the dependencies to the `maptools` and `rgeos` packages, which were 
both retired in 2023, and with additional support for `sf` objects.

The `sfReapportion` function is intended as a drop-in replacement for Joël's
`spReapportion` function: it takes exactly the same arguments, and outputs 
exactly the same kind of results, also as a data frame.

__Very much work in progress:__

- passing `sf` instead of `Spatial` objects is experimental
- merging polygons with duplicated IDs is experimental
- using weights (`mode = "proportion"`) is not yet supported
- using a weights matrix is not yet supported either

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

# N.B. weight matrix not yet supported
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

## See also

`sf::st_interpolate_aw`

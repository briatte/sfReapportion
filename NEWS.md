# version 0.1.7 (2026-01-21)

- losing dependency to `rlang` (removed `.data` pronoun from tidy-selections)
- added two data equality tests against the `areal` package

# version 0.1.6 (2026-01-20)

- require a recent version of `sp` to avoid any calls to `rgdal` or `rgeos` (#3)

# version 0.1.5 (2025-12-29)

- warn in case of invalid geometries in `sf` objects (see #2)

# version 0.1.4 (2025-12-26)

- updated documentation
- provide code to match results with those of the `areal` package

# version 0.1.3 (2025-12-16)

- much faster code for merging polygons using `sf` (thanks to Roger Bivand)
- `mode = "proportion"` with `weights` re-implemented (__lightly tested__ only)
- `weight_matrix` re-implemented (__untested__, will throw a warning)
- additional tests
- package passes `R CMD CHECK`

# version 0.1.2 (2025-12-10)

- support for merging polygons using `sf` (slow, and __lightly tested__ only)

# version 0.1.1 (2025-12-09)

- additional check on `variables` (numeric only)
- allowing `sf` objects as inputs (__untested__)
- losing dependency to `plyr` (faster code, too)

# version 0.1.0 (2025-12-09)

- `sfReapportion` seems to work without calling `rgeos` or `maptools`

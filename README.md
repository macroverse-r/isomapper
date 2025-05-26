
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isomapper

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end -->

The goal of isomapper is to provide utilities for working with country
codes, ISO standards, and geographic entity mapping. It includes
functions for converting between different coding schemes, standardizing
country names, and retrieving geographic metadata.

This package is part of the [macroverse
ecosystem](https://github.com/macroverse-r/macroverse).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macroverse-r/isomapper")
```

## Examples

### Converting Country Names to ISO Codes

``` r
library(isomapper)

# Convert country names to ISO3 codes
im_ctry2iso(c("United States", "Germany", "Japan"))
#> [1] "USA" "DEU" "JPN"

# Handles various formats
im_ctry2iso(c("USA", "U.S.A.", "United States of America"))
#> [1] "USA" "USA" "USA"

# Works with partial matches (if unambiguous)
im_ctry2iso(c("Franc", "Deutsch"), min_letter = 5)
#> [1] "FRA" "DEU"
```

### Getting Country Information from ISO Codes

``` r
# Get country names
im_from_iso(c("USA", "FRA", "CHN"), opt = "Name")
#> [1] "United States" "France" "China"

# Get regions
im_from_iso(c("USA", "FRA", "CHN"), opt = "Region")
#> [1] "AMERICAS" "EUROPE" "ASIA"

# Get economic classification
im_from_iso(c("USA", "CHN", "ETH"), opt = "Center-Periphery")
#> [1] "CTR_LDR" "SMP_LDR" "PERI"

# Get ISO2 codes
im_from_iso(c("USA", "GBR", "FRA"), opt = "ISO2")
#> [1] "US" "GB" "FR"
```

### Working with Country Categories

``` r
# Get all EU countries
eu_countries <- im_get_category("EU")

# Get BRICS countries
brics <- im_get_category("BRICS")

# Get multiple categories
g7_and_brics <- im_get_category(c("G7", "BRICS"))

# Exclude specific countries
europe_except_uk <- im_get_category("EUROPE-GBR")

# Get oil exporters
oil_exporters <- im_get_category("HYD_EXP")
```

## Available Categories

The package includes numerous predefined categories:

- **Economic Groups**: G7, G20, OECD, EU, ASEAN, BRICS, OPEC, etc.
- **Geographic Regions**: AFRICA, AMERICAS, ASIA, EUROPE, OCEANIA
- **Sub-regions**: NORTH_AFRICA, EAST_ASIA, WESTERN_EUROPE, etc.
- **Economic Classification**:
  - CTR_LDR (Center Leaders)
  - CTR_FOL (Center Followers)
  - SMP_LDR (Semi-periphery Leaders)
  - SMP_FOL (Semi-periphery Followers)
  - PERI (Periphery)
- **Resource Categories**:
  - HYD_EXP (Hydrocarbon Exporters)
  - HYD_IMP (Hydrocarbon Importers)
  - NRS_REN (Natural Resource Rent)

## Data

The package includes comprehensive ISO code data:

``` r
# Load ISO data
data(ISO_DATA)

# Access country mappings
head(list_ctry2iso)
head(list_iso2country)

# View available categories
list_categories
```

## macroverse Ecosystem

The isomapper package is used throughout the macroverse ecosystem: -
**[mvcommon](https://github.com/macroverse-r/mvcommon)**: Common
utilities and validation -
**[pplot](https://github.com/macroverse-r/pplot)**: Panel data
visualization - **isomapper**: ISO codes and country mapping (this
package) - **[macrodata](https://github.com/macroverse-r/macrodata)**:
Data loading and processing -
**[mvlazy](https://github.com/macroverse-r/mvlazy)**: Convenience
functions -
**[macroverse](https://github.com/macroverse-r/macroverse)**:
Meta-package loading all components

## License

This package is available under a dual-licensing model: - **Open
Source**: AGPL-3.0 for academic and non-commercial use - **Commercial**:
Alternative licensing available for commercial applications

See LICENSE.md for details.

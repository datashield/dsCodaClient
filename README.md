# dsCodaClient

DataSHIELD client-side functions for compositional data analysis (CoDa).
Compositional data are vectors of parts that sum to a constant, such as
the 24-hour movement behaviour composition (sleep, sedentary behaviour and
physical activity intensities) studied in the ProPASS consortium.
`dsCodaClient` drives the server-side package
[dsCoda](https://github.com/datashield/dsCoda), which wraps the
[compositions](https://cran.r-project.org/package=compositions) and
[zCompositions](https://cran.r-project.org/package=zCompositions) packages,
so that closure, zero replacement, log-ratio transforms and downstream
analysis run across DataSHIELD servers without individual records leaving
them.

## Functions

| Function | What it does |
|---|---|
| `ds.lrEM()` | Replace zeros / values below detection limit with `zCompositions::lrEM()` (server-side object). |
| `ds.acomp()` | Create a closed Aitchison composition with `compositions::acomp()` (server-side object). |
| `ds.ilr()` | Compute isometric log-ratio coordinates, optionally with a user-supplied basis `V` (server-side object). |
| `ds.meanComp()` | Pooled geometric-mean composition across all servers, returned to the client. |
| `build_sequential_ilr_V()` | Build the ILR basis for a sequential binary partition, for use with `ds.ilr()`. |

## Installation

```r
# install.packages("remotes")
remotes::install_github("datashield/dsCodaClient")
```

`dsCodaClient` needs `DSI` (>= 1.8.0) and `dsBaseClient`;
`ds.meanComp()` additionally uses `dsTidyverseClient` and `compositions`
on the client. Once the package is published on the DataSHIELD CRAN
mirror it will also be installable with
`install.packages("dsCodaClient", repos = c("https://cran.obiba.org", "https://cloud.r-project.org"))`.

Every DataSHIELD server you connect to must have
[dsBase](https://github.com/datashield/dsBase),
[dsCoda](https://github.com/datashield/dsCoda) and, for the regression
workflow in the vignette,
[dsTidyverse](https://github.com/molgenis/ds-tidyverse) installed.

## Quick start

```r
library(DSI)
library(dsBaseClient)
library(dsCodaClient)

# conns <- DSI::datashield.login(logindata, assign = TRUE, symbol = "D")

parts <- c("vpa", "mpa", "lipa", "sleep", "sb")

ds.acomp(X = "D", parts = parts, objectname = "comp", datasources = conns)

V <- build_sequential_ilr_V(length(parts), parts)
ds.ilr(X = "comp", V = V, objectname = "ilr_sbp", datasources = conns)

ds.meanComp("comp", datasources = conns)
```

The vignette walks through the full workflow, including zero replacement
and a pooled regression on the ILR coordinates, using
[DSLite](https://cran.r-project.org/package=DSLite) so no Opal server is
needed: `vignette("dsCodaClient")` or the
[package site](https://datashield.github.io/dsCodaClient/).

## License

GPL-3. Developed by the ProPASS consortium and the DataSHIELD community.

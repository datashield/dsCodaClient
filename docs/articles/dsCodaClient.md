# Compositional data analysis with dsCodaClient

## What this package does

Compositional data (CoDa) are vectors of parts that carry relative
information only: the parts sum to a constant, so increasing one part
necessarily decreases the others, and standard statistics on the raw
parts are misleading. The usual remedy is to work with log-ratios of the
parts, which live in ordinary Euclidean space.

The running example is the 24-hour movement behaviour composition used
in the ProPASS consortium: minutes per day of vigorous physical activity
(`vpa`), moderate physical activity (`mpa`), light physical activity
(`lipa`), sleep (`sleep`) and sedentary behaviour (`sb`), which sum to
1440.

`dsCodaClient` lets you run that workflow across DataSHIELD servers
without seeing individual records. Every server needs `dsBase`, `dsCoda`
and (for the regression step) `dsTidyverse` installed.

## Data requirements

- Parts are columns of a data frame; one row per participant.
- Parts must be strictly positive. Zeros are not allowed in a
  composition, so replace them first (see below).
- Every server must hold the same parts under the same column names.

## A DSLite session with three studies

We simulate three studies. A handful of participants have zero minutes
of vigorous activity, which is what happens in real accelerometer data.

``` r
library(DSI)
library(DSLite)
library(dsBaseClient)
library(dsTidyverseClient)
library(dsCodaClient)

set.seed(1)
make_study <- function(n, zeros) {
  x <- data.frame(
    id = seq_len(n),
    vpa = rgamma(n, 2, 1) * 10 + 1,
    mpa = rgamma(n, 3, 1) * 30 + 1,
    lipa = rgamma(n, 4, 1) * 60 + 1,
    sleep = rnorm(n, 480, 40),
    sb = rnorm(n, 600, 60),
    bmi = rnorm(n, 26, 4)
  )
  x$vpa[sample(n, zeros)] <- 0
  x
}
study1 <- make_study(120, 8)
study2 <- make_study(150, 10)
study3 <- make_study(90, 6)

dslite.server <- newDSLiteServer(
  tables = list(study1 = study1, study2 = study2, study3 = study3),
  config = DSLite::defaultDSConfiguration(include = c("dsBase", "dsCoda", "dsTidyverse"))
)

builder <- DSI::newDSLoginBuilder()
for (s in c("study1", "study2", "study3")) {
  builder$append(server = s, url = "dslite.server", table = s, driver = "DSLiteDriver")
}
conns <- DSI::datashield.login(builder$build(), assign = TRUE, symbol = "D")
```

Against real servers you would build `logindata` with Opal or Armadillo
credentials; everything after
[`datashield.login()`](https://datashield.github.io/DSI/reference/datashield.login.html)
is identical.

## 1. Replace zeros before closing the composition

[`ds.lrEM()`](https://github.com/datashield/dsCodaClient/reference/ds.lrEM.md)
runs the log-ratio EM algorithm of
[`zCompositions::lrEM()`](https://rdrr.io/pkg/zCompositions/man/lrEM.html)
on the server. It replaces values flagged by `label` with imputed values
below the detection limits in `dl`, given on the same scale as the data
(minutes here). Do this on the raw parts, before
[`ds.acomp()`](https://github.com/datashield/dsCodaClient/reference/ds.acomp.md).

``` r
parts <- c("vpa", "mpa", "lipa", "sleep", "sb")

ds.select(df.name = "D", tidy_expr = list(vpa, mpa, lipa, sleep, sb),
          newobj = "D_parts", datasources = conns)

ds.lrEM(X = "D_parts", label = 0, dl = rep(1, length(parts)),
        objectname = "D_imp", datasources = conns)
```

``` r
ds.dim("D_imp", datasources = conns)
#> $`dimensions of D_imp in study1`
#> [1] 120   5
#> 
#> $`dimensions of D_imp in study2`
#> [1] 150   5
#> 
#> $`dimensions of D_imp in study3`
#> [1] 90  5
#> 
#> $`dimensions of D_imp in combined studies`
#> [1] 360   5
```

Note that `z.delete = TRUE` (the default) drops rows whose parts are all
zero, so check the dimensions still match `D` before combining objects.

## 2. Close the composition

[`ds.acomp()`](https://github.com/datashield/dsCodaClient/reference/ds.acomp.md)
creates a
[`compositions::acomp`](https://rdrr.io/pkg/compositions/man/acomp.html)
object on each server. Parts can be given by name or index; `total = 1`
rescales each row to proportions.

``` r
ds.acomp(X = "D_imp", parts = parts, objectname = "comp", datasources = conns)
```

``` r
ds.class("comp", datasources = conns)
#> $study1
#> [1] "acomp"
#> 
#> $study2
#> [1] "acomp"
#> 
#> $study3
#> [1] "acomp"
```

## 3. Isometric log-ratio coordinates

An ILR transform maps a D-part composition to D - 1 orthonormal
coordinates.
[`build_sequential_ilr_V()`](https://github.com/datashield/dsCodaClient/reference/build_sequential_ilr_V.md)
builds the basis for a sequential binary partition: coordinate 1
contrasts `vpa` against all other parts, coordinate 2 contrasts `mpa`
against the parts after it, and so on. Reorder `parts` if you want a
different first contrast.

``` r
V <- build_sequential_ilr_V(length(parts), parts)
round(V, 3)
#>         ilr1   ilr2   ilr3   ilr4
#> vpa    0.894  0.000  0.000  0.000
#> mpa   -0.224  0.866  0.000  0.000
#> lipa  -0.224 -0.289  0.816  0.000
#> sleep -0.224 -0.289 -0.408  0.707
#> sb    -0.224 -0.289 -0.408 -0.707
```

[`ds.ilr()`](https://github.com/datashield/dsCodaClient/reference/ds.ilr.md)
sends the basis to the servers and stores the coordinates. With
`V = NULL` the default basis of
[`compositions::ilrBase()`](https://rdrr.io/pkg/compositions/man/ilrBase.html)
is used instead.

``` r
ds.ilr(X = "comp", V = V, objectname = "ilr_sbp", datasources = conns)
```

## 4. Pooled mean composition

[`ds.meanComp()`](https://github.com/datashield/dsCodaClient/reference/ds.meanComp.md)
computes the mean of each ILR coordinate across all servers with
`dsBaseClient::ds.mean(type = "combine")` and inverts the transform on
the client. The result is the pooled geometric-mean composition, which
is the compositional analogue of the arithmetic mean.

``` r
mean_comp <- ds.meanComp("comp", datasources = conns)
round(mean_comp, 4)
#>    vpa    mpa   lipa  sleep     sb 
#> 0.0098 0.0561 0.1532 0.3474 0.4336
round(mean_comp * 1440)  # back to minutes per day
#>   vpa   mpa  lipa sleep    sb 
#>    14    81   221   500   624
```

## 5. Regression on the ILR coordinates

The coordinates are stored as a
[`compositions::rmult`](https://rdrr.io/pkg/compositions/man/rmult.html)
matrix, which
[`ds.glm()`](https://rdrr.io/pkg/dsBaseClient/man/ds.glm.html) cannot
address by column. Convert it to a tibble first; the columns come out
named `V1` to `V4`, where `V1` is the first column of `V` (`ilr1`), `V2`
is `ilr2`, and so on.

``` r
ds.as_tibble("ilr_sbp", newobj = "ilr_tbl", datasources = conns)
ds.colnames("ilr_tbl", datasources = conns)$study1
#> [1] "V1" "V2" "V3" "V4"

fit <- ds.glm(
  formula = "D$bmi ~ ilr_tbl$V1 + ilr_tbl$V2 + ilr_tbl$V3 + ilr_tbl$V4",
  family = "gaussian", datasources = conns
)
round(fit$coefficients, 3)
#>             Estimate Std. Error z-value p-value low0.95CI high0.95CI
#> (Intercept)   26.313      1.130  23.284   0.000    24.098     28.528
#> ilr_tbl$V1    -0.004      0.226  -0.019   0.985    -0.448      0.439
#> ilr_tbl$V2     0.090      0.418   0.214   0.830    -0.730      0.909
#> ilr_tbl$V3    -0.536      0.515  -1.040   0.298    -1.546      0.474
#> ilr_tbl$V4     3.231      2.247   1.438   0.151    -1.174      7.635
```

Server-side messages and `glm.fit` convergence warnings from the
single-step IRLS iterations are hidden in this chunk; they are normal
for the DataSHIELD GLM algorithm and stay on the server with a real
connection. The coefficient for `ilr_tbl$V1` is the change in BMI per
unit increase of the first coordinate, i.e. more `vpa` relative to all
other behaviours.

``` r
DSI::datashield.logout(conns)
```

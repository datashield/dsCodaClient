# Compute mean composition via ILR mean and inverse transform

Convenience wrapper that creates ILR coordinates server-side, computes
the mean of each ILR coordinate, and returns the inverse ILR (geometric
mean composition) on the client. Same as \`compositions::mean()\` does.

## Usage

``` r
ds.meanComp(X, datasources = NULL)
```

## Arguments

- X:

  character string specifying the name of the server-side dataset,
  matrix, data.frame, or acomp object containing the compositional data
  (parts in columns).

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If not specified, the default set of
  connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- ilr_objectname:

  optional character string for the intermediate ILR object name on the
  server. If NULL, a temporary name is generated.

- tibble_objectname:

  optional character string for the intermediate tibble name on the
  server. If NULL, a temporary name is generated.

## Value

A named numeric vector containing the estimated mean composition (parts
sum to 1). Part names are taken from the input (when available),
otherwise generic names are used.

## Details

This function automates the typical sequence: 1) compute ILR coordinates
server-side; 2) convert to tibble for column access; 3) compute means of
the ILR coordinates ; and 4) invert the ILR means client-side using
compositions::ilrInv to obtain the geometric mean composition.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{
  # Assuming a server-side object D with compositional parts in columns
  mean_comp <- dsCodaClient::ds.meanComp("D")
  print(mean_comp)
} # }
```

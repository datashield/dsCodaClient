# Build sequential ILR basis matrix (sequential binary partition)

Construct an orthonormal contrast matrix \`V\` for ILR coordinates using
the sequential binary partition (SBP): at step \`j\` compare part \`j\`
against the remaining parts \`j+1..D\`.

## Usage

``` r
build_sequential_ilr_V(D, part_names = NULL)
```

## Arguments

- D:

  An integer giving the number of parts in the composition (\>= 2).

- part_names:

  Optional character vector of part names to use as row names.

## Value

A numeric \`D x (D - 1)\` matrix whose columns form an orthonormal basis
for ILR coordinates under the sequential SBP. Row names are taken from
\`part_names\` when provided; column names are \`ilr1\`, ...,
\`ilr(D-1)\`.

## Details

The resulting \`V\` can be supplied to \`compositions::ilr(x, V = V)\`
to perform the isometric log-ratio (ILR) transformation based on a
sequential binary partition. Column \`j\` contrasts part \`j\` with the
remaining parts \`j+1..D\`, with appropriate normalizing constants to
ensure orthonormality.

## Examples

``` r
# Using number of parts only
V3 <- build_sequential_ilr_V(3)

# Using number of parts with custom names
part_names <- c('vpa', 'mpa', 'lipa', 'sleep', 'sb')
V5 <- build_sequential_ilr_V(5, part_names)

# Use with compositions::ilr
# ilr_vals <- compositions::ilr(X, V = V5)
```

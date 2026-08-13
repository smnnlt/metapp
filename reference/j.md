# Calculating the Hedges correction factor

Calculates the Hedges correction factor for small sample bias

## Usage

``` r
j(x, exact = TRUE)
```

## Arguments

- x:

  numeric, the degrees of freedom.

- exact:

  Whether to use the exact formula or an approximation. Defaults to
  true.

## Value

A numeric, the bias correction factor.

## Examples

``` r
j(20)
#> [1] 0.9619445
```

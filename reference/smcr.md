# Calculating a standardized change score (raw score based standardization)

Calculates the standardized change score with raw score standardization
for a single group.

## Usage

``` r
smcr(xpre, xpost, sdpre, sdpost, n, r, hedges = TRUE, exact = TRUE)
```

## Arguments

- xpre, xpost:

  mean of the pre and post score

- sdpre, sdpost:

  standard deviation of the pre and post score

- n:

  sample size

- r:

  pre-post correlation

- hedges:

  whether an bias correction for small sample sizes according to Hedges
  should be applied (defaults to TRUE).

- exact:

  whether to use the exact formula for the small sample size correction
  (otherwise an approximation is used). This is only relevant when
  `hedges=TRUE`. Defaults to TRUE.

## Details

Uses the average standard deviations of pre and post scores as the
standardizer. Uses the variance estimator by Borenstein & Hedges (2019),
Eq. 11.26.

## Examples

``` r
smcr(10, 12, 2, 3, 20, 0.8)
#>          es        var
#> 1 0.7530156 0.02409888
```

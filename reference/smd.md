# Calculating a standardized mean difference

Calculates a SMD effect size for a simple group comparison.

## Usage

``` r
smd(
  x1,
  x2,
  sd1,
  sd2,
  n1,
  n2,
  hedges = TRUE,
  homo = TRUE,
  vartype = 1,
  exact = TRUE
)
```

## Arguments

- x1, x2:

  means of the first and seconds group.

- sd1, sd2:

  standard deviations of the first and second group.

- n1, n2:

  sample sizes of the first and second group.

- hedges:

  whether an bias correction for small sample sizes according to Hedges
  should be applied (defaults to TRUE).

- homo:

  whether the pooled sd should be used as the standardizer (assuming
  homogeneous population variances). Defaults to TRUE.

- vartype:

  whether to use the variance estimator proposed by Hedges (1, as is
  default in metafor), the variance estimator from Borenstein (2), the
  default in RevMan (3), or the unbiased estimator that is the default
  in meta (4). Defaults to 1.

- exact:

  whether to use the exact formula for the small sample size correction
  (otherwise an approximation is used). This is only relevant when
  `hedges=TRUE`. Defaults to TRUE.

## Value

A data frame of the class mpp with the effect size (es) and its variance
(var).

## Examples

``` r
smd(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#>          es       var
#> 1 -1.375888 0.2493469
```

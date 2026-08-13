# Calculating a standardized change score difference (raw score based standardization)

Calculates a standardized change score group comparison for pre-post
designs with a raw score based standardization (see Morris 2008).

## Usage

``` r
ppc(
  x1d,
  x2d,
  sd1pre,
  sd2pre,
  n1,
  n2,
  r = NA,
  r1 = NA,
  r2 = NA,
  type = 2,
  var_becker = FALSE
)
```

## Arguments

- x1d, x2d:

  mean changes of the first and second group

- sd1pre, sd2pre:

  pre-score standard deviations of the first and second group

- n1, n2:

  sample sizes of the first and second group.

- r1, r2, r:

  pre-post correlation of first and second group (for `type=1`), or for
  both groups (for `type=2`).

- type:

  Type of effect size, see Morris (2008). (1) for the difference of
  standardized mean changes of each group, (2) for a standardization
  using the pooled pre sd. Defaults to 2.

- var_becker:

  For `type=1`, whether the approximate variance estimator by
  Becker (1988) should be used (calculating variances for each group and
  then adding them up). Otherwise uses the variance estimation by Morris
  (2008, Typ ppc1). See Morris 2000 for a comparison. Defaults to FALSE.

## Value

A data frame of the class mpp with the effect size (es) and its variance
(var).

## Examples

``` r
ppc(x1d = 5, x2d = 2, sd1pre = 3, sd2pre = 4, n1 = 20, n2 = 24, r = 0.8)
#>          es        var
#> 1 0.8223769 0.04563379
```

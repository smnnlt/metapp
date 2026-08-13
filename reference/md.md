# Calculating a mean difference

Calculates a raw MD effect size for a simple group comparison.

## Usage

``` r
md(x1, x2, sd1, sd2, n1, n2, var_homo = FALSE)
```

## Arguments

- x1, x2:

  means of the first and seconds group.

- sd1, sd2:

  standard deviations of the first and second group.

- n1, n2:

  sample sizes of the first and second group.

- var_homo:

  whether an variance estimator with pooled standard deviation should be
  used, that assumes homoscedasticity for the population variance
  (defaults to FALSE).

## Value

A data frame of the class mpp with the effect size (es) and its variance
(var).

## Examples

``` r
md(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)
#>   es     var
#> 1 -5 2.59596
```

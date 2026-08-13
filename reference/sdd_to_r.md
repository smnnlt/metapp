# Get pre-post correlation from change score standard deviation

Calculates the pre-post correlation for a group from the pre, post and
change score standard deviation.

## Usage

``` r
sdd_to_r(sd_pre, sd_post, sd_d)
```

## Arguments

- sd_pre, sd_post:

  pre and post standard deviations.

- sd_d:

  standard deviation of change scores

## Value

A numeric, the pre-post correlation.

## Examples

``` r
sdd_to_r(sd_pre = 3, sd_post = 4, sd_d = 2)
#> [1] 0.875
```

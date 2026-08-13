# Get change score standard deviation from pre-post correlation

Calculates the change score standard deviation of a group from the pre
and post standard deviations and the pre-post correlation.

## Usage

``` r
r_to_sdd(sd_pre, sd_post, r)
```

## Arguments

- sd_pre, sd_post:

  pre and post standard deviations.

- r:

  pre-post correlation.

## Value

A numeric, the standard deviation of change scores.

## Examples

``` r
r_to_sdd(sd_pre = 5, sd_post = 7, r = 0.9)
#> [1] 3.316625
```

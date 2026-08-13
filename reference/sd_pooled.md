# Calculate pooled standard deviation

Pools the standard deviation of two groups

## Usage

``` r
sd_pooled(sd1, sd2, n1, n2, mle = FALSE)
```

## Arguments

- sd1, sd2:

  standard deviations of the first and second group.

- n1, n2:

  sample sizes of the first and second group.

- mle:

  Whether the maximum likelihood estimator should be used, which has N
  instead of df (N-2) in the denominator. See for example McGrath &
  Mayer (2006). Defaults to FALSE.

## Value

the pooled standard deviation

## Examples

``` r
sd_pooled(5, 3, 15, 20)
#> [1] 3.973396
```

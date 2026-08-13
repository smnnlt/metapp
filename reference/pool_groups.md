# Pool groups

Pools the mean, standard deviation and sample size for two groups.

## Usage

``` r
pool_groups(x1, x2, sd1, sd2, n1, n2)
```

## Arguments

- x1, x2:

  means of the first and seconds group.

- sd1, sd2:

  standard deviations of the first and second group.

- n1, n2:

  sample sizes of the first and second group.

## Value

a data frame with the group mean (mean), standard deviation (sd) and
sample size (n).

## Examples

``` r
pool_groups(x1 = 10, x2 = 12, sd1 = 4, sd2 = 5, n1 = 10, n2 = 11)
#>          x       sd  n
#> 1 11.04762 4.554955 21
```

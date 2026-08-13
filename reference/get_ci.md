# Get the confidence interval for a given effect size

Calculates the confidence interval for

## Usage

``` r
get_ci(es, level = 0.95, df = NA)
```

## Arguments

- es:

  A data frame of the class mpp with an effect size and its variance.
  For example, the output of
  [`md`](https://smnnlt.github.io/metapp/reference/md.md),
  [`smd`](https://smnnlt.github.io/metapp/reference/smd.md),
  [`smcr`](https://smnnlt.github.io/metapp/reference/smcr.md),
  [`ppc`](https://smnnlt.github.io/metapp/reference/ppc.md).

- level:

  the confidence level. Defaults to 0.95.

- df:

  The degrees of freedom. Defaults to NA, which assumes a normal
  distribution. Otherwise a t-distribution is assumed.

## Value

A data.frame with the effect size (es), its variance (var) and the lower
and upper limits of the confidence interval (ci_low, ci_high).

## Examples

``` r
# calculate a SMD first
g <- smd(x1 = 10, x2 = 15, sd1 = 3, sd2 = 4, n1 = 11, n2 = 9)

# calculate confidence interval
get_ci(g)
#>          es       var   ci_low    ci_high
#> 1 -1.375888 0.2493469 -2.35459 -0.3971872
```

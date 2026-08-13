# Changelog

## metapp (development version)

## metapp 0.0.4 (2026-08-13)

- Zenodo release

## metapp 0.0.3 (2025-08-07)

- Add new unbiased variance estimator for Hedges’ g SMD (`vartype = 4`,
  default for `meta` package).
- Fix bug to avoid overflow in exact correction factor calculation for
  large df
- Fix bug in tests

## metapp 0.0.2 (2025-02-12)

- New [`smcr()`](https://smnnlt.github.io/metapp/reference/smcr.md)
  function for standardized change scores with raw score standardization
  in a single group
- Renamed `smc()` to
  [`ppc()`](https://smnnlt.github.io/metapp/reference/ppc.md)
- New helper functions
  [`sd_pooled()`](https://smnnlt.github.io/metapp/reference/sd_pooled.md)
  and [`sd_avg()`](https://smnnlt.github.io/metapp/reference/sd_avg.md)
  for calculating the pooled or average standard deviation.
- [`md()`](https://smnnlt.github.io/metapp/reference/md.md) now has
  `var_homo = FALSE` as the new default (similar to metafor and RevMan)

## metapp 0.0.1 (2025-01-31)

- First development release

# Add legal status

Add legal status to crab data

## Usage

``` r
add_legal(x, stock = NULL, use_observer = T, use_current = T)
```

## Arguments

- x:

  crab data that includes fields 'size' and 'sex', and 'legal' (if
  use_observer = T) and 'crab_year' (if use_historic = T).

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

- use_observer:

  T/F. Default to observer record when available. x must contain field
  'legal'. Default = T.

- use_current:

  T/F. Use current legal size on the whole time series, for WBT and EBT
  only. Default = T.

## Value

x with additional column 'legal' overwritten so that legal definition is
based on 1) observer records if use_observer = T, 2) size and sex if
observer record is missing or uncertain. 0 = sublegal, 1 = legal.

## Examples

``` r
add_legal(x = crab_dump, stock = "BBRKC")
#> Error: object 'crab_dump' not found
```

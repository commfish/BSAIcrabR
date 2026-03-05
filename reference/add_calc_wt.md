# Add Calculated Weight

Add calculated weight based on weight at length parameters.

## Usage

``` r
add_calc_wt(x, stock, units = "t")
```

## Arguments

- x:

  crab data that includes fields 'size', 'sex', and 'clutch' and
  'maturity' for chionoecetes stocks (if use_observer = T) and
  'crab_year' (if use_historic = T).

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

- units:

  NULL. Unit of measurement, "t" or "lb". Default = "t".

## Value

x with additional column 'calc_wt'

## Examples

``` r
add_calc_wt(data, stock = "BBRKC")
#> Error in add_calc_wt(data, stock = "BBRKC"): Cannot find sex !!
```

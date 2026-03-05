# Get Average Weight

Get average weight of crab by grouping variable using weight at length
parameters

## Usage

``` r
get_avg_wt(data, stock, by, lump = T, units = "t")
```

## Arguments

- data:

  NULL. Crab sample data in the format as the output of
  [`load_crab_dump()`](https://commfish.github.io/BSAIcrabR/reference/load_crab_dump.md)

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

- by:

  NULL. Vector of column names other than crab year, fishery, and size
  to use as a grouping variable.

- lump:

  T/F Lump shell condition into new and old (TRUE) or not (FALSE).
  Default = TRUE.

- units:

  NULL. Unit of measurement, "t" or "lb". Default = "t".

## Value

Data frame including crab year, fishery, grouping variables, and average
weight.

## Examples

``` r
get_avg_wt(data, by = "group")
#> Error in UseMethod("filter"): no applicable method for 'filter' applied to an object of class "function"
```

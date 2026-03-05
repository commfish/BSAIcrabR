# Get Total Catch

Estimate total catch from observer CPUE data

## Usage

``` r
get_total_catch(
  pot_data,
  crab_data,
  ft_data,
  stock,
  by = NULL,
  lump = T,
  units = "t"
)
```

## Arguments

- pot_data:

  NULL. Observer count pot data in the same format as the output of
  [`load_pot_dump()`](https://commfish.github.io/BSAIcrabR/reference/load_pot_dump.md)

- crab_data:

  NULL. Observer measure pot data in the same format as the output of
  [`load_crab_dump()`](https://commfish.github.io/BSAIcrabR/reference/load_crab_dump.md)

- ft_data:

  NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort'
  and whatever variables are specified in by.

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

- by:

  NULL. Vector of column names other than crab year, fishery, and group
  to use as a grouping variable.

- lump:

  T/F. Lump shell condition into new and old (TRUE) or not (FALSE).
  Default = TRUE.

- units:

  NULL. Unit of measurement, "t" or "lb". Default = "t".

## Value

Data frame including crab year, fishery, grouping variables, effort,
average weight, and total catch. Average weights used for expansion to
weight are only based on crab year, fishery, and sex. No other 'by'
variables are included here, so that the sum of total catch by variables
(i.e., sex and shell condition) will equal the total catch by sex.

## Examples

``` r
get_avg_wt(pot_data, crab_data, ft_data, stock = "BBRKC")
#> Error: object 'crab_data' not found
```

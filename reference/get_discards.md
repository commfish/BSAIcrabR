# Get Directed Discards

Estimate directed fishery discards by sex using subtraction method

## Usage

``` r
get_discards(retained_catch, total_catch, stock)
```

## Arguments

- retained_catch:

  NULL. Output of `get_retaied_catch()`.

- total_catch:

  NULL. Output of
  [`get_total_catch()`](https://commfish.github.io/BSAIcrabR/reference/get_total_catch.md).
  'by' argument could be anything as long as group or sex is included.

- stock:

  NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG,
  WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.

## Value

Data frame including crab year, fishery, sex, discards, and discard
mortality.

## Examples

``` r
get_discards(retained_catch, total_catch, stock = "BBRKC")
#> Error: object 'total_catch' not found
```

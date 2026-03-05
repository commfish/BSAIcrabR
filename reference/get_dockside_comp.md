# Get retained size composition

Compute retained size composition from dockside data

## Usage

``` r
get_dockside_comp(data, by = NULL, lump = T)
```

## Arguments

- data:

  NULL. Dockside sample data in the format as the output of
  [`load_dockside()`](https://commfish.github.io/BSAIcrabR/reference/load_dockside.md)

- by:

  NULL. Vector of column names other than crab year, fishery, and size
  to use as a grouping variable.

- lump:

  T/F lump shell condition into new and old (TRUE) or not (FALSE).
  Default = TRUE.

## Value

Size frequency by year, fishery, and size

## Examples

``` r
dockside_comp(data, by = "shell", lump = T)
#> Error in dockside_comp(data, by = "shell", lump = T): could not find function "dockside_comp"
```

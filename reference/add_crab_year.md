# Add crab year to data

Add crab year to observer and dockside data

## Usage

``` r
add_crab_year(x, date_correct = T, date_format = "mdy")
```

## Arguments

- x:

  Observer or dockside data frame with "fishery" code column.

- date_correct:

  Do crab year correction if date is in different crab year than
  regulatory year at the time, default = T.

- date_format:

  Format of date field, default = "mdy".

## Value

x with additional column 'crab_year'

## Examples

``` r
add_crab_year(dockside)
#> Error: object 'dockside' not found
```

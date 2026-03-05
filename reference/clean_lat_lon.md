# Clean Lat and Lon

Clean latitude and longitude data across the dateline

## Usage

``` r
clean_lat_lon(x)
```

## Arguments

- x:

  Pot or observer data with columns longitude, latitude, and eastwest

## Value

x with location information correct across the date line to be negative

## Examples

``` r
clean_lat_lon(pots)
#> Error: object 'pots' not found
```

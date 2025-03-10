#' Clean Lat and Lon
#'
#' Clean latitude and longitude data across the dateline
#' @param x Pot or observer data with columns longitude, latitude, and eastwest
#' @return x with location information correct across the date line to be negative
#' @examples
#' clean_lat_lon(pots)
#' @export
clean_lat_lon <- function(x) {

  x %>%
    mutate(longitude = ifelse(longitude > 0, longitude * -1, longitude),
           longitude = ifelse(longitude == -9, NA, longitude),
           longitude = ifelse(eastwest == "E", (-180 - longitude) + -180, longitude)) -> out

  return(out)

}

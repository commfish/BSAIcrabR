#' Add crab year to data
#'
#' Add crab year to observer and dockside data
#' @param x Observer or dockside data frame with "fishery" code column.
#' @param date_correct Do crab year correction if date is in different crab year than regulatory year at the time, default = T.
#' @param date_format Format of date field, default = "mdy".
#' @return x with additional column 'crab_year'
#' @examples
#' add_crab_year(dockside)
#' @export
add_crab_year <- function(x, date_correct = T, date_format = "mdy") {

  x %>%
    dplyr::mutate(crab_year = ifelse(as.numeric(substring(fishery, 3, 4)) < 60,
                              as.numeric(substring(fishery, 3, 4)) + 2000,
                              as.numeric(substring(fishery, 3, 4)) + 1900)) -> out
  if(date_correct == T){

  if(class(out$sample_date) == "Date") {
    out %>%
      # adjust crab year
      dplyr::mutate(crab_year = case_when(sample_date > mdy(paste0("6/30/", crab_year + 1)) ~ crab_year + 1,
                                          sample_date > mdy(paste0("6/30/", crab_year)) & sample_date <= mdy(paste0("6/30/", crab_year + 1)) ~ crab_year,
                                          sample_date <= mdy(paste0("6/30/", crab_year)) ~ crab_year - 1,
                                          is.na(sample_date) ~ crab_year)) -> out
  }
  if(class(out$sample_date) != "Date") {
    out %>%
      # coerce sample date to date format
      dplyr::mutate(sample_date = as_date(parse_date_time(sample_date, orders = date_format))) %>%
      # adjust crab year
      dplyr::mutate(crab_year = case_when(sample_date > mdy(paste0("6/30/", crab_year + 1)) ~ crab_year + 1,
                                          sample_date > mdy(paste0("6/30/", crab_year)) & sample_date <= mdy(paste0("6/30/", crab_year + 1)) ~ crab_year,
                                          sample_date <= mdy(paste0("6/30/", crab_year)) ~ crab_year - 1,
                                          is.na(sample_date) ~ crab_year)) -> out
  }

  }

  return(out)

}

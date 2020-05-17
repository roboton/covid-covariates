##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param google_mob_fill
##' @return
##' @author roboton
##' @export
aggregate_mob <- function(.data) {
  .data %>%
    spread(value_type, value) %>%
    mutate(
      residential_percent_change_from_baseline =
        residential_percent_change_from_baseline * -1,
      mean_mobility_percent_change_from_baseline = mean(c(
        grocery_and_pharmacy_percent_change_from_baseline,
        parks_percent_change_from_baseline,
        residential_percent_change_from_baseline,
        retail_and_recreation_percent_change_from_baseline,
        transit_stations_percent_change_from_baseline,
        workplaces_percent_change_from_baseline),
        na.rm = TRUE)) %>%
    mutate(residential_percent_change_from_baseline =
             residential_percent_change_from_baseline * -1) %>%
    gather(value_type, value, ends_with("_percent_change_from_baseline")) %>%
    return()
}

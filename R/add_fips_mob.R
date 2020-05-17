##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @param state_fips_2018
##' @param county_fips_2018
##' @return
##' @author roboton
##' @export
add_fips_mob <- function(.data, state_fips_2018, county_fips_2018) {
  .data %>%
    left_join(state_fips_2018, by = c("sub_region_1"="name")) %>%
    left_join(county_fips_2018, by = c("state_fips", "sub_region_2"="name")) %>%
    filter(country_region_code == "US") %>%
    mutate(geo_type = case_when(
      is.na(sub_region_1) & is.na(sub_region_2) ~ "country",
      is.na(sub_region_2) ~ "state",
      TRUE ~ "county")) %>%
    mutate(geo_name = coalesce(sub_region_2, sub_region_1, country_region)) %>%
    rename(state_name = sub_region_1, county_name = sub_region_2) %>%
    return()
}

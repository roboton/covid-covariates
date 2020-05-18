##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @param fips5
##' @return
##' @author roboton
##' @export
add_fips5_mob <- function(.data, state_fips, county_fips) {
  .data %>%
    mutate(geo_name = coalesce(sub_region_2, sub_region_1, country_region)) %>%
    left_join(state_fips %>%
                mutate(country_region_code = "US"),
              by = c("sub_region_1" = "name", "country_region_code")) %>%
    left_join(county_fips %>%
                mutate(name = str_remove(name, " city$") %>%
                         str_remove(" Municipality$") %>%
                         str_remove(" Census Area$") %>%
                         str_remove(" City and Borough$") %>%
                         str_remove(" Borough$") %>%
                         str_replace("LaSalle Parish", "La Salle Parish") %>%
                         str_replace("Kenai Peninsula", "Kenai Peninsula Borough") %>%
                         str_replace("Oglala Lakota County", "Shannon County")),
              by = c("sub_region_2" = "name", "state_fips")) %>%
    mutate(
      fips5 = case_when(
        country_region_code == "US" & is.na(sub_region_1) &
          is.na(sub_region_2) ~ "00000",
        country_region_code == "US" & is.na(sub_region_2) ~
          paste0(state_fips, "000"),
        country_region_code == "US" ~ county_fips),
      geo_type = case_when(
        country_region_code == "US" & is.na(sub_region_1) &
          is.na(sub_region_2) ~ "country",
        country_region_code == "US" & is.na(sub_region_2) ~ "state",
        country_region_code == "US" ~ "county")) %>%
    select(-county_fips, -state_fips) %>%
    return()
}

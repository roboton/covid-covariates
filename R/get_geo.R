##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param level
##' @param country
##' @param subregion1
##' @param subregion2
##' @return
##' @author roboton
##' @export
##' 

get_geo <- function(level, country, subregion1, subregion2,
                    sr_lookup) {
  
  if (all(is.na(level))) {
    # guess level from missing values
    level <- case_when(
      is.na(subregion1) ~ "country",
      is.na(subregion2) ~ "state",
      TRUE ~ "county")
  }
  
  country <- norm_geo_name(country, "country")
  country_code <- countrycode(country, "country.name", "iso2c")
  country_code <- coalesce(country_code, country)
  
  subregion1 <- norm_geo_name(subregion1, "subregion1")
  subregion1_code <- sr_lookup %>%
    filter(level == "state") %>%
    right_join(tibble(country, subregion1),
               by = c("country", "subregion1")) %>%
    mutate(subregion1_code = coalesce(subregion1_code, subregion1)) %>%
    pull(subregion1_code)
    
  subregion2 <- norm_geo_name(subregion2, "subregion2")
  subregion2_code <- sr_lookup %>%
    right_join(tibble(country, subregion1, subregion2),
               by = c("country", "subregion1", "subregion2")) %>%
    mutate(subregion2_code = coalesce(subregion2_code, subregion2)) %>%
    pull(subregion2_code)
  
  geocode <- tibble(country_code, subregion1_code, subregion2_code) %>%
    rowwise() %>%
    mutate(geocode = paste(na.omit(c(country_code, subregion1_code,
                                      subregion2_code)), collapse = "-")) %>%
    pull(geocode)
  return(geocode)
}

norm_geo_name <- function(geo, type) {
  # only handle US right now which is
  # states: abbreviation
  # country: US
  # counties: fips? names?
  case_when(
    type == "subregion2" ~ geo %>%
      str_remove(" city$") %>%
      str_remove(" Municipality$") %>%
      str_remove(" Census Area$") %>%
      str_remove(" City and Borough$") %>%
      str_remove(" Borough$") %>%
      str_replace("LaSalle Parish", "La Salle Parish") %>%
      str_replace("Kenai Peninsula", "Kenai Peninsula Borough") %>%
      str_replace("Oglala Lakota County", "Shannon County"),
    type == "subregion1" ~ geo,
    type == "country" ~ geo,
    TRUE ~ geo
    ) %>% return()
}

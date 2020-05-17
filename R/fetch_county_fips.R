##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param url
##' @param skip
##' @return
##' @author roboton
##' @export
fetch_county_fips <- function(
  url = "https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx",
  skip = 4) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_xlsx(tf, skip = skip) %>%
    filter(`Place Code (FIPS)` == "00000" &
             `County Subdivision Code (FIPS)` == "00000" &
             `Consolidtated City Code (FIPS)` == "00000") %>%
    mutate(county_fips = paste0(`State Code (FIPS)`,
                                `County Code (FIPS)`)) %>%
    select(name=`Area Name (including legal/statistical area description)`,
           state_fips = `State Code (FIPS)`, county_fips) %>%
    mutate(name = str_remove(name, " city$") %>%
             str_remove(" Municipality$") %>%
             str_remove(" Census Area$") %>%
             str_remove(" City and Borough$") %>%
             str_remove(" Borough$") %>%
             str_replace("LaSalle Parish", "La Salle Parish") %>%
             str_replace("Kenai Peninsula", "Kenai Peninsula Borough") %>%
             str_replace("Oglala Lakota County", "Shannon County")) %>%
    filter(state_fips != "00" & !str_ends(county_fips, "000")) %>% # United States
    return()
}

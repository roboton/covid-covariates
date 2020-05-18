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
    filter(`Summary Level` == "050") %>%
    mutate(county_fips = paste0(`State Code (FIPS)`,
                                `County Code (FIPS)`)) %>%
    select(name=`Area Name (including legal/statistical area description)`,
           state_fips = `State Code (FIPS)`, county_fips) %>%
    return()
}

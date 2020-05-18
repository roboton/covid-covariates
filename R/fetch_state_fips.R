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
fetch_state_fips <- function(
  url = "https://www2.census.gov/programs-surveys/popest/geographies/2018/state-geocodes-v2018.xlsx",
  skip = 5) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_xlsx(tf, skip = skip) %>%
    select(name=`Name`, state_fips=`State (FIPS)`) %>%
    filter(state_fips != "00") %>%
    return()
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param url
##' @param col_types
##' @return
##' @author roboton
##' @export
fetch_csv_zip <- function(url, col_types = NULL) {
  GET(url, write_disk(tf <- tempfile(fileext = ".zip")))
  unzip(tf, exdir = td <- tempdir())
  list.files(td, "*.csv") %>%
    map_dfr(~ read_csv(file.path(td, .x),
                       col_types = col_types)) %>%
    bind_rows()
}

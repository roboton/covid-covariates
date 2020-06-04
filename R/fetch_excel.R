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
fetch_excel <- function(url, skip) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_xlsx(tf, skip = skip)
}

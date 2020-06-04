##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @return
##' @author roboton
##' @export
clean_colnames <- function(.data) {
  .data %>%
    rename_all(
      ~ .x %>%
        str_to_lower() %>%
        str_remove("_percent_change_from_baseline") %>%
        str_remove(" raw value") %>%
        str_replace_all("%", "pct")
     ) %>%
    tibble(.name_repair = "universal") %>%
    rename_all(
      ~ .x %>%
       str_replace_all("[\\._-]+", "_")
    )
}

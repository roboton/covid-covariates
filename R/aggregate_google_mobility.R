##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @return
##' @author roboton
##' @export
aggregate_google_mobility <- function(.data) {
  .data %>%
    mutate(value = if_else(str_starts(value_type, "residential"),
                           value * -1, value)) %>%
    group_by_at(vars(-value_type, -value)) %>%
    summarise_at(vars(value), mean, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(value_type = "mobility_index_percent_change_from_baseline") %>%
    bind_rows(.data) %>% 
    return()
}

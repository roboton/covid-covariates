##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @return
##' @author roboton
##' @export
impute_missing_mob <- function(.data) {
  .data %>%
    pivot_wider(names_from=date, values_from=value) %>%
    pivot_longer(starts_with("2020"), names_to = "date",
                 values_to = "value") %>%
    group_by_at(vars(-date, -value)) %>%
    arrange(date) %>%
    filter(sum(!is.na(value)) >= 2) %>%
    mutate(value = na.fill(value, "extend")) %>%
    return()
}

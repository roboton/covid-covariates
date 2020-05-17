##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @return
##' @author roboton
##' @export
impute_missing_chr <- function(.data, missing_thresh = 0.05) {
  .data %>%
    # select columns with less than `missing_thresh`% missing values
    select_if(~ mean(is.na(.x)) < missing_thresh) %>%
    # replace missing values by state averages
    group_by(`State FIPS Code`) %>%
    mutate_at(vars(ends_with(" raw value")),
              ~ if_else(is.na(.x), mean(.x), .x)) %>%
    ungroup() %>% return()
}

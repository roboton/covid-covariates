##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param .data
##' @param model
##' @return
##' @author roboton
##' @export
compute_cpt <- function(.data, model = "meancpt") {
  .data %>% nest() %>% rowwise() %>%
    mutate(cpt_mdl = list(envcpt(data$value, models = model,
                                 verbose = FALSE))) %>%
    mutate(cpt_params = list(extract_cpt_params(data, cpt_mdl))) %>%
    unnest(cpt_params) %>% ungroup() %>%
    return()
}

extract_cpt_params <- function(cpt_data, cpt_mdl, mdl_name = "meancpt") {
  if (all(is.na(cpt_mdl))) {
    return(NA)
  }
  cpt_mdl <- cpt_mdl[[mdl_name]]
  dates <- ymd(cpt_data$date)
  values <- cpt_data$value
  
  means <- cpt_mdl@param.est$mean
  vars <- cpt_mdl@param.est$variance
  
  end_pts <- cpt_mdl@cpts
  seg_lens <- seg.len(cpt_mdl)
  start_pts <- (end_pts - seg_lens) + 1
  
  eq_idx <- head(order(seg_lens, decreasing = T), 2)
  min_idx <- which.min(means)
  max_idx <- which.max(means)
  first_idx <- min(min_idx, max_idx)
  last_idx <- max(min_idx, max_idx)
  
  # important vals
  change_start <- end_pts[first_idx]
  change_end <- start_pts[last_idx]
  
  # save vals
  change_start_date <- dates[change_start]
  change_end_date <- dates[change_end]
  
  mean_before <- means[first_idx]
  mean_after <- means[last_idx]
  var_before <- vars[first_idx]
  var_after <- vars[last_idx]
  change_diff <- mean_after - mean_before
  change_slope <- change_diff / (last_idx - first_idx)
  start_seg <- c(start_pts[first_idx], end_pts[first_idx])
  start_seg_dates <- dates[start_seg]
  end_seg <- c(start_pts[last_idx], end_pts[last_idx])
  end_seg_dates <- dates[end_seg]
  revert <- last(means) - mean_after
  
  data.frame(
    change_start_date = change_start_date,
    change_diff = change_diff,
    change_slope = change_slope,
    change_end_date = change_end_date,
    change_days = change_end_date - change_start_date,
    mean_before = mean_before,
    mean_after= mean_after,
    var_before = var_before,
    var_after= var_after,
    revert = revert)
}

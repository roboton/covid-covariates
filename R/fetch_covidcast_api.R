##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author roboton
##' @export
fetch_covidcast_api <- function() {
  metadata <- fromJSON(
    "https://delphi.cmu.edu/epidata/api.php?source=covidcast_meta") 
  
  fetch_args <- metadata$epidata %>%  rowwise() %>%
    mutate(time_values = list(seq(ymd(min_time), ymd(max_time), by = "day"))) %>%
    unnest(time_values) %>%
    mutate(time_values = format(time_values, "%Y%m%d"), geo_value = "*",
           time_type = "day", source="covidcast") %>%
    select(source, data_source, signal, time_type, geo_type, time_values,
           geo_value)

  # execute fetch
  covcast_daily <- fetch_args %>%
    # only fetch daily data
    # filter(time_type %in% time_types) %>%
    mutate(res = pmap(., fetch_delphi)) %>%
    select(-geo_value, -time_values) %>%
    unnest(res) %>%
    mutate(date = ymd(time_value)) %>%
    select(-res, -time_value)
  return(covcast_daily)
}

# dependencies
source(paste0(
  "https://raw.githubusercontent.com",
  "/cmu-delphi/delphi-epidata/main/src/client/delphi_epidata.R"))

# function to process a single api call
fetch_delphi <- function(
  source, data_source, signal, time_type, geo_type, time_values, geo_value) {
  res <- Epidata[["covidcast"]](data_source, signal, time_type, geo_type,
                                list(time_values), geo_value)
  # result had an error
  if(res$result != 1) {
    warning(str_glue("Error fetching {data_source}, {signal}, {time_values}: ",
                     "{res$message}"))
    return(NA)
  }
  tmp <- res$epidata %>%
    # There may be a better way to parse the list of lists
    toJSON(auto_unbox = TRUE) %>% fromJSON()
  # handle case where missing values returned as a data.frame with a NULL
  if(class(tmp$stderr) == "data.frame" ||
     class(tmp$sample_size) == "data.frame" ||
     class(tmp$direction) == "data.frame") {
    tmp$stderr <- NA_real_
    tmp$sample_size <- NA_real_
    tmp$direction <- NA_integer_
    return(tmp)
  }
  return(tmp %>%
    # handle case where missing values returned as an empty list       
    mutate(
      direction = map_int(direction, function(x) {
        if (is.null(unlist(x))) return(NA_integer_) else return(unlist(x)) }),
      stderr = map_dbl(stderr, function(x) {
        if (is.null(unlist(x)) || class(unlist(x)) == "data.frame") 
          return(NA_real_) else return(unlist(x)) }),
      sample_size = map_dbl(sample_size, function(x) {
        if (is.null(unlist(x)) || class(unlist(x)) == "data.frame") 
          return(NA_real_) else return(unlist(x)) })))
}

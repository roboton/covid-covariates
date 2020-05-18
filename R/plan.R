covid_covariates_plan <- drake::drake_plan(
 
  # Google mobility
  google_mob_tidy = read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
    col_types = "ccccDnnnnnn", na = c("")) %>%
    gather(value_type, value, ends_with("_percent_change_from_baseline")),
  google_mob_fill = impute_missing_mob(google_mob_tidy),
  google_mob_cpt = compute_cpt(google_mob_fill),
  google_mob_csv = write_csv(select(google_mob_cpt, -cpt_mdl, -data),
                             file_out("data/google_mob_cpt.csv")),
  google_mob_rds = write_rds(select(google_mob_cpt, -cpt_mdl, -data),
                             file_out("data/google_mob_cpt.rds")),
  
  # County health rankings (US)
  county_health_raw = read_csv(
    "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"),
  county_health_clean = county_health_raw %>%
    slice(-1) %>%
    mutate_at(vars(-1:-5), as.numeric) %>%
    rename_all(~ str_replace(.x, "^\\%", "Pct Pop.")) %>%
    select(1:5, ends_with(" raw value")),
  county_health_fill = impute_missing_chr(county_health_clean),
  county_health_csv = write_csv(county_health_fill,
                                file_out("data/county_health.csv")),
  county_health_rds = write_rds(county_health_fill,
                                file_out("data/county_health.rds")),
  
  # COVIDcast Delphi (US)
  covidcast_daily = fetch_covidcast(),
  covidcast_csv = write_csv(
    covidcast_daily, file_out("data/covidcast_daily.csv")),
  covidcast_rds = write_rds(
    covidcast_daily, file_out("data/covidcast_daily.rds")),
  
  # County Join
  # Google mobility (US)
  ## US State/County data
  county_fips = fetch_county_fips(),
  state_fips = fetch_state_fips(),
  google_mob_fips = add_fips5_mob(google_mob_cpt, state_fips, county_fips),
  google_mob_county = google_mob_fips %>%
    filter(geo_type == "county") %>%
    select(fips5, value_type, starts_with("change_"), starts_with("mean_"),
           starts_with("var_"), starts_with("seg_"), revert) %>%
    pivot_wider(names_from = value_type,
                values_from=c(-fips5, -value_type)),
 
  county_health_join = county_health_fill %>%
    select(-`State FIPS Code`, -`County FIPS Code`, -`State Abbreviation`,
           -Name) %>%
    rename(fips5 = `5-digit FIPS Code`),
    
 covidcast_county = covidcast_daily %>%
    filter(geo_type == "county" & !is.na(geo_value)) %>%
    select(data_source, signal, geo_value, value, date),
  covidcast_county_mean = covidcast_county %>%
    unite(metric, data_source, signal) %>%
    group_by(fips5 = geo_value, metric) %>%
    summarise_at(vars(value), mean, na.rm = TRUE) %>%
    ungroup() %>% spread(metric, value),
  
  # Joined
  county_joined_all = county_fips %>%
    rename(fips5 = county_fips) %>%
    left_join(state_fips, by = "state_fips",
              suffix = c("_county", "_state")) %>%
    left_join(google_mob_county,
              by = "fips5", suffix = c(".census", ".mob")) %>%
    left_join(county_health_join,
              by = "fips5", suffix = c("", ".chr")) %>%
    left_join(covidcast_county_mean,
              by = "fips5", suffix = c("", ".covcast")),
  county_joined_csv = write_csv(county_joined_all,
                                file_out("data/county_joined.csv")),
  county_joined_rds = write_rds(county_joined_all,
                                file_out("data/county_joined.rds")),

  # RMarkdown
  cov_delphi = target(
    command = {
      rmarkdown::render(knitr_in("doc/cov-delphi.Rmd"))
      file_out("doc/cov-delphi.nb.html")
    }
  ),
  cov_mobility = target(
    command = {
      rmarkdown::render(knitr_in("doc/cov-mobility.Rmd"))
      file_out("doc/cov-mobility.nb.html")
    }
  ),
  cov_prediction = target(
    command = {
      rmarkdown::render(knitr_in("doc/cov-prediction.Rmd"))
      file_out("doc/cov-prediction.nb.html")
    }
  )
)

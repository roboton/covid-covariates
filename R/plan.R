covid_covariates_plan <- drake::drake_plan(
  
  # US State/County data
  state_fips_2018 = fetch_state_fips(),
  county_fips_2018 = fetch_county_fips(),
  
  # Google mobility
  google_mob_tidy = read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
    col_types = "ccccDnnnnnn") %>%
    gather(value_type, value, ends_with("_percent_change_from_baseline")),
  google_mob_fips = add_fips_mob(google_mob_tidy, state_fips_2018,
                                 county_fips_2018),
  google_mob_agg = aggregate_mob(google_mob_fips),
  google_mob_fill = impute_missing_mob(google_mob_agg),
  google_mob_cpt = compute_cpt(google_mob_fill),
  google_mob_csv = write_csv(select(google_mob_cpt, -cpt_mdl, -data),
                             file_out("data/google_mob_cpt.csv")),
  google_mob_county = google_mob_cpt %>%
    filter(geo_type == "county") %>%
    select(county_fips, value_type, starts_with("change_"),
           starts_with("mean_"), starts_with("var_"), starts_with("seg_"),
           revert) %>%
    pivot_wider(names_from = value_type,
                values_from=c(-county_fips, -value_type)) %>%
    filter(!is.na(county_fips)),
  
  # County health rankings
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
  
  # COVIDcast Delphi
  covidcast_daily = fetch_covidcast(),
  covidcast_csv = write_csv(
    covidcast_daily, file_out("data/covidcast_daily.csv")),
  covidcast_county = covidcast_daily %>%
    filter(geo_type == "county") %>%
    select(data_source, signal, geo_value, value, date),
  
  covidcast_county_mean = covidcast_county %>%
    unite(metric, data_source, signal) %>%
    group_by(county_fips = geo_value, metric) %>%
    summarise_at(vars(value), mean, na.rm = TRUE) %>%
    ungroup() %>%
    spread(metric, value) %>% filter(!is.na(county_fips)),
  
  # Joined
  county_joined_all = county_fips_2018 %>%
    left_join(google_mob_county,
              by = "county_fips",
              suffix = c(".census", ".mob")) %>%
    left_join(county_health_fill %>%
                select(-`State FIPS Code`, -`County FIPS Code`,
                       -Name, -`State Abbreviation`),
              by = c("county_fips" = "5-digit FIPS Code"),
              suffix = c("", ".chr")) %>%
    left_join(covidcast_county_mean,
              by = "county_fips",
              suffix = c("", ".covcast")),
  county_joined_csv = write_csv(county_joined_all,
                                file_out("data/county_joined.csv")),

  # R-Markdown
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

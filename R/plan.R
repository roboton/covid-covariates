# fetch
# save
# clean + tidy + standardize
# save
# join
# save
# impute
# save


# Geo columns: country,subregion1,subregion2,geocode
# Value columns: value_type, value
# clean values
# tidy format
# standardize geo
# select columns

# fetch/clean > tidy > select/rename > standardize geo > join

covid_covariates_plan <- drake::drake_plan(
  # look up data

  us_state_fips = fetch_excel(
    "https://www2.census.gov/programs-surveys/popest/geographies/2018/state-geocodes-v2018.xlsx", 5) %>%
    filter(`State (FIPS)` != "00") %>%
    select(state_name = `Name`, state_fips = `State (FIPS)`) %>%
    left_join(tibble(state_abb = state.abb,
                     state_name = state.name), by = "state_name"),
  
  us_county_fips = fetch_excel(
    "https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx", 4) %>%
    filter(`Summary Level` == "050") %>%
    mutate(county_fips = paste0(`State Code (FIPS)`,
                                `County Code (FIPS)`)) %>%
    select(county_name =
             `Area Name (including legal/statistical area description)`,
           state_fips = `State Code (FIPS)`, county_fips) %>%
    left_join(us_state_fips, by = "state_fips"),
  
  sr_lookup = bind_rows(
    us_state_fips %>%
      mutate(country = "United States", country_code = "US",
             subregion1 = norm_geo_name(state_name, "state"),
             level = "state", subregion2 = NA, subregion2_code = NA) %>%
      select(
        level, country, country_code,
        subregion1, subregion1_code = state_abb,
        subregion2, subregion2_code),
    us_county_fips %>%
      mutate(country = "United States", country_code = "US",
             subregion1 = norm_geo_name(state_name, "state"),
             subregion2 = norm_geo_name(county_name, "county"),
             level = "county") %>%
      select(
        level, country, country_code,
        subregion1, subregion1_code = state_abb,
        subregion2, subregion2_code = county_fips)
    ),

  # google mobility
  google_mobility_raw = read_csv(
    "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
    col_types = "ccccDnnnnnn", na = c("")),
  google_mobility_tidy = google_mobility_raw %>%
    pivot_longer(names_to = "value_type", values_to = "value",
                 ends_with("_percent_change_from_baseline")),
  google_mobility_names = google_mobility_tidy %>%
    rename(country_code = country_region_code, country = country_region,
           subregion1 = sub_region_1, subregion2 = sub_region_2),
  google_mobility_geo = google_mobility_names %>%
    mutate(geocode = get_geo(level = NA, country, subregion1, subregion2,
                             sr_lookup)),
  google_mobility_impute = google_mobility_geo %>%
    group_by(geocode, value_type) %>%
    filter(sum(!is.na(value)) > 2) %>%
    arrange(date) %>%
    mutate(value = na.fill(value, "extend")),
  google_mobility_wide = google_mobility_impute %>%
    spread(value_type, value) %>% clean_colnames(),
  google_mobility_csv = write_csv(
    google_mobility_wide, file_out("data/google_mobility.csv")),
  google_mobility_agg = aggregate_google_mobility(google_mobility_impute),
  google_mobility_cpt = compute_mobility_cpt(google_mobility_agg),
  google_mobility_cpt_wide = google_mobility_cpt %>%
    select(-cpt_mdl, -data) %>%
    pivot_wider(names_from = value_type,
                values_from = change_start_date:revert) %>%
    clean_colnames(),
  google_mobility_cpt_csv = write_csv(
    google_mobility_cpt_wide, file_out("data/google_mobility_cpt.csv")),
  
  # county health rankings
  chr_covariates_raw = read_csv(
    "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv") %>%
    # second row repeated column names
    slice(-1) %>%
    # convert numeric columns to numeric
    mutate_at(vars(-1:-5), as.numeric),
  chr_covariates_tidy = chr_covariates_raw %>%
    select(subregion2_code = `5-digit FIPS Code`,
           subregion1_code = `State Abbreviation`, ends_with(" raw value")) %>%
    pivot_longer(names_to = "value_type", values_to = "value",
                 ends_with(" raw value")),
  chr_covariates_geo = chr_covariates_tidy %>%
    mutate(geocode = paste("US", subregion1_code, subregion2_code,
                           sep = "-")) %>%
    filter(subregion1_code != "US"),
  chr_covariates_wide = chr_covariates_geo %>% spread(value_type, value) %>%
    clean_colnames(),
  chr_covariates_csv = write_csv(chr_covariates_wide,
                                 file_out("data/chr_covariates.csv")),
  
  # mit elections
  mit_elections_raw = read_csv(
    "https://dataverse.harvard.edu/api/access/datafile/3641280?format=original&gbrecs=true"),
  mit_elections_tidy = mit_elections_raw %>% filter(FIPS == 2001) %>%
    select(-totalvotes, -candidate, -version, -office) %>%
    mutate(party = replace_na(party, "other")) %>%
    group_by_at(vars(-candidatevotes)) %>%
    summarise(candidatevotes = sum(candidatevotes)) %>%
    pivot_longer(names_to = "value_type", values_to = "value",
                 candidatevotes) %>%
    unite(value_type, party, value_type),
  mit_elections_geo = mit_elections_tidy %>%
    mutate(geocode = paste("US", state_po, str_pad(FIPS, 5, "left", "0"),
                           sep = "-")),
  mit_elections_wide = mit_elections_geo %>% spread(value_type, value) %>%
    clean_colnames(),
  mit_elections_csv = write_csv(mit_elections_wide,
                                file_out("data/mit_elections.csv")),
  
  # corona data scraper
  cds_covid_raw = fetch_csv_zip(
    "https://coronadatascraper.com/timeseries-tidy.csv.zip",
    col_types = "ccccccnnnccDcn"),
  cds_covid_tidy = cds_covid_raw %>% rename(value_type = type),
  cds_covid_geo = cds_covid_tidy %>%
    mutate(geocode = get_geo(level, country, state, county, sr_lookup)),
  cds_covid_wide = cds_covid_geo %>% spread(value_type, value) %>%
    clean_colnames(),
  cds_covid_csv = write_csv(cds_covid_wide,
                            file_out("data/cds_covid.csv")),
  cds_covid_wide_county = cds_covid_wide %>%
    filter(level == "county" & !is.na(population)),
    
  # cmu covidcast
  cmu_covidcast_raw = fetch_covidcast_api(),
  cmu_covidcast_tidy = cmu_covidcast_raw %>%
    unite(signal, data_source, signal) %>%
    select(-source, -time_type) %>%
    gather(value_type, value, value, direction, stderr, sample_size) %>%
    unite(value_type, signal, value_type),
  cmu_covidcast_geo = cmu_covidcast_tidy %>%
    left_join(us_county_fips, by = c("geo_value" = "county_fips")) %>%
    mutate(geocode = case_when(
      geo_type == "county" ~ paste("US", state_abb, geo_value, sep = "-"),
      geo_type == "state" ~ paste("US", state_abb, sep = "-"),
      TRUE ~ NA_character_
    )) %>%
    mutate(
      county_name = case_when(
        geo_value == "70002" ~ "Dukes and Nantucket Counties",
        geo_value == "70003" ~ "Kansas City",
        TRUE ~ county_name),
      state_name = case_when(
        geo_value == "70002" ~ "Massachusetts",
        geo_value == "70003" ~ "Missouri",
        TRUE ~ state_name),
      state_abb = case_when(
        geo_value == "70002" ~ "MA",
        geo_value == "70003" ~ "MO",
        TRUE ~ state_abb)) %>%
    filter(!is.na(county_name)) %>%
    select(-state_fips), 
  cmu_covidcast_wide = cmu_covidcast_geo %>% spread(value_type, value) %>%
    clean_colnames(),
  cmu_covidcast_csv = write_csv(cmu_covidcast_wide,
                                file_out("data/cmu_covidcast.csv")),
  
  # joined sets
  covidcast_chr = cmu_covidcast_wide %>% filter(geo_type == "county") %>%
    left_join(chr_covariates_wide, by = c("geocode"),
              suffix = c("", "_chr")),
  covidcast_chr_csv = write_csv(covidcast_chr,
                                file_out("data/covidcast_chr.csv")),
  
  county_joined = cmu_covidcast_wide %>% filter(geo_type == "county") %>%
    left_join(chr_covariates_wide, by = c("geocode"),
              suffix = c("", "_chr")) %>%
    left_join(google_mobility_cpt_wide, by = c("geocode"),
              suffix = c("", "_gmob")) %>%
    left_join(mit_elections_wide %>% filter(year == max(year)),
              by = c("geocode"),
              suffix = c("", "_elec")) %>%
    left_join(cds_covid_wide_county, by = c("date", "geocode"),
                                            suffix = c("", "_cds")),
  county_joined_csv = write_csv(county_joined,
                                file_out("data/county_joined.csv")),
  county_means = county_joined %>% select(-date) %>%
    mutate_if(~ is.Date(.x) | is.difftime(.x), as.numeric) %>%
    group_by(geocode) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    ungroup() %>%
    select_if(~ mean(is.na(.x) | is.nan(.x)) != 1) %>%
    mutate_at(vars(starts_with("jhu_csse_")), ~ if_else(is.na(.x), 0, .x)),
  
  # NBER set
  
  nber_dat = covidcast_chr %>%
    #mutate_if(~ is.difftime(.x) | is.Date(.x), as.numeric) %>%
    filter(date <= max(date[!is.na(google_survey_raw_cli_value)]) + weeks(4)) %>%
    select_if(~ mean(is.na(.x)) < 1) %>%
    select(-date, -subregion1_code, -subregion2_code, -geo_type, -state_name) %>%
    # drop zero'd NYC counties
    filter(!geo_value %in% c("36005", "36047", "36081", "36085")) %>%
    unite(county_name, county_name, state_abb, sep = ", ") %>%
    group_by_at(vars(geo_value:geocode, starts_with("subregion"))) %>%
    #summarise_all(~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE))) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    #filter(!is.na(google_survey_raw_cli_value)) %>%
    mutate(google_survey_raw_cli_value = google_survey_raw_cli_value * 0.01 *
             google_survey_raw_cli_sample_size),
  nber_dat_csv = write_csv(nber_dat, file_out("data/nber_dat.csv")),
  
    # covidcast_chr_county = covidcast_county_clean(covidcast_daily),
  # covidcast_chr_county_means = covidcast_chr_county %>% 
  #   group_by(fips, chr_name) %>%
  #   summarise_at(vars(-date), mean, na.rm = TRUE) %>%
  #   ungroup() %>%
  #   left_join(county_party, by = c("fips" = "fips5")) %>%
  #    mutate(value_google_survey_raw_cli =
  #             value_google_survey_raw_cli * 0.01 *
  #             sample_size_google_survey_raw_cli),
  
  # # for ML
  # train_test = initial_split(
  #   county_means %>% column_to_rownames("geocode") %>%
  #     #filter_at(vars(starts_with("google_survey")), ~ !is.na(.x)) %>%
  #     rename_all(~ str_remove(.x, "_value$"))),
  # recipe = train_test %>% training() %>%
  #   recipe(
  #     # JHU
  #     jhu_csse_confirmed_7day_avg_cumulative_nu +
  #       jhu_csse_confirmed_7day_avg_cumulative_pr +
  #       jhu_csse_confirmed_7day_avg_incidence_num +
  #       jhu_csse_confirmed_7day_avg_incidence_pro +
  #       jhu_csse_confirmed_cumulative_num +
  #       jhu_csse_confirmed_cumulative_prop +
  #       jhu_csse_confirmed_incidence_num + # ~ .) %>% 
  #       jhu_csse_confirmed_incidence_prop +
  #       jhu_csse_deaths_7day_avg_cumulative_num +
  #       jhu_csse_deaths_7day_avg_cumulative_prop +
  #       jhu_csse_deaths_7day_avg_incidence_num +
  #       jhu_csse_deaths_7day_avg_incidence_prop +
  #       jhu_csse_deaths_cumulative_num +
  #       jhu_csse_deaths_cumulative_prop +
  #       jhu_csse_deaths_incidence_num +
  #       jhu_csse_deaths_incidence_prop +
  #       # CDS
  #       active + cases + deaths + growthfactor + hospitalized + recovered +
  #       tested ~ .) %>%
  #   step_knnimpute(all_predictors()) %>% prep(),
  # test_set = recipe %>% bake(train_test %>% testing()) %>%
  #   filter(complete.cases(.)),
  # train_set = juice(recipe) %>% filter(complete.cases(.)),
  
  # # RMarkdown
  # cov_delphi = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-delphi.Rmd"))
  #     file_out("doc/cov-delphi.nb.html")
  #   }
  # ),
  # cov_mobility = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-mobility.Rmd"))
  #     file_out("doc/cov-mobility.nb.html")
  #   }
  # ),
  # cov_prediction_data = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-prediction-data.Rmd"))
  #     file_out("doc/cov-prediction-data.nb.html")
  #   }
  # ),
  # cov_prediction_methods = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-prediction-methods.Rmd"))
  #     file_out("doc/cov-prediction-methods.nb.html")
  #   }
  # ) ,
  # cov_prediction_model = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-prediction-model.Rmd"))
  #     file_out("doc/cov-prediction-model.nb.html")
  #   }
  # ),
  # cov_prediction_ts = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/cov-prediction-timeseries.Rmd"))
  #     file_out("doc/cov-prediction-timeseries.nb.html")
  #   }
  # ),
  cov_nber = target(
    command = {
      rmarkdown::render(knitr_in("doc/nber-ai.Rmd"))
      file_out("doc/nber-ai.pdf")
    }
  )
)
  # Old 
  # # COVIDcast Daily (US)
  # covidcast_daily = fetch_covidcast(),
  # # covidcast_daily = read_rds("data/covidcast_daily.rds"),
  # covidcast_csv = write_csv(
  #   covidcast_daily, file_out("data/covidcast_daily.csv")),
  # covidcast_rds = write_rds(
  #   covidcast_daily, file_out("data/covidcast_daily.rds")),
  # 
  # # 2016 County election results (US)
  # county_party = read_csv(mit_elections_url) %>%
  #   filter(year == 2016) %>%
  #   mutate(fips5 = str_pad(FIPS, 5, side = "left", "0")) %>%
  #   group_by(fips5, chr_party = party) %>%
  #   summarise_at(vars(candidatevotes), sum) %>%
  #   filter(candidatevotes == max(candidatevotes)),
  # 
  # 
  # # COVIDcast Daily (US)
  # covidcast_daily = fetch_covidcast(),
  # # covidcast_daily = read_rds("data/covidcast_daily.rds"),
  # covidcast_csv = write_csv(
  #   covidcast_daily, file_out("data/covidcast_daily.csv")),
  # covidcast_rds = write_rds(
  #   covidcast_daily, file_out("data/covidcast_daily.rds")),
  # 
  # # 2016 County election results (US)
  # county_party = read_csv(mit_elections_url) %>%
  #   filter(year == 2016) %>%
  #   mutate(fips5 = str_pad(FIPS, 5, side = "left", "0")) %>%
  #   group_by(fips5, chr_party = party) %>%
  #   summarise_at(vars(candidatevotes), sum) %>%
  #   filter(candidatevotes == max(candidatevotes)),
  # 
  # ## County-level data sets
  # 
  # # Google mobility (US)
  # county_fips = fetch_county_fips(),
  # state_fips = fetch_state_fips(),
  # google_mob_fips = add_fips5_mob(google_mob_cpt, state_fips, county_fips),
  # google_mob_county = google_mob_fips %>%
  #   filter(geo_type == "county") %>%
  #   select(fips5, value_type, starts_with("change_"), starts_with("mean_"),
  #          starts_with("var_"), starts_with("seg_"), revert) %>%
  #   pivot_wider(names_from = value_type,
  #               values_from = c(-fips5, -value_type)) %>%
  #   rename_at(vars(-fips5), ~ paste0("mob_", .x)),
  # 
  # county_health_join = county_health_fill %>%
  #   select(-`State FIPS Code`, -`County FIPS Code`, -`State Abbreviation`,
  #          -Name) %>%
  #   rename(fips5 = `5-digit FIPS Code`) %>%
  #   rename_at(vars(-fips5), ~ paste0("chr_", .x)),
  # 
  # # County means
  # covidcast_county = covidcast_daily %>%
  #   filter(geo_type == "county" & !is.na(geo_value)) %>%
  #   select(data_source, signal, geo_value, value, date),
  # covidcast_county_mean = covidcast_county %>%
  #   unite(metric, data_source, signal) %>%
  #   group_by(fips5 = geo_value, metric) %>%
  #   summarise_at(vars(value), mean, na.rm = TRUE) %>%
  #   ungroup() %>% spread(metric, value) %>%
  #   rename_at(vars(-fips5, -starts_with("jhu-csse")),
  #                  ~ paste0("cmu_", .x)),
  # 
  # # Joined
  # county_joined_all = county_fips %>%
  #   rename(fips5 = county_fips) %>%
  #   left_join(state_fips, by = "state_fips",
  #             suffix = c("_county", "_state")) %>%
  #   left_join(google_mob_county,
  #             by = "fips5", suffix = c(".census", ".mob")) %>%
  #   left_join(county_health_join,
  #             by = "fips5", suffix = c("", ".chr")) %>%
  #   left_join(covidcast_county_mean,
  #             by = "fips5", suffix = c("", ".covcast")),
  # county_joined_csv = write_csv(county_joined_all,
  #                               file_out("data/county_joined.csv")),
  # county_joined_rds = write_rds(county_joined_all,
  #                               file_out("data/county_joined.rds")),
  # 
  # # NBER AI
  # covidcast_chr_county = covidcast_county_clean(covidcast_daily),
  # covidcast_chr_county_means = covidcast_chr_county %>% 
  #   group_by(fips, chr_name) %>%
  #   summarise_at(vars(-date), mean, na.rm = TRUE) %>%
  #   ungroup() %>%
  #   left_join(county_party, by = c("fips" = "fips5")) %>%
  #    mutate(value_google_survey_raw_cli =
  #             value_google_survey_raw_cli * 0.01 *
  #             sample_size_google_survey_raw_cli),
  # 
  # # Prep train/test data
  # county_joined_sparse = make_sparse(county_joined_all),
  # prep_data = train_test_data(county_joined_sparse),
  # prep_data_full = train_test_data(county_joined_all),
  # recipe = create_impute_recipe(prep_data),
  # recipe_full = create_impute_recipe(prep_data_full),
  # test_set = recipe %>% bake(prep_data %>% testing()),
  # train_set = juice(recipe),
  # test_set_full = recipe_full %>% bake(prep_data_full %>% testing()),
  # train_set_full = juice(recipe_full),
  # 

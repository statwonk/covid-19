library(tidyverse)

######################################################################
# COVID-19 Deaths Model
# Author: @statwonk
# Note: Please feel free to copy, criticize, or improve.
# Description: a statistical model to begin investigating the relationships
#   between support for Trump and county-level COVID-19 deaths.
######################################################################

# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
readr::read_csv("Archive/covid_deaths_usafacts.csv", col_types = cols(
  .default = col_double(),
  `County Name` = col_character(),
  State = col_character()
)) %>%
  janitor::clean_names() %>%
  pivot_longer(cols = -c(county_fips, county_name, state, state_fips)) %>%
  filter(!grepl("Statewide", county_name)) %>%
  mutate(date = gsub("x", "", name),
         date = gsub("_", "/", date),
         date = as.POSIXct(date, format = "%m/%d/%y")) %>%
  group_by(state, county_name, county_fips) %>%
  arrange(date) %>%
  mutate(period = 1:n()) %>%
  ungroup() %>%
  rename(deaths = value) %>%
  right_join(
    read_csv("Archive/countypres_2000-2016.csv") %>%
      filter(year == 2016, grepl("Trump", candidate)) %>%
      select(county_fips = FIPS, candidatevotes, totalvotes),
    by = "county_fips") -> d

readr::read_csv("Archive/cc-est2018-alldata.csv", col_types = cols(
  year = col_double(),
  state = col_character(),
  state_po = col_character(),
  county = col_character(),
  FIPS = col_double(),
  office = col_character(),
  candidate = col_character(),
  party = col_character(),
  candidatevotes = col_integer(),
  totalvotes = col_integer(),
  version = col_double()
)) %>%
  # https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2018/cc-est2018-alldata.pdf
  filter(AGEGRP == 0) %>% # all ages
  mutate(county_fips = paste0(as.integer(STATE), COUNTY),
         county_fips = as.integer(county_fips)) %>%
  janitor::clean_names() %>%
  filter(year == 11) %>%
  # latest year
  select(county_fips, tot_pop, tot_male,
         tot_female, tot_male,
         ba_male, ba_female,
         wa_male, wa_female) %>%
  right_join(d) %>%
  group_by(county_fips) %>%
  # deaths are reported cumulatively in the source data
  mutate(deaths = deaths - dplyr::lag(deaths)) %>%
  ungroup() -> d

readr::read_csv("Archive/County_Zhvi_AllHomes.csv") %>%
  janitor::clean_names() %>%
  select(region_id, region_name, state_name, contains("fips"), median_home_value = x2019_01_31) %>%
  mutate(county_fips = paste0(as.integer(state_code_fips), municipal_code_fips),
         county_fips = as.integer(county_fips)) %>%
  right_join(d) -> d

readr::read_csv("Archive/co-est2019-alldata.csv") %>%
  janitor::clean_names() %>%
  mutate(county_fips = paste0(as.integer(state), county),
         county_fips = as.integer(county_fips)) %>%
  select(county_fips, popestimate2019) %>%
  right_join(d) -> d

readr::read_csv("Archive/USA_Population_Density.csv", col_types = cols(
  OBJECTID = col_double(),
  ID = col_character(),
  NAME = col_character(),
  ST_ABBREV = col_character(),
  LANDAREA = col_double(),
  TOTPOP_CY = col_double(),
  POPDENS_CY = col_double()
)) %>%
  mutate(county_fips = as.integer(ID)) %>%
  janitor::clean_names() %>%
  select(-objectid) %>%
  select(county_fips, landarea) %>%
  right_join(d) %>%
  mutate(vote_share = candidatevotes/totalvotes) %>%
  mutate(pop_density = popestimate2019/landarea) -> d

d %>%
  distinct(date) %>%
  arrange(date) %>%
  mutate(overall_period = 1:n()) %>%
  right_join(d) %>%
  filter(!is.na(deaths)) -> d

d %>%
  select(deaths,
         state, county_fips,
         overall_period, vote_share, pop_density,
         median_home_value,
         tot_pop, tot_male,
         ba_male, ba_female,
         wa_male, wa_female
  ) %>%
  group_by(state, county_fips) %>%
  arrange(overall_period) %>%
  group_by(state, county_fips, overall_period) %>%
  summarise(deaths = sum(deaths),
            median_home_value = unique(median_home_value),
            tot_pop = unique(tot_pop),
            tot_male = unique(tot_male),
            ba_male = unique(ba_male),
            ba_female = unique(ba_female),
            wa_male = unique(wa_male),
            wa_female = unique(wa_female),
            vote_share = unique(vote_share),
            pop_density = unique(pop_density)) %>%
  ungroup() %>%
  mutate(county_fips = factor(county_fips)) -> d

d %>%
  distinct(county_fips, .keep_all = TRUE) %>%
  select(county_fips, vote_share, pop_density) %>%
  right_join(d %>% select(-vote_share, -pop_density)) %>%
  mutate(vote_share = scale(vote_share)[, 1],
         pop_density = scale(pop_density)[, 1],
         population = scale(tot_pop)[, 1],
         pct_male = scale(tot_male/tot_pop)[, 1],
         pct_black = scale((ba_female + ba_male)/tot_pop)[, 1],
         pct_white = scale((wa_female + wa_male)/tot_pop)[, 1],
         median_home_value = scale(median_home_value)[, 1]) -> d

lm(
  deaths ~ vote_share +
    pop_density +
    median_home_value +
    factor(state) +
    factor(overall_period) +
    population +
    pct_male +
    pct_white +
    pct_black,
  data = d
) -> fit

broom::tidy(fit) -> params
params %>% filter(!grepl("factor", term))

# plot(fit)

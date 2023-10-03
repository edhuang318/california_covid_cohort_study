library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(tidyverse)
library(sf)           # For reading, writing and working with spatial objects

setwd("~/Documents/covid-19-time-series-metrics-by-county-and-state-twzxyf")
#use this dataset to map out California
covid_cases_deaths_tests <- read.csv("statewide-covid-19-cases-deaths-tests.csv")

total_case_counts_by_county <- covid_cases_deaths_tests %>%
  group_by(area) %>%
  summarize(total_cases = max(cumulative_cases)) 
total_death_counts_by_county <- covid_cases_deaths_tests %>%
  group_by(area) %>%
  summarize(total_deaths = max(cumulative_deaths))
total_case_counts_by_county
total_death_counts_by_county

#County Populations to Determine Case/Death Density
#website: https://www.california-demographics.com/counties_by_population
#copied and pasted data into an Excel sheet, then saved it as a .csv file
california_county_population <- read_csv("California County Population 2021 Estimate.csv")
california_county_population <- california_county_population %>%
  mutate(County = str_remove_all(County, " County")) %>%
  rename(area = "County")
total_case_counts_by_county <- total_case_counts_by_county %>%
  left_join(california_county_population)
total_death_counts_by_county <- total_death_counts_by_county %>%
  left_join(california_county_population)
total_case_counts_by_county <- total_case_counts_by_county %>%
  mutate(case_density = total_cases*100000/Population)
total_death_counts_by_county <- total_death_counts_by_county %>%
  mutate(death_density = total_deaths*100000/Population)
#Now make the map
census_variables <- load_variables(2021, "acs5", cache = TRUE)
View(census_variables)
unique(census_variables$concept)

#census_api_key("1fd361849952fe6d87edfdc07962d8805c8e0903", install = TRUE)
#readRenviron("~/.Renviron")
ca_county_population <- get_acs(
  geography = "county",
  state = "CA",
  variables = "B01003_001",
  geometry = TRUE)
ca_county_population <- ca_county_population %>%
  mutate(area = str_remove_all(NAME, " County, California"))
ca_county_population <- ca_county_population %>%
  left_join(total_case_counts_by_county)
ca_county_population <- ca_county_population %>%
  left_join(total_death_counts_by_county)

ca_county_population %>%
  ggplot(aes(fill = estimate)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Total Individuals") +
  ggtitle("Population of California, 2020 United States Census")
ca_county_population %>%
  ggplot(aes(fill = total_cases)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Total Cases") +
  ggtitle("COVID-19 Caseload of California")
ca_county_population %>%
  ggplot(aes(fill = case_density)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Cases per 100,000 Individuals") +
  ggtitle("COVID-19 Prevalence in California")
ca_county_population %>%
  ggplot(aes(fill = total_deaths)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Total Individuals") +
  ggtitle("COVID-19 Mortality in California")
ca_county_population %>%
  ggplot(aes(fill = death_density)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "Deaths per 100,000 Individuals") +
  ggtitle("COVID-19 Mortality Prevalence in California")

# harris %>%
#   mutate(percent = 100 * (value / summary_value)) %>%
#   ggplot(aes(fill = percent)) +
#   facet_wrap(~variable) +
#   geom_sf(color = NA) +
#   theme_void() + 
#   scale_fill_viridis_c() + 
#   labs(fill = "% of population\n(2020 Census)")
# ggplot(ny, aes(fill = estimate)) + 
#   geom_sf() + 
#   theme_void() + 
#   scale_fill_viridis_c(labels = scales::dollar)
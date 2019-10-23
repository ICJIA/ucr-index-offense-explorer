library(dplyr)


# generate app_map
app_map <- rmapshaper::ms_simplify(icjiar::counties, keep = 0.005)
app_map@data <-
  app_map@data %>%
  mutate(
    rural = case_when(
      rural_urban_2010 == 1 ~ "Completely Rural",
      rural_urban_2010 == 2 ~ "Mostly Rural",
      rural_urban_2010 == 3 ~ "Mostly Urban",
      TRUE ~ "Completely Urban"
    )
  )

# generate app_data
app_data <- icjiar::crimes_isp %>%
  left_join(select(app_map@data, county = name, region, rural)) %>%
  left_join(select(icjiar::populations, -fips)) %>%
  select(
    year,
    county,
    region,
    rural,
    population,
    violent_crime:arson
  )

# save data for app
save(app_data, app_map, file = "data.rda")

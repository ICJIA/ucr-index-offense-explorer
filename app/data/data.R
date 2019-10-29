library(dplyr)

# generate app_data
app_data <-
  read.csv("ucr.csv") %>%
  select(
    year,
    county = county_name,
    region,
    rural = community_type,
    population,
    murder,
    sexual_assault,
    robbery,
    aggravated_assault,
    burglary,
    larceny_theft,
    motor_vehicle_theft,
    arson
  ) %>%
  mutate(
    region = case_when(
      region == "Northern - Cook" ~ "Cook",
      region %in% c("Northern - Collar", "Northern - Other") ~ "Northern",
      TRUE ~ as.character(region)
    ),
    person_crime = murder + sexual_assault + robbery + aggravated_assault,
    property_crime = burglary + larceny_theft + motor_vehicle_theft + arson
  )

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

# save data for app
save(app_data, app_map, file = "data.rda")

library(dplyr)


# generate mymap
mymap <- rmapshaper::ms_simplify(icjiar::counties, keep = 0.005)
mymap@data <-
  mymap@data %>%
  mutate(
    rural = case_when(
      rural_urban_2010 == 1 ~ "Completely Rural",
      rural_urban_2010 == 2 ~ "Mostly Rural",
      rural_urban_2010 == 3 ~ "Mostly Urban",
      TRUE ~ "Completely Urban"
    )
  )

# generate mydata
mydata <- icjiar::crimes_isp %>%
  left_join(select(mymap@data, county = name, region, rural)) %>%
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
save(mydata, mymap, file = "data.rda")

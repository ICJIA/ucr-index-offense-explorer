library(icjiar)
library(dplyr)


# generate mymap
mymap <- counties
mymap@data <-
  mymap@data %>%
  mutate(
    rural = ifelse(
      rural_urban_2010 == 1,
      "Completely Rural",
      ifelse(
        rural_urban_2010 == 2,
        "Mostly Rural",
        ifelse(
          rural_urban_2010 == 3,
          "Mostly Urban",
          "Completely Urban"
        )
      )
    )
  )

# generate mydata
mydata <- crimes_isp %>%
  left_join(select(mymap@data, county = name, region, rural)) %>%
  left_join(select(populations, -fips)) %>%
  select(
    year,
    county,
    region,
    rural,
    population,
    violent_crime:arson
  ) %>%
  mutate(county = as.factor(county))

# save data for app
save(mydata, mymap, file = "data.rda")

library(icjiar)
library(dplyr)


# generate mydata
mydata <- crimes_isp %>%
  left_join(select(counties@data, county = name, region)) %>%
  left_join(select(populations, -fips)) %>%
  select(
    year,
    county,
    region,
    population,
    violent_crime:arson
  ) %>%
  mutate(county = as.factor(county))


# generate mymap
mymap <- counties


# save data for app
save(mydata, mymap, file = "data.rda")

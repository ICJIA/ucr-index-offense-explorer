library(icjiar)
library(dplyr)


# generate mydata
mydata <- crimes_isp %>%
  left_join(select(counties@data, county = name, region, type)) %>%
  left_join(populations) %>%
  select(
    year,
    county,
    region,
    type,
    population,
    violent_crime:arson
  )


# generate mymap
mymap <- counties


# save data for app
save(mydata, mymap, file = "data.rda")

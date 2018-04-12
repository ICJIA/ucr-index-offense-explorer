 # PREPARE FOR THE SESSION #
#---------------------------------------------------------------------------
# set up the working directory
# path_wd <- 'O:/Bobae_workspace/project_ShinyApp_ISP'
# setwd(path_wd)


# import packages
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
# library(icjiar)


# IMPORT DATA #
#---------------------------------------------------------------------------
# import population data
pop <- read_excel('2015CensusPlacesPops.xlsx', sheet='2015')


# import ISP data
files_isp <- list.files(path='data', pattern='^ISP.*')
read_isp <- function(file_isp, Y2015=FALSE){
  path <- paste('data', file_isp, sep='/')
  output <- read_excel(path) %>%
    data.table()
  
  if(Y2015==FALSE){
    output <- output %>%
      select(-c(POPchg, RindxChg, Indxrtchg, MurdChg, RapeChg,
                AssltChg, RobChg, BurgChg, LarcChg, ArsonChg))
  }
  
  output <- output[is.na(Agency),] %>%
    select(-c(Agency, AgencyType, Multicnty, Verified))
  
  return(output)
}

isp_2012_11 <- read_isp(files_isp[1])
isp_2013_12 <- read_isp(files_isp[2])
isp_2014_13 <- read_isp(files_isp[3])
isp_2015_14 <- read_isp(files_isp[4], Y2015=TRUE)


# get yearly ISP data and combine all ISP data
isp_2011 <- isp_2012_11[, .(year       = 2011,
                            county     = County,
                            murder     = as.numeric(Murder11),
                            rape       = as.numeric(Rape11),
                            robbery    = as.numeric(Rob11),
                            aggAssult  = as.numeric(Asslt11),
                            burglary   = as.numeric(Burg11),
                            larcenyTft = as.numeric(Larc11),
                            MVTft      = as.numeric(Auto11),
                            arson      = as.numeric(Arson11))]
isp_2012 <- isp_2012_11[, .(year       = 2012,
                            county     = County,
                            murder     = as.numeric(Murder12),
                            rape       = as.numeric(Rape12),
                            robbery    = as.numeric(Rob12),
                            aggAssult  = as.numeric(Asslt12),
                            burglary   = as.numeric(Burg12),
                            larcenyTft = as.numeric(Larc12),
                            MVTft      = as.numeric(Auto12),
                            arson      = as.numeric(Arson12))]
isp_2013 <- isp_2013_12[, .(year       = 2013,
                            county     = County,
                            murder     = as.numeric(Murder13),
                            rape       = as.numeric(Rape13),
                            robbery    = as.numeric(Rob13),
                            aggAssult  = as.numeric(Asslt13),
                            burglary   = as.numeric(Burg13),
                            larcenyTft = as.numeric(Larc13),
                            MVTft      = as.numeric(Auto13),
                            arson      = as.numeric(Arson13))]
isp_2014 <- isp_2014_13[, .(year       = 2014,
                            county     = County,
                            murder     = as.numeric(Murder14),
                            rape       = as.numeric(Rape14),
                            robbery    = as.numeric(Rob14),
                            aggAssult  = as.numeric(Asslt14),
                            burglary   = as.numeric(Burg14),
                            larcenyTft = as.numeric(Larc14),
                            MVTft      = as.numeric(Auto14),
                            arson      = as.numeric(Arson14))]
isp_2015 <- isp_2015_14[, .(year       = 2015,
                            county     = County,
                            murder     = as.numeric(CH15),
                            rape       = as.numeric(Rape15),
                            robbery    = as.numeric(Rob15),
                            aggAssult  = as.numeric(AggBA15),
                            burglary   = as.numeric(Burg15),
                            larcenyTft = as.numeric(Theft15),
                            MVTft      = as.numeric(MVT15),
                            arson      = as.numeric(Arson15))]


# TRANSFORM DATA #
#---------------------------------------------------------------------------
# combine isp data
isp <- plyr::rbind.fill(isp_2011, isp_2012, isp_2013, isp_2014, isp_2015) %>%
  mutate(violentCrime  = murder + rape + robbery + aggAssult,
         propertyCrime = burglary + larcenyTft + MVTft + arson) %>%
  select(1:2, violentCrime, 3:6, propertyCrime, 7:10)


# transform population data
pop %<>%
  filter(grepl(pattern='^(?!.*Balance of).*County$', NAME, perl=TRUE)) %>%
  separate(NAME, into=c('county', 'REMOVE'), sep=' County') %>%
  select(county,
         `2011` = POPESTIMATE2011,
         `2012` = POPESTIMATE2012,
         `2013` = POPESTIMATE2013,
         `2014` = POPESTIMATE2014,
         `2015` = POPESTIMATE2015) %>%
  gather(`2011`:`2015`, key='year', value='population') %>%
  mutate(year = as.integer(year))


# change some county names to match
isp[isp$county=='De Kalb',]$county <- 'DeKalb'
isp[isp$county=='Du Page',]$county <- 'DuPage'
isp[isp$county=='La Salle',]$county <- 'LaSalle'


# combine isp and population data
output <- isp %>%
  mutate(region = hash_counties_to_regions[[county]],
         countyType = hash_counties_to_types[[county]]) %>%
  select(year, county, region, countyType, 3:12) %>%
  left_join(pop, by=c('year', 'county'))


# WRAP UP THE SESSION #
#---------------------------------------------------------------------------
# save output
fwrite(output, 'app/data.csv')


# clear workspace
rm(list = ls())

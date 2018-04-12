# get URLs
url_ISP_2015_14 <- 'http://www.isp.state.il.us/docs/cii/cii15/ds/CrimeData_15_14.xlsx'
url_ISP_2014_13 <- 'http://www.isp.state.il.us/docs/cii/cii14/ds/IndexCrimeDrugArrestData_14_13.xlsx'
url_ISP_2013_12 <- 'http://www.isp.state.il.us/docs/cii/cii13/ds/IndexCrimeOffenses_Data_13_12.xlsx'
url_ISP_2012_11 <- 'http://www.isp.state.il.us/docs/cii/cii12/ds/Internet_Crime_12_11.xlsx'
urls_ISP <- c(url_ISP_2015_14, url_ISP_2014_13, url_ISP_2013_12, url_ISP_2012_11)

destfiles_ISP <- c('data/ISP_2015_14.xlsx', 'data/ISP_2014_13.xlsx',
                   'data/ISP_2013_12.xlsx', 'data/ISP_2012_11.xlsx')


# download data if not already existing
if(!length(list.files('data'))){
  
  for(i in 1:length(urls_ISP)){
    download.file(urls_ISP[i], destfiles_ISP[i], mode='wb', cacheOK=FALSE) 
  }
  
}


# clear workspace
rm(list = ls())
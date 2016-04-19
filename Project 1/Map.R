###########################
## Using choroplethr zip ##
###########################

# install.packages("devtools")
library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrZip)
library(dplyr)
library(zipcode)
#data(zip.regions)

avgbyzip = latest %>% 
  group_by(., zipcode) %>%
  summarise(., mean(score))
colnames(avgbyzip) = c('region','value')
avgbyzip$region = clean.zipcodes(avgbyzip$region)
scored_zips = unique(avgbyzip$region)
missing_zips = c('10048','10055', '10057', '10104', '10105', '10106', '10107', '10118', '10121', '10123', '10155', '10166', '10175', '10176', '10178', '10179', '10281', '10285', '10317', '11242', '11249', '11256', '11352')
adjusted_zips = scored_zips[!(scored_zips %in% missing_zips)]

zip_choropleth(avgbyzip, 
               msa_zoom = "New York-Newark-Jersey City, NY-NJ-PA",
               zip_zoom = adjusted_zips,
               title="NYC latest restaurant inspection score averages",
               legend="Average scores")


latest_short = latest[latest$zipcode %in% missing_zips,]
dim(latest_short)

#ggmap
# library(ggmap)

######MIssing zip codes: ] 10065 10075 10057 10317

# zips = read.csv('zipcode.csv', header=T, stringsAsFactors = F) %>%
#   rename(., zipcode=zip)
# latest2 = left_join(latest[latest$boro!= 'Missing',], zips, by='zipcode') %>%
#   select(., longitude, latitude, score)
# latest_neg = latest2[latest2$score<=0,]
# latest_pos = latest2[latest2$score>0,]
# latest3 = latest_pos[rep(row.names(latest_pos), latest_pos$score), ]
# latest_combined = rbind(latest_neg,latest_pos)
# latest_combined = latest_combined[,c('longitude','latitude')]
# 
# # 
# # qmplot(longitude, latitude, data = latest2, source='google')
# 
# m <- get_map("New York City", zoom = 12, source="stamen")
# 
# # ggmap(m, base_layer = ggplot(aes(x = longitude, y = latitude, size = score), data = latest2))  + 
# #   geom_point(color="red",alpha=0.2) + 
# #   scale_fill_gradient(low = "yellow", high = "red")
# #   #stat_density2d(aes(x = latest2$longitude, y = latest2$latitude, fill=..level..), data=latest2,geom="polygon", alpha=0.2)
# 
# 
# ggmap(m) + 
#   #geom_point(aes(x=latest2$longitude, y=latest2$latitude), colour = 'red', alpha = 0.1, data = latest2)
#   geom_density2d(data = latest_combined, aes(x = longitude, y = latitude), size = 0.3) +
#   stat_density2d(data = latest_combined, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),  geom = "polygon") + 
#   scale_fill_gradient(low = "green", high = "red") + 
#   scale_alpha(range = c(0, 0.3), guide = FALSE)



###########################
## Using choroplethr zip ##
###########################

# install.packages("devtools")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrZip)
library(dplyr)
library(zipcode)
data(zip.regions)

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

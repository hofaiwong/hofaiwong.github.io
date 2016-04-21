#Project 1 - By cuisine
library(plyr)
library(dplyr)
library(ggplot2)
###########################
###  Scores by cuisine  ###
###########################

####Current state stats by cuisine
restByCuisine = restaurants %>%
  group_by(., cuisine) %>%
  summarise(., count=n()) %>%
  arrange(., desc(count))

restByCuisineFiltered = restaurants %>%
  group_by(., cuisine) %>%
  summarise(., count=n()) %>%
  arrange(., desc(count)) %>%
  top_n(., 10, count)

sum(restByCuisineFiltered$count) / sum(restByCuisine$count) #coverage of restaurants after filtering

restaurantsFiltered = semi_join(restaurants, restByCuisineFiltered, by = 'cuisine') #Will keep top 10 covering 64%
inspectionsFiltered = semi_join(inspections, restByCuisineFiltered, by = 'cuisine')

####Visualize current state by cuisine

#Stacked bar chart of restaurants by cuisine and grade
ggplot(data=restaurantsFiltered, aes(x=reorder(cuisine, cuisine, function(x) length(x)))) + 
  geom_bar(aes(fill=new_grade)) + 
  coord_flip() +
  labs(title='Restaurants by cuisine (top 30) and grade',
       x='Cuisine (Top 30)',
       y='Restaurants') +
  scale_fill_discrete(name='Grade')

# #Boxplot of restaurants by cuisine
# ggplot(data=restaurantsFiltered, aes(x=reorder(cuisine, -score), y=score)) + 
#   geom_boxplot() + 
#   coord_flip()
#   labs(title='Latest scores by borough', x='Borough', y='Latest scores') + 
#   stat_summary(fun.y=mean, colour="white", geom="point", shape=16, size=2)

# ggplot(data=restaurantsFiltered, aes(x=cuisine)) + 
#   geom_bar(aes(fill=new_grade), position = 'fill') + 
#   coord_flip()

# ggplot(data=restaurantsFiltered, aes(x=score)) + 
#   geom_histogram(binwidth = 2, aes(fill = cuisine))

ggplot(data=restaurantsFiltered, aes(x = score)) + 
  geom_freqpoly(aes(color=cuisine), binwidth=2) + 
  coord_cartesian(xlim=c(0,40)) + 
  labs(title='Restaurants by score and cuisine', x='Score', y='Restaurants') + 
  scale_colour_discrete(name="Cuisine")

ggplot(data=restaurantsFiltered, aes (x=score)) + 
  geom_density(aes(color=cuisine)) + 
  coord_cartesian(xlim=c(0,40)) +
  labs(title='Density of restaurants by score and cuisine', x='Score', y='Density') + 
  scale_colour_discrete(name="Cuisine")


#############################
#### Closings by cuisine ####
#############################

# ###Summarise closings, reclosings and inspections by borough and month/year
# inspectionsByMonthByCuisine = inspections %>%
#   group_by(., cuisine, yearmon) %>%
#   summarise(., inspections=n())
# 
# closingsByMonthByCuisine = inspections %>% 
#   filter(., action %in% c('closed','reclosed')) %>%
#   group_by(., cuisine, yearmon) %>%
#   summarise(., closings=n())
# 
# reclosingsByMonthByCuisine = inspections %>% 
#   filter(., action=='reclosed') %>%
#   group_by(., cuisine, yearmon) %>%
#   summarise(., reclosings=n())

###Ratio of inspections that lead to closings by cuisine
inspectionClosingSummaryByCuisine = inspectionsFiltered %>%
  group_by(., cuisine) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            reclosings = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections
            )

inspectionClosingRatioByCuisine.gg = ggplot(data=inspectionClosingSummaryByCuisine, aes(x=reorder(cuisine, ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Inspection closing ratio by cuisine', x='Cuisine', y='Inspection closing ratio') +
  coord_flip()
inspectionClosingRatioByCuisine.gg #Overall inspection closing ratio by borough


############ CONTINUE HERE



###Ratio of unique restaurant that closed by borough
restaurantClosingSummary = merge(
      summarize(group_by(unique(inspections[inspections$action %in% c('closed','reclosed'),c('camis','boro')]), boro), unique_camis_closed = n()),
      summarize(group_by(unique(inspections[,c('camis','boro')]), boro), total_camis = n()),
      by = 'boro')
restaurantClosingSummary$ratio = restaurantClosingSummary$unique_camis_closed / restaurantClosingSummary$total_camis
restaurantClosingSummary #Closings by count of camis and % of repeat offenders

restaurantClosingRatio.gg = ggplot(data=restaurantClosingSummary, aes(x=reorder(boro, -ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Restaurant closing ratio by borough', x='Borough', y='Restaurant closing ratio')
restaurantClosingRatio.gg #Overall restaurant closing ratio by borough


############################
#### Reclosings by boro ####
############################

#Create list of all unique restaurants that were closed at least once (we are considering closings, not reclosings as part of the same inspection cycle)
closedRestaurants =  inspections[inspections$action == 'closed',c('camis','boro','zipcode')] %>%
  group_by(., camis, boro, zipcode) %>%
  summarize(., count = n())

#Summarize counts by borough of restaurants that were closed, closed once only, closed more than once
closedByBoro = closedRestaurants %>%
  group_by(., boro) %>%
  summarize(., count = n())
closedMoreThanOnceByBoro = closedRestaurants[closedRestaurants$count > 1,] %>%
  group_by(., boro) %>%
  summarize(., count = n())

#Combine into one table by borough
reclosingsByBoro = cbind(closedByBoro, closedMoreThanOnceByBoro[,2])  
colnames(reclosingsByBoro) = c('boro','total_closed','closed_more_than_once')
reclosingsByBoro$ratioClosedMoreThanOnce = reclosingsByBoro$closed_more_than_once / reclosingsByBoro$total_closed
reclosingsByBoro

#Ratio of restaurants by borough that were closed more than once out of restaurants that were closed at least once
ggplot (data = reclosingsByBoro, aes (x = reorder(boro, -ratioClosedMoreThanOnce), y = ratioClosedMoreThanOnce)) + 
  geom_bar(stat='identity') +
  labs(title='Ratio of repeat closures by Borough', x='Borough', y='Ratio of repeat closings')



############################
#### Re/closings by zip ####
############################

#Summarize counts by zip of restaurants that were closed, closed once only, closed more than once
closedByZip = closedRestaurants %>%
  group_by(., zipcode) %>%
  summarize(., count = n())
colnames(closedByZip) = c('region','value')

closedMoreThanOnceByZip = closedRestaurants[closedRestaurants$count > 1,] %>%
  group_by(., zipcode) %>%
  summarize(., count = n())
colnames(closedMoreThanOnceByZip) = c('region','value')

#Map of closures by zipcode
zip_choropleth(closedByZip, 
               zip_zoom = adjusted_zips,
               title="Closed restaurants by zipcode",
               legend="Restaurants",
               num_colors = 5)

zip_choropleth(closedMoreThanOnceByZip, 
               zip_zoom = adjusted_zips,
               title="Repeatedly closed restaurants by zipcode",
               legend="Restaurants",
               num_colors = 5)
#----------> Fix legend and colour
















###########################
#### Trends by borough ####
###########################

####Need trend of closing ratio over time

###Graphs for closings and inspections over time by borough
# ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) +
#   geom_freqpoly(aes(color = boro), binwidth = 90) + #confirm bin for dates
#   labs(title='Count of closings over time by borough', x='Inspection date', y='Count of closings')
#   scale_colour_discrete(name="Borough")

ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  labs(title='Density of closings over time by borough', x='Inspection date', y='Density of closings')
  scale_colour_discrete(name="Borough")

# ggplot (data = inspections, aes (x = inspection.date)) + 
#   geom_freqpoly(aes(color = boro), binwidth=30) + 
#   labs(title = 'Count of inspections over time by borough', x='Inspection date', y='Count of inspections') +
#   scale_colour_discrete(name="Borough")

ggplot (data = inspections, aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  labs(title = 'Density of inspections over time by borough', x='Inspection date', y='Density of inspections') +
  scale_colour_discrete(name="Borough")

# inspectionSummaryByMonth = merge(cntInspections, cntClosings, by = c('boro','yearmon'), all=T)
# inspectionSummaryByMonth = merge(inspectionSummaryByMonth, cntReclosings, by = c('boro','yearmon'), all=T)
# inspectionSummaryByMonth = merge(inspectionSummaryByMonth, cntReopenings, by = c('boro','yearmon'), all=T)
# inspectionSummaryByMonth$closingRatio = inspectionSummaryByMonth$closings / inspectionSummaryByMonth$inspections
# 
# closingRatio.gg = ggplot(data=inspectionSummaryByMonth[!is.na(inspectionSummaryByMonth$closingRatio),], aes(x=yearmon)) +
#   geom_line(aes(y=closingRatio, color = boro)) +
#   xlab("Inspection Date") + ylab("Closing ratio") +
#   scale_colour_discrete(name="Borough") +
#   ggtitle("Closing ratio by month by borough")
# closingRatio.gg
# # ####################### => Too hard to read





#Trend of score by borough


#By Time
ggplot (data = latest, aes(x = inspection.date, y = score)) + geom_point()

#By aggregate stat
ggplot(data = mean_score_by_boro, aes(x=boro, y = latest_mean_score)) + geom_point()

#Current graphs by cuisine
ggplot(data=latest, aes(x=reorder(latest$cuisine.description,latest$score), y=latest$score)) + coord_flip() + geom_bar(stat='identity')
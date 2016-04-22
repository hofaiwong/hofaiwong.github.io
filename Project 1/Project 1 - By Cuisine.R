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
  top_n(., 20, count)

sum(restByCuisineFiltered$count) / sum(restByCuisine$count) #coverage of restaurants after filtering

restaurantsFiltered = tbl_df(semi_join(restaurants, restByCuisineFiltered, by = 'cuisine')) #Will keep top xx covering yy%
inspectionsFiltered = tbl_df(semi_join(inspections, restByCuisineFiltered, by = 'cuisine'))

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

# ggplot(data=restaurantsFiltered, aes(x = score)) + 
#   geom_freqpoly(aes(color=cuisine), binwidth=2) + 
#   coord_cartesian(xlim=c(0,40)) + 
#   labs(title='Restaurants by score and cuisine', x='Score', y='Restaurants') + 
#   scale_colour_discrete(name="Cuisine")

ggplot(data=restaurantsFiltered, aes (x=score)) + 
  geom_density(aes(color=cuisine)) + 
  coord_cartesian(xlim=c(0,40)) +
  labs(title='Density of restaurants by score and cuisine', x='Score', y='Density') + 
  scale_colour_discrete(name="Cuisine")
#----------> Change to line instead of box in legend

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

inspectionClosingRatioByCuisine.gg = 
  ggplot(data=inspectionClosingSummaryByCuisine, aes(x=reorder(cuisine, ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Inspection closing ratio by cuisine', x='Cuisine', y='Inspection closing ratio') +
  coord_flip()
inspectionClosingRatioByCuisine.gg #Overall inspection closing ratio by cuisine


###Ratio of inspections that lead to closings by cuisine - with boro
inspectionClosingSummaryByCuisineBoro = inspectionsFiltered %>%
  group_by(., cuisine, boro) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            reclosings = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections
            )

#Refactor cuisines to keep order in plot as previous plot
inspectionClosingSummaryByCuisine$cuisine = factor(inspectionClosingSummaryByCuisine$cuisine)
newFactor = levels(inspectionClosingSummaryByCuisine$cuisine)[order(-rank(inspectionClosingSummaryByCuisine$ratio))]
inspectionClosingSummaryByCuisineBoro$cuisine = factor(inspectionClosingSummaryByCuisineBoro$cuisine, levels=rev(newFactor))

inspectionClosingRatioByCuisineBoro.gg = ggplot(data=inspectionClosingSummaryByCuisineBoro, aes(x=cuisine, y=ratio)) + 
  geom_bar(stat='identity', aes(fill=boro)) + 
  facet_grid(~ boro) +
  labs(title='Inspection closing ratio by cuisine and borough', x='Cuisine', y='Inspection closing ratio') +
  coord_flip()
#---->move legend to bottom
inspectionClosingRatioByCuisineBoro.gg #Overall inspection closing ratio by cuisine and borough





###Ratio of unique restaurant that closed by cuisine
restaurantClosingSummaryByCuisine = inspectionsFiltered %>%
  group_by(., camis, cuisine) %>%
  summarise(., n = sum(action %in% c('closed','reclosed'))) %>%
  group_by(., cuisine) %>%
  summarise(., unique_camis_closed = sum(n>0), total_camis = n(), ratio = unique_camis_closed / total_camis)
restaurantClosingSummaryByCuisine #Closings by count of camis and % of repeat offenders

restaurantClosingRatioByCuisine.gg = ggplot(data=restaurantClosingSummaryByCuisine, aes(x=reorder(cuisine, ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Restaurant closing ratio by cuisine', x='Cuisine', y='Restaurant closing ratio') +
  coord_flip()
restaurantClosingRatioByCuisine.gg #Overall restaurant closing ratio by cuisine



###Ratio of unique restaurant that closed by cuisine - with boro
restaurantClosingSummaryByCuisineBoro = inspectionsFiltered %>%
  group_by(., camis, cuisine, boro) %>%
  summarise(., n = sum(action %in% c('closed','reclosed'))) %>%
  group_by(., cuisine, boro) %>%
  summarise(., unique_camis_closed = sum(n>0), total_camis = n(), ratio = unique_camis_closed / total_camis)
restaurantClosingSummaryByCuisineBoro #Unique closings by count of camis and % of repeat offenders

#Refactor cuisines to keep order in plot as previous plot
restaurantClosingSummaryByCuisine$cuisine = factor(restaurantClosingSummaryByCuisine$cuisine)
newFactor = levels(restaurantClosingSummaryByCuisine$cuisine)[order(-rank(restaurantClosingSummaryByCuisine$ratio))]
restaurantClosingSummaryByCuisineBoro$cuisine = factor(restaurantClosingSummaryByCuisineBoro$cuisine, levels=rev(newFactor))

restaurantClosingRatioByCuisineBoro.gg = ggplot(data=restaurantClosingSummaryByCuisineBoro, aes(x=cuisine, y=ratio)) + 
  geom_bar(stat='identity', aes(fill=boro)) + 
  facet_grid(~ boro) +
  labs(title='Restaurant closing ratio by cuisine and borough', x='Cuisine', y='Restaurant closing ratio') +
  coord_flip()
restaurantClosingRatioByCuisineBoro.gg #Overall restaurant closing ratio by cuisine and borough





###############################
#### Reclosings by cuisine ####
##############################

#Create list of all unique restaurants that were closed at least once (we are considering closings, not reclosings as part of the same inspection cycle)
reclosingsByCuisine = unique(
  inspectionsFiltered %>%
    filter(., action == 'closed') %>%
    select(., camis, cuisine, inspection.date)) %>%
  group_by(., camis, cuisine) %>%
  summarise(., closures = n()) %>% 
  group_by(., cuisine) %>%
  summarise(., closed_once = sum(closures==1), closed_more_than_once = sum(closures>1), total_closed = n()) %>%
  mutate(., ratio = closed_more_than_once / total_closed) 
reclosingsByCuisine

ggplot(data = reclosingsByCuisine, aes (x = reorder(cuisine, ratio), y = ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Ratio of repeat closures by cuisine', x='Cuisine', y='Ratio of repeat closings') +
  coord_flip()




# #Unique restaurant closings by cuisine - with borough
# reclosingsByCuisineBoro = unique(
#   inspectionsFiltered %>%
#     filter(., action == 'closed') %>%
#     select(., camis, cuisine, inspection.date, boro)) %>%
#   group_by(., camis, cuisine, boro) %>%
#   summarise(., closures = n()) %>% 
#   group_by(., cuisine, boro) %>%
#   summarise(., closed_once = sum(closures==1), closed_more_than_once = sum(closures>1), total_closed = n()) %>%
#   mutate(., ratio = closed_more_than_once / total_closed) 
# reclosingsByCuisineBoro
# 
# ggplot(data = reclosingsByCuisineBoro, aes (x = reorder(cuisine, ratio), y = ratio)) + 
#   geom_bar(stat='identity', aes(fill=boro)) +
#   facet_grid(~boro) +
#   labs(title='Ratio of repeat closures by cuisine and borough', x='Cuisine', y='Ratio of repeat closings') +
#   coord_flip()
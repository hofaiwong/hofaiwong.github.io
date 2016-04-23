##############################################
###  Data Science Bootcamp 5               ###
###  Project 1 - Exploratory Visualization ###
###  Ho Fai Wong / April 24, 2016          ###
###  NYC restaurant health inspections     ###
##############################################


###########################
#### Data load/cleanup ####
###########################

#Load libraries
library(plyr)
library(dplyr)
library(zipcode)
library(ggplot2)
library(choroplethrZip)
library(lubridate)

#Load inspection data
setwd("~/Desktop/Project Data")
raw = read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = F)
raw.df = tbl_df(raw)

#Rename and re-format columns
names(raw.df) = tolower(names(raw.df))
raw.df = rename(raw.df, cuisine = cuisine.description) %>%
  mutate(., inspection.date = as.Date(inspection.date, "%m/%d/%Y")) %>%
  mutate(., grade.date = as.Date(grade.date, "%m/%d/%Y")) %>%
  mutate(., record.date = as.Date(record.date, "%m/%d/%Y")) %>%
  mutate(., phone = as.double(phone)) %>%
  mutate(., boro = factor(boro, levels(factor(boro))[c(1:3,5:6,4)])) %>%
  mutate(., zipcode = clean.zipcodes(zipcode)) %>% #Turn zip to string with clean format e.g. preceding 0s, for zip_choroplethr

  #Shorten/reword action and cuisine
  mutate(., cuisine = gsub(pattern = 'Latin \\(Cuban, Dominican, Puerto Rican, South \\& Central American\\)', replacement = 'Latin', x = cuisine, ignore.case = F)) %>%
  mutate(., cuisine = gsub(pattern = 'CafÃ©/Coffee/Tea', replacement = 'Cafe/Coffee/Tea', x = cuisine, ignore.case = F)) %>%
  mutate(., cuisine = factor(cuisine, levels(factor(cuisine))[c(1:55,57,59:84,58,56)])) %>% #Moving N/A and Other to the bottom of cuisine factors
  mutate(., action = gsub(pattern = "Violations were cited in the following area\\(s\\).", replacement = "violations", x = action, ignore.case = F)) %>%
  mutate(., action = gsub(pattern = "No violations were recorded at the time of this inspection.", replacement = "no violations", x = action, ignore.case = F)) %>%
  mutate(., action = gsub(pattern = "Establishment re-opened by DOHMH", replacement = "reopened", x = action, ignore.case = F)) %>%
  mutate(., action = gsub(pattern = "Establishment Closed by DOHMH.  violationsand those requiring immediate action were addressed.", replacement = "closed", x = action, ignore.case = F)) %>%
  mutate(., action = gsub(pattern = "Establishment re-closed by DOHMH", replacement = "reclosed", x = action, ignore.case = F)) %>%

  #Add columns for analysis
  mutate(., yearmon = as.Date(paste("1",
                                    strftime(inspection.date, "%m"), 
                                    strftime(inspection.date, "%Y"), 
                                    sep="."), 
                              format="%d.%m.%Y")) %>% #Add column for month and year of inspection
  mutate(., new_grade = ifelse(score < 0, 'Negative', 
                               ifelse(score < 14 , 'A',
                                      ifelse(score < 28, 'B', 'C')))) #Assign grades based on scores (existing data may have score but no grade)

  #Fix bad data
levels(raw.df$boro) = c('Bronx','Brooklyn','Manhattan','Queens','Staten Island','Missing')
raw.df$boro[raw.df$zipcode==11249] = 'Brooklyn' #Fill missing data...
raw.df = filter(raw.df, 
                  zipcode!='07005' & #Eliminate NJ inspection...
                  boro != 'Missing' & #Eliminate remaining rows with boro == 'Missing'; confirmed that none are in NYC
                  inspection.date > '1900-01-01' & #Eliminate rows where the restaurant hasn't been inspected yet
                  !is.na(score) & #Eliminate rows without a score
                  score >= 0 #Eliminate rows with a negative score
)





####################################
#### Create tables for analysis ####
####################################

#Unique inspections
inspections = unique(select(raw.df, camis, boro, zipcode, cuisine, 
                            inspection.date, action, score, new_grade, yearmon))

#Unique inspections but only the latest for each restaurant
latest = merge(
  aggregate(inspection.date ~ camis, inspections, max), 
  inspections)

#Unique restaurants and their latest score/grade
restaurants = unique(select(latest, camis, boro, zipcode, cuisine, score, new_grade))




####################################
####  Current state by borough  ####
####################################

#Bar plot: Restaurants by borough and latest grade
ggplot(data=latest, aes(x=reorder(boro, boro, function(x)-length(x)))) + 
  geom_bar(aes(fill=new_grade), position='dodge') +
  labs(title='Restaurants by borough and latest grade', 
       x='Borough', 
       y='Restaurants') +
  scale_y_continuous(limits=c(0,9000), breaks=seq(0,10000,1000)) +
  scale_fill_brewer(name="Grade", palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())
  

# #Box plot: Latest score by borough
# ggplot(data=latest, aes(x=reorder(boro, boro, function(x)-length(x)), y=score)) + 
#   geom_boxplot(aes(fill=boro)) + 
#   labs(title='Latest score by borough', 
#        x='Borough', 
#        y='Latest score') + 
#   stat_summary(fun.y=mean, colour="white", geom="point", shape=16, size=2) +
#   scale_fill_brewer(name="Borough", palette='Set1')
#
# #Freqpoly: Restaurants by latest score and borough
# ggplot(data=latest, aes(x=score)) + 
#   geom_freqpoly(aes(color=boro), binwidth=2) + 
#   coord_cartesian(xlim=c(0,40)) + 
#   labs(title='Restaurants by latest score and borough', 
#        x='Latest score', 
#        y='Restaurants') + 
#   scale_colour_brewer(name="Borough", palette='Set1')

#Density: Restaurants by latest score and borough
ggplot(data=latest, aes (x=score)) + 
  geom_density(aes(color=boro)) + 
  coord_cartesian(xlim=c(0,40)) +
  labs(title='Density of restaurants by latest score and borough', 
       x='Score', 
       y='Restaurant density') + 
  scale_y_continuous(breaks=seq(0,0.14,0.02),
                     labels = scales::percent) +
  scale_colour_brewer(name="Borough", palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())




####################################
####  Current state by zipcode  ####
####################################

#Map average scores by zipcode
avgbyzip = latest %>% 
  group_by(., zipcode) %>%
  summarise(., value=mean(score)) %>%
  rename(., region=zipcode)

#Choroplethr doesn't have all zipcodes in the NY inspection data (uses 2010 census ZCTAs)
scored_zips = unique(avgbyzip$region) #zipcodes that appear in inspection data data
missing_zips = c('10048','10055', '10057', '10104', '10105', '10106', '10107', '10118', '10121', '10123', '10155', '10166', '10175', '10176', '10178', '10179', '10281', '10285', '10317', '11242', '11249', '11256', '11352') #zipcodes that are not in choropleth package
adjusted_zips = scored_zips[!(scored_zips %in% missing_zips)] #zipcodes that appear in original data and in choropleth package

#Map: Average scores by zipcode; zipcodes are binned into 9 buckets of equal size due to limitations of the function
zip_choropleth(avgbyzip, 
               zip_zoom = adjusted_zips,
               title="Average of latest scores by zipcode",
               num_colors=5) + 
  scale_fill_brewer(palette='OrRd', name='Average score')




#############################
#### Closures by borough ####
#############################

#Total counts of inspections that led to closures by borough
inspClosByBoro = inspections %>%
  group_by(., boro) %>%
  summarise(., 
            closures = sum(action == 'closed' | action == 'reclosed'),
            inspections = n(),
            ratio = closures / inspections
  )

#Bar plot: Inspection closure ratio by borough
ggplot(data=inspClosByBoro, aes(x=reorder(boro, -ratio), y=ratio)) + 
  geom_bar(stat='identity', aes(fill=boro)) +
  labs(title='Inspection closure ratio by borough', 
       x='Borough', 
       y='Inspection closure ratio') +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_brewer(palette = 'Set1', name = 'Borough') + 
  theme_bw() +
  theme(legend.key=element_blank())




###############################
#### Reclosures by borough ####
###############################

#Restaurants that were closed at least once (closures, not reclosures as part of the same inspection cycle)
reclosedByBoro =  inspections %>% 
  filter(., action == 'closed') %>%
  group_by(., camis, boro, zipcode) %>%
  summarize(., count = n()) %>%
  group_by(., boro) %>%
  summarize(., total_closed = n(), 
            closed_more_than_once = sum(count>1), 
            ratio = closed_more_than_once / total_closed)

#Bar plot: Ratio of restaurants by borough that were closed more than once, out of restaurants that were closed at least once
ggplot (data = reclosedByBoro, aes (x = reorder(boro, -ratio), y = ratio)) + 
  geom_bar(stat='identity', aes(fill=boro)) +
  labs(title='Repeat closure ratio by borough', 
       x='Borough', 
       y='Repeat closure ratio') +
  scale_y_continuous(limits=c(0,0.12), 
                     breaks=seq(0,0.12,0.02), 
                     labels = scales::percent) + 
  scale_fill_brewer(palette = 'Set1', name = 'Borough') + 
  theme_bw() +
  theme(legend.key=element_blank())




####################################
####  Current state by cuisine  ####
####################################

#Down-select top 20 cuisine types for further analysis (out of 84)
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
restaurantsFiltered = tbl_df(semi_join(restaurants, restByCuisineFiltered, by = 'cuisine')) #Will keep top 20 covering ~81%
inspectionsFiltered = tbl_df(semi_join(inspections, restByCuisineFiltered, by = 'cuisine'))

# #Stacked bar plot: Restaurants by cuisine and grade
# ggplot(data=restaurantsFiltered, aes(x=reorder(cuisine, cuisine, function(x) length(x)))) + 
#   geom_bar(aes(fill=new_grade)) + 
#   coord_flip() +
#   labs(title='Restaurants by cuisine (top 30) and grade',
#        x='Cuisine (Top 30)',
#        y='Restaurants') +
#   scale_fill_discrete(name='Grade')
# 
# #Density: Restaurants by score and borough
# ggplot(data=restaurantsFiltered, aes (x=score)) +
#   geom_density(aes(color=cuisine)) +
#   coord_cartesian(xlim=c(0,40)) +
#   labs(title='Density of restaurants by score and cuisine', x='Score', y='Density') +
#   scale_colour_discrete(name="Cuisine")



#############################
#### Closures by cuisine ####
#############################

#Ratio of inspection closures by cuisine
inspClosByCuisine = inspectionsFiltered %>%
  group_by(., cuisine) %>%
  summarise(., 
            closures = sum(action == 'closed' | action == 'reclosed'),
            reclosures = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closures / inspections
  )

#Bar plot: Inspection closure ratio by cuisine
ggplot(data=inspClosByCuisine, aes(x=reorder(cuisine, ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Inspection closure ratio by cuisine', 
       x='Cuisine', 
       y='Inspection closure ratio') +
  scale_y_continuous(limits=c(0,0.03),
                     breaks=seq(0,3,0.005),
                     labels = scales::percent) +
  theme_bw() +
  coord_flip()



#Ratio of inspection closures by cuisine and boro
inspClosByCuisineBoro = inspectionsFiltered %>%
  group_by(., cuisine, boro) %>%
  summarise(., 
            closures = sum(action == 'closed' | action == 'reclosed'),
            reclosures = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closures / inspections
  )

#Refactor cuisines to keep same order as in previous plot
inspClosByCuisineBoro$cuisine = factor(inspClosByCuisineBoro$cuisine)
newFactor = levels(inspClosByCuisineBoro$cuisine)[order(-rank(inspClosByCuisine$ratio))]
inspClosByCuisineBoro$cuisine = factor(inspClosByCuisineBoro$cuisine, levels=rev(newFactor))

#Faceted bar plot: Inspection closure ratio by cuisine and borough
ggplot(data=inspClosByCuisineBoro, aes(x=cuisine, y=ratio)) + 
  geom_bar(stat='identity', aes(fill=boro)) + 
  facet_grid(. ~ boro) +
  labs(title='Inspection closure ratio by cuisine and borough', x='Cuisine', y='Inspection closure ratio') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = 'Borough') +
  theme_bw() +
  theme(legend.key=element_blank(), legend.position="bottom") +
  coord_flip()
  




###############################
#### Reclosures by cuisine ####
###############################

#Restaurants that were closed at least once (closures, not reclosures as part of the same inspection cycle)
reclosedByCuisine = unique(
  inspectionsFiltered %>%
    filter(., action == 'closed') %>%
    select(., camis, cuisine, inspection.date)) %>%
  group_by(., camis, cuisine) %>%
  summarise(., closures = n()) %>% 
  group_by(., cuisine) %>%
  summarise(., closed_once = sum(closures==1), 
            closed_more_than_once = sum(closures>1), 
            total_closed = n(),
            ratio = closed_more_than_once / total_closed) 


#Bar plot: Ratio of restaurants by cuisine that were closed more than once, out of restaurants that were closed at least once
ggplot(data = reclosedByCuisine, aes (x = reorder(cuisine, ratio), y = ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Ratio of repeat closures by cuisine', 
       x='Cuisine', 
       y='Ratio of repeat closures') +
  scale_y_continuous(limits=c(0,0.2),
                     breaks=seq(0,0.2,0.05),
                     labels=scales::percent) +
  theme_bw() +
  coord_flip()




########################
#### Score by month ####
########################

#Trend of average scores by month and borough
trendScore = inspections %>%
  mutate(., month=month(inspection.date)) %>%
  group_by(., month, boro) %>%
  summarise(., avg = mean(score))

ggplot(data=trendScore, aes(x = month, y = avg)) + 
  geom_freqpoly(stat='identity', aes(color=boro)) +
  labs(title='Average score by month and borough',
       x='Month',
       y='Average score') +
  coord_cartesian(xlim = c(1,12), ylim = c(13,18)) +
  scale_x_continuous(breaks = 1:12, 
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_color_brewer(name='Borough', palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())


################################
#### Closure ratio by month ####
################################

#Trend of inspection closure ratio by month and borough
trendClosures = inspections %>% 
  mutate(., month=month(inspection.date)) %>%
  group_by(., month, boro) %>%
  summarise(., 
            closures = sum(action == 'closed' | action == 'reclosed'),
            inspections = n(),
            ratio = closures / inspections
  )

ggplot(data=trendClosures, aes(x=month, y=ratio)) + 
  geom_freqpoly(stat='identity', aes(color=boro)) + 
  labs(title='Inspection closure ratio by month and borough',
       x='Month',
       y='Inspection closure ratio') +
  coord_cartesian(xlim = c(1,12)) +
  scale_x_continuous(breaks = 1:12, 
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(limits=c(0.005,0.035),
                     breaks=seq(0,0.035,0.005),
                     labels=scales::percent) +
  scale_color_brewer(name='Borough', palette='Set1') +
  theme_bw() +
  theme(legend.key=element_blank())
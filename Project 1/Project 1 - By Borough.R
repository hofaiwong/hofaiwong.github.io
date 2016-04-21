#Project 1

##########################
####     Data load    ####
##########################
library(plyr)
library(dplyr)
library(zipcode)
library(ggplot2)
#library(zoo)
#library(reshape2)
# install.packages("devtools")
#library(devtools)
#install_github('arilamstein/choroplethrZip@v1.5.0')
#library(choroplethr)
library(choroplethrZip)

setwd("~/Desktop/hofaiwong.github.io/Project 1")
raw = read.csv("~/Desktop/Project Data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = FALSE)
raw.df = tbl_df(raw)
names(raw.df) = tolower(names(raw.df))
raw.df = rename(raw.df, cuisine = cuisine.description)
raw.df$inspection.date = as.Date(raw.df$inspection.date, "%m/%d/%Y")
raw.df$grade.date = as.Date(raw.df$grade.date, "%m/%d/%Y")
raw.df$record.date = as.Date(raw.df$record.date, "%m/%d/%Y")
raw.df$phone = as.double(raw.df$phone)
raw.df$boro = factor(raw.df$boro, levels(factor(raw.df$boro))[c(1:3,5:6,4)])
levels(raw.df$boro) = c('Bronx','Brooklyn','Manhattan','Queens','Staten Island','Missing')
raw.df$cuisine = factor(raw.df$cuisine, levels(factor(raw.df$cuisine))[c(1:55,57,59:84,58,56)]) #Moving N/A and Other to the bottom of cuisine factors
raw.df$zipcode = clean.zipcodes(raw.df$zipcode) #Turn zip to string with clean format e.g. preceding 0s

raw.df$action = gsub(pattern = "Violations were cited in the following area\\(s\\).", replacement = "violations", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "No violations were recorded at the time of this inspection.", replacement = "no violations", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment re-opened by DOHMH", replacement = "reopened", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment Closed by DOHMH.  violationsand those requiring immediate action were addressed.", replacement = "closed", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment re-closed by DOHMH", replacement = "reclosed", x = raw.df$action, ignore.case = F)

raw.df$boro[raw.df$zipcode==11249] = 'Brooklyn' #Fill missing data...
raw.df = raw.df[raw.df$zipcode!='07005',] #Eliminate NJ inspection...
raw.df = raw.df[raw.df$boro != 'Missing',] #Eliminate remaining rows with boro == 'Missing'; confirmed that none are in NYC
raw.df = raw.df[raw.df$inspection.date > '1900-01-01',] #Eliminate rows where the restaurant hasn't been inspected yet
raw.df = raw.df[!is.na(raw.df$score),] #Eliminate rows without a score
raw.df = raw.df[raw.df$score >= 0,] #Eliminate rows with a negative score ----> PENDING


###########################
###  Scores by borough  ###
###########################

#Creating table for unique restaurants
restaurants = unique(select(latest, camis, boro, zipcode, cuisine, score, new_grade))
#ggplot(data = restaurants, aes(x=reorder(boro, boro, function(x)-length(x)))) + geom_bar(aes(fill=new_grade))

#Creating table for unique inspections
inspections = unique(select(raw.df, camis, boro, zipcode, cuisine, inspection.date, action, score))
inspections$yearmon = as.Date(paste("1",strftime(inspections$inspection.date, "%m"), strftime(inspections$inspection.date, "%Y"), sep="."), format="%d.%m.%Y")
inspections$new_grade = ifelse(inspections$score < 0, 'Negative', ifelse(inspections$score < 14 , 'A', ifelse(inspections$score < 28, 'B', 'C'))) #Assign grades based on scores (existing data may have score but no grade)
#ggplot(data = inspections, aes(x=reorder(boro, boro, function(x)-length(x)))) + geom_bar()

#Creating table for latest scores only
latest = merge(aggregate(inspection.date ~ camis, inspections, max), inspections)

#Visualize current state by borough
ggplot(data=latest, aes(x=reorder(boro, boro, function(x)-length(x)))) + 
  geom_bar(aes(fill=new_grade), position='dodge') +
  labs(title='Restaurants by latest grades and borough', x='Borough', y='Restaurants') +
  scale_fill_discrete(name="Latest \ngrades")

ggplot(data=latest, aes(x=reorder(boro, boro, function(x)-length(x)), y=score)) + 
  geom_boxplot(aes(fill=boro)) + 
  labs(title='Latest scores by borough', x='Borough', y='Latest scores') + 
  stat_summary(fun.y=mean, colour="white", geom="point", shape=16, size=2)


# Violin plot: prefer whiskey boxplot
# ggplot(data=latest, aes(x=reorder(boro, boro, function(x)-length(x)), y=score)) + 
#   geom_violin(aes(fill=boro)) + 
#   labs(title='Latest scores by borough', x='Borough', y='Latest scores')


#ggplot(data=latest, aes(x=boro)) + geom_bar(aes(fill=new_grade))
#ggplot(data=latest, aes(x=boro)) + geom_bar(aes(fill=new_grade), position = 'fill')
#ggplot (data=latest, aes(x=score)) + geom_histogram(binwidth = 2, aes(fill = boro))
#ggplot (data=latest, aes(x=score)) + geom_histogram(binwidth = 2, aes(fill = new_grade))

ggplot(data=latest, aes(x = score)) + 
  geom_freqpoly(aes(color=boro), binwidth=2) + 
  coord_cartesian(xlim=c(0,40)) + 
  labs(title='Restaurants by latest score and borough', x='Score', y='Restaurants') + 
  scale_colour_discrete(name="Borough")

ggplot(data=latest, aes (x=score)) + 
  geom_density(aes(color=boro)) + 
  coord_cartesian(xlim=c(0,40)) +
  labs(title='Density of restaurants by latest score and borough', x='Score', y='Density') + 
  scale_colour_discrete(name="Borough")

#Map average scores by zipcode
avgbyzip = latest %>% 
  group_by(., zipcode) %>%
  summarise(., mean(score))
colnames(avgbyzip) = c('region','value')
scored_zips = unique(avgbyzip$region) #zipcodes that appear in original data
missing_zips = c('10048','10055', '10057', '10104', '10105', '10106', '10107', '10118', '10121', '10123', '10155', '10166', '10175', '10176', '10178', '10179', '10281', '10285', '10317', '11242', '11249', '11256', '11352') #zipcodes that are not in choloroplethr package
adjusted_zips = scored_zips[!(scored_zips %in% missing_zips)] #zipcodes that appear in original data and in choloroplethr package

zip_choropleth(avgbyzip, 
               zip_zoom = adjusted_zips,
               title="Average of latest scores by zipcode",
               legend="Average scores",
               num_colors = 5)
####--------> change bins


##########################
#### Closings by boro ####
##########################

# ###Summarise closings, reclosings and inspections by borough and month/year
# inspectionsByMonth = inspections %>%
#   group_by(., boro, yearmon) %>%
#   summarise(., inspections=n())
# 
# closingsByMonth = filter(inspections, action %in% c('closed','reclosed')) %>%
#   group_by(., boro, yearmon) %>%
#   summarise(., closings=n())
# 
# reclosingsByMonth = filter(inspections, action=='reclosed') %>%
#   group_by(., boro, yearmon) %>%
#   summarise(., reclosings=n())
# 
# ###Ratio of inspections that lead to closings by borough
# inspectionClosingSummary = cbind(summarize(group_by(closingsByMonth, boro), closings = sum(closings)), 
#                      summarize(group_by(reclosingsByMonth, boro), reclosings = sum(reclosings))[,2],
#                      summarize(group_by(inspectionsByMonth, boro), inspections = sum(inspections))[,2])
# inspectionClosingSummary = mutate(inspectionClosingSummary, ratio = closings / inspections)




#########REPLACE WITH THIS:
inspectionClosingSummary = inspections %>%
  group_by(., boro) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            reclosings = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections
  )

inspectionClosingRatio.gg = ggplot(data=inspectionClosingSummary, aes(x=reorder(boro, -ratio), y=ratio)) + 
  geom_bar(stat='identity') +
  labs(title='Inspection closing ratio by borough', x='Borough', y='Inspection closing ratio')
inspectionClosingRatio.gg #Overall inspection closing ratio by borough

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
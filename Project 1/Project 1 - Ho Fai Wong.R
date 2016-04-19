#Project 1

##########################
####     Data load    ####
##########################

library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)
setwd("~/Desktop/Project 1")
raw = read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = FALSE)
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

raw.df$action = gsub(pattern = "Violations were cited in the following area\\(s\\).", replacement = "violations", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "No violations were recorded at the time of this inspection.", replacement = "no violations", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment re-opened by DOHMH", replacement = "reopened", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment Closed by DOHMH.  violationsand those requiring immediate action were addressed.", replacement = "closed", x = raw.df$action, ignore.case = F)
raw.df$action = gsub(pattern = "Establishment re-closed by DOHMH", replacement = "reclosed", x = raw.df$action, ignore.case = F)

raw.df$boro[raw.df$zipcode==11249] = 'Brooklyn' #Fill missing data
raw.df = raw.df[raw.df$boro != 'Missing',] #Eliminate remaining rows with boro == 'Missing'; confirmed that none are in NYC
raw.df = raw.df[raw.df$inspection.date > '1900-01-01',] #Eliminate rows where the restaurant hasn't been inspected yet
raw.df = raw.df[!is.na(raw.df$score),] #Eliminate rows without a score
raw.df = raw.df[raw.df$score >= 0,] #Eliminate rows with a negative score ----> PENDING


##########################
###  Latest by borough ###
##########################

#Creating table for unique inspections
inspections = unique(select(raw.df, camis, boro, zipcode, cuisine, inspection.date, action, score))
inspections$yearmon = as.Date(paste("1",strftime(inspections$inspection.date, "%m"), strftime(inspections$inspection.date, "%Y"), sep="."), format="%d.%m.%Y")
inspections$new_grade = ifelse(inspections$score < 0, 'Negative', ifelse(latest$score < 14 , 'A', ifelse(latest$score < 28, 'B', 'C'))) #Assign grades based on scores (existing data may have score but no grade)

#Creating table for latest scores only
latest = merge(aggregate(inspection.date ~ camis, inspections, max), inspections)

#Current graphs by borough

#ggplot(data=latest, aes(x=boro)) + geom_bar(aes(fill=new_score))
#ggplot(data=latest, aes(x=boro)) + geom_bar(aes(fill=new_score), position = 'fill')
#ggplot(data=latest, aes(x=boro)) + geom_bar(aes(fill=new_score), position = 'dodge')
#ggplot (data = latest, aes(x = boro, y = score)) + geom_boxplot()
#ggplot (data = latest, aes(x=score)) + geom_histogram(binwidth = 2, aes(fill = boro))
#ggplot (data = latest, aes(x=score)) + geom_histogram(binwidth = 2, aes(fill = new_score))

scoreByBoro.gg = ggplot (data = latest[latest$boro != 'Missing',], aes (x = score)) + 
  geom_freqpoly(aes(color = boro), binwidth=2) + 
  coord_cartesian(xlim = c(0,40)) + 
  xlab("Score") + ylab("Count of establishments") + 
  scale_colour_discrete(name="Borough") + 
  ggtitle("Establishments by latest score and borough")
scoreByBoro.gg

scoreDensityByBoro.gg = ggplot (data = latest[latest$boro != 'Missing',], aes (x = score)) + 
  geom_density(aes(color = boro)) + 
  coord_cartesian(xlim = c(0,40)) +
  xlab("Score") + ylab("Density") + 
  scale_colour_discrete(name="Borough") + 
  ggtitle("Density of establishments by latest score and borough")
scoreDensityByBoro.gg



##########################
####     Closings     ####
##########################

###Distinct closings, reclosings, reopenings and inspections tables
closings = filter(inspections, action %in% c('closed','reclosed'))
reclosings = filter(inspections, action == 'reclosed')
#reopenings = unique(inspections[grep('reopen',inspections$action, ignore.case = T),])

cntInspections = summarize(group_by(inspections, boro, yearmon), inspections = n())
cntClosings = summarize(group_by(closings, boro, yearmon), closings = n())
cntReclosings = summarize(group_by(reclosings, boro, yearmon),reclosings = n())
#cntReopenings = summarize(group_by(reopenings, boro, yearmon), reopenings = n())


###Closings by borough
closingRatio = cbind(summarize(group_by(cntClosings, boro), closings = sum(closings)), 
                     summarize(group_by(cntReclosings, boro), reclosings = sum(reclosings))[,2],
                     summarize(group_by(filter(cntInspections, boro != 'Missing'), boro), inspections = sum(inspections))[,2])
closingRatio = mutate(closingRatio, closingRatio = closings / inspections)

closingRatio.gg = ggplot (data = closingRatio[closingRatio$boro != 'Missing',], aes (x = reorder(boro, -ratio), y = ratio)) + 
  geom_bar(stat='identity') +
  xlab("Borough") + ylab("Inspection closing ratio") + 
  ggtitle('Inspection closing ratio by Borough')
closingRatio.gg #Overall inspection closing ratio by borough

cntClosingsStats = merge(summarize(group_by(unique(select(closings, camis, boro)), boro), unique_camis_closed = n()),
      summarize(group_by(unique(select(inspections, camis, boro)), boro), total_camis = n()),
      by = 'boro')
cntClosingsStats$ratio = cntClosingsStats$unique_camis_closed / cntClosingsStats$total_camis
cntClosingsStats #Closings by count of camis and % of repeat offenders


##Reclosings summary
reclosingsByID = dcast(summarize(group_by(closings, camis, boro, action), n()), camis + boro ~ temp$action)

closedByBoro = group_by(reclosingsByID[reclosingsByID$closed >0,], boro) %>%
  summarize(., closed = n())
closedOnceByBoro = group_by(reclosingsByID[reclosingsByID$closed == 1,], boro) %>%
  summarize(., closed = n())
closedMoreThanOnceByBoro = group_by(reclosingsByID[reclosingsByID$closed > 1,], boro) %>%
  summarize(., closed = n())

reclosingsByBoro = cbind(closedByBoro, closedOnceByBoro[,2], closedMoreThanOnceByBoro[,2])
colnames(reclosingsByBoro) = c('boro','total_closed','closed_once','closed_more_than_once')
reclosingsByBoro$ratioClosedMoreThanOnce = reclosingsByBoro$closed_more_than_once / reclosingsByBoro$total_closed

ggplot(data = reclosingsByBoro, aes(x=boro))
reclosingRatio.gg = ggplot (data = reclosingsByBoro[!is.na(reclosingsByBoro$boro),], aes (x = reorder(boro, -ratioClosedMoreThanOnce), y = ratioClosedMoreThanOnce)) + 
  geom_bar(stat='identity') +
  xlab("Borough") + ylab("Repeat closing ratio") + 
  ggtitle('Ratio of repeat closures by Borough')
reclosingRatio.gg #Ratio of establishments closed more than once by borough


###Graphs for closings and inspections over time by borough
# closings.gg = ggplot (data = closings, aes (x = inspection.date)) + 
#   geom_freqpoly(aes(color = boro), binwidth = 90) + #confirm bin for dates
#   xlab("Inspection Date") + ylab("Count of closings") + 
#   scale_colour_discrete(name="Borough") + 
#   ggtitle("Count of closings over time by borough")
# closings.gg

closingDensity.gg = ggplot (data = closings, aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  xlab("Inspection Date") + ylab("Density of closings") + 
  scale_colour_discrete(name="Borough") + 
  ggtitle("Density of closings over time by borough")
closingDensity.gg

# inspections.gg = ggplot (data = inspections, aes (x = inspection.date)) + 
#   geom_freqpoly(aes(color = boro), binwidth=30) + 
#   xlab("Inspection Date") + ylab("Count of inspections") + 
#   scale_colour_discrete(name="Borough") + 
#   ggtitle("Count of inspections over time by borough")
# inspections.gg

inspectionsDensity.gg = ggplot (data = inspections[inspections$boro != 'Missing',], aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  xlab("Inspection Date") + ylab("Density of inspections") + 
  scale_colour_discrete(name="Borough") + 
  ggtitle("Density of inspections over time by borough")
inspectionsDensity.gg

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





#By Time
ggplot (data = latest, aes(x = inspection.date, y = score)) + geom_point()

#By aggregate stat
ggplot(data = mean_score_by_boro, aes(x=boro, y = latest_mean_score)) + geom_point()

#Current graphs by cuisine
ggplot(data=latest, aes(x=reorder(latest$cuisine.description,latest$score), y=latest$score)) + coord_flip() + geom_bar(stat='identity')



#Calculate latest aggregate stats
mean_score_by_zip = group_by(latest, zipcode) %>%
  summarise(round(mean(score),2))
mean_score_by_zip$zipcode = as.character(mean_score_by_zip$zipcode)

mean_score_by_cuisine = group_by(latest, cuisine) %>%
  summarise(latest_mean_score = round(mean(score),2))

mean_score_by_boro = group_by(latest, boro) %>%
  summarise(round(mean(score),2))
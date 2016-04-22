###########################
#### Trends by borough ####
###########################

####-----------> Need trend of closing ratio over time

# #Count of closings over time by borough
# ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) +
#   geom_freqpoly(aes(color = boro), binwidth = 90) + #confirm bin for dates
#   labs(title='Count of closings over time by borough', x='Inspection date', y='Count of closings') +
#   scale_colour_discrete(name="Borough")

#Density of closings over time by borough
ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  labs(title='Density of closings over time by borough', x='Inspection date', y='Density of closings') +
  scale_colour_discrete(name="Borough")

# #Count of inspections over time by borough
# ggplot (data = inspections, aes (x = inspection.date)) +
#   geom_freqpoly(aes(color = boro), binwidth=30) +
#   labs(title = 'Count of inspections over time by borough', x='Inspection date', y='Count of inspections') +
#   scale_colour_discrete(name="Borough")

#Density of inspections over time by borough
ggplot (data = inspections, aes (x = inspection.date)) + 
  geom_density(aes(color = boro)) + 
  labs(title = 'Density of inspections over time by borough', x='Inspection date', y='Density of inspections') +
  scale_colour_discrete(name="Borough")


#Inspection closing ratio by month
library(lubridate)


# #Trend of inspection closure ratio by month - Too simplistic
# closingsByMonth = inspections %>% 
#   mutate(., month=month(inspection.date)) %>%
#   group_by(., month) %>%
#   summarise(., 
#             closings = sum(action == 'closed' | action == 'reclosed'),
#             reclosings = sum(action == 'reclosed'),
#             inspections = n(),
#             ratio = closings / inspections
#   )
# 
# ggplot(data=closingsByMonth, aes(x=month, y=ratio)) + 
#   geom_line() +
#   labs(title='Inspection closure ratio by month',
#        x='Month',
#        y='Inspection closure ratio'
#        )



# #Trend of inspection closure ratio by month and borough
closingsByMonthBoro = inspections %>% 
  mutate(., month=month(inspection.date)) %>%
  group_by(., month, boro) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            reclosings = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections
  )


ggplot(data=closingsByMonthBoro, aes(x=month, y=ratio)) + 
  geom_line(stat='identity', aes(color=boro)) + 
  labs(title='Inspection closure ratio by month',
       x='Month',
       y='Inspection closure ratio') +
  coord_cartesian(xlim = c(1,12)) +
  scale_x_continuous(breaks = 1:12, 
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))



#Trend of inspection closing ratio by month and year
ClosingsByYearMonBoro = inspections %>%
  group_by(., yearmon, boro) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            reclosings = sum(action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections)

ggplot(data=ClosingsByYearMonBoro, aes(x=yearmon, y=ratio)) +
  geom_freqpoly(stat='identity', aes(color=boro)) +
  labs(title='Inspection closure ratio over time',
       x='Month/Year',
       y='Inspection closure ratio'
  ) + facet_grid(~boro)



#Trend of score by borough
trendScoreBoro = inspections %>%
  group_by(., boro, inspection.date) %>%
  summarise(., avg = mean(score))

ggplot(data=trendScoreBoro, aes(x = inspection.date, y = avg)) + 
  geom_smooth(stat='identity', aes(color=boro))


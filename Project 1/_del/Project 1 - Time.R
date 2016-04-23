########################
#### Score by month ####
########################

#Trend of average scores by month and borough
trendScore = inspections %>%
  mutate(., month=month(inspection.date)) %>%
  group_by(., month, boro) %>%
  summarise(., avg = mean(score))

trendScore.gg = ggplot(data=trendScore, aes(x = month, y = avg)) + 
  geom_freqpoly(stat='identity', aes(color=boro)) +
  labs(title='Average score by inspection month and borough',
     x='Inspection month',
     y='Average score') +
  coord_cartesian(xlim = c(1,12)) +
  scale_x_continuous(breaks = 1:12, 
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_color_brewer(name='Borough', palette='Set1')
trendScore.gg

################################
#### Closing ratio by month ####
################################

#Trend of inspection closing ratio by month and borough
trendClosings = inspections %>% 
  mutate(., month=month(inspection.date)) %>%
  group_by(., month, boro) %>%
  summarise(., 
            closings = sum(action == 'closed' | action == 'reclosed'),
            inspections = n(),
            ratio = closings / inspections
  )

trendClosings.gg = ggplot(data=trendClosings, aes(x=month, y=ratio)) + 
  geom_freqpoly(stat='identity', aes(color=boro)) + 
  labs(title='Inspection closing ratio by month and borough',
       x='Inspection month',
       y='Inspection closing ratio') +
  coord_cartesian(xlim = c(1,12)) +
  scale_x_continuous(breaks = 1:12, 
                     labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  #scale_color_discrete(name='Borough') +
  scale_color_brewer(name='Borough', palette='Set1')
trendClosings.gg




###########################
#### Trends - all time ####
###########################

# #Count of closings over time by borough
# ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) +
#   geom_freqpoly(aes(color = boro), binwidth = 90) + #confirm bin for dates
#   labs(title='Count of closings over time by borough', x='Inspection date', y='Count of closings') +
#   scale_colour_discrete(name="Borough")

# #Density of closings over time by borough
# ggplot (data = inspections[inspections$action %in% c('closed','reclosed'),], aes (x = inspection.date)) + 
#   geom_density(aes(color = boro)) + 
#   labs(title='Density of closings over time by borough', x='Inspection date', y='Density of closings') +
#   scale_colour_discrete(name="Borough")

# #Count of inspections over time by borough
# ggplot (data = inspections, aes (x = inspection.date)) +
#   geom_freqpoly(aes(color = boro), binwidth=30) +
#   labs(title = 'Count of inspections over time by borough', x='Inspection date', y='Count of inspections') +
#   scale_colour_discrete(name="Borough")

# #Density of inspections over time by borough
# ggplot (data = inspections, aes (x = inspection.date)) + 
#   geom_density(aes(color = boro)) + 
#   labs(title = 'Density of inspections over time by borough', x='Inspection date', y='Density of inspections') +
#   scale_colour_discrete(name="Borough")

# #Trend of inspection closing ratio by borough, month and year
# ClosingsByYearMonBoro = inspections %>%
#   group_by(., yearmon, boro) %>%
#   summarise(.,
#             closings = sum(action == 'closed' | action == 'reclosed'),
#             reclosings = sum(action == 'reclosed'),
#             inspections = n(),
#             ratio = closings / inspections)
# 
# ggplot(data=ClosingsByYearMonBoro, aes(x=yearmon, y=ratio)) +
#   geom_freqpoly(stat='identity', aes(color=boro)) +
#   labs(title='Inspection closure ratio over time',
#        x='Month/Year',
#        y='Inspection closure ratio'
#   ) + facet_wrap(~boro)
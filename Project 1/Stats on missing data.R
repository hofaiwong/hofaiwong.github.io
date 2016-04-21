###Stats on missing data
library(dplyr)
setwd("~/Desktop/hofaiwong.github.io/Project 1")
raw = read.csv("~/Desktop/Project Data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv", stringsAsFactors = FALSE)
missing.df = tbl_df(raw)




#Missing boro
noBoro = missing.df[missing.df$BORO == 'Missing',] #Rows with missing boro: 75
rest = unique(missing.df[missing.df$BORO == 'Missing',c('CAMIS')]) #CAMIS with missing boro: 6
temp = inner_join(missing.df, rest, by = 'CAMIS')
temp[temp$BORO == 'Missing',]
length(unique(missing.df$CAMIS)) #25920 restaurants total
unique(noBoro$ZIPCODE) #3 zipcodes not in data, 1 in NY others not
rm(noBoro, rest, temp)




#Missing score
noScore = subset(missing.df, is.na(SCORE))
dim(noScore) #30,521 rows without scores

noScore %>%
  group_by(., CAMIS, INSPECTION.DATE, ACTION) %>%
  summarise(., count=n()) %>% #26,609 inspections without scores
  summarise(., count=n()) %>%
  summarise(., count=n()) #14,020 restaurants had inspections without scores
# 15% ~ 26609/177140 inspections that don't lead to scores, normal

negScore = subset(missing.df, SCORE<0)
dim(negScore) #217 rows with negative scores

negScore %>%
  group_by(., CAMIS, INSPECTION.DATE, ACTION, SCORE) %>%
  summarise(., count=n()) %>% #54 inspections with negative scores
  summarise(., count=n()) %>%
  summarise(., count=n()) %>%
  summarise(., count=n()) #54 restaurants had inspections without scores
#0.03% ~ 54/177140 inspections that lead to negative scores

negRest = unique(select(negScore, CAMIS))
temp = inner_join(missing.df, negRest, by ='CAMIS')
#restaurants that had negative scores at an inspection could have had positive scores at another. Needs further explanation

missing.df %>%
  group_by(., CAMIS, INSPECTION.DATE, ACTION, SCORE) %>%
  summarise(., count=n()) %>% #177,140 inspections total
  summarise(., count=n()) %>%
  summarise(., count=n()) %>% #156,573 inspection days
  summarise(., count=n()) #25,920 restaurants total

rm(temp, negRest, negScore, noScore)

#No inspection date
noDate = filter(missing.df, INSPECTION.DATE == '01/01/1900') #1039 rows with no inspection date
noRest = unique(select(noDate, CAMIS)) #1039 restaurants with an inspection/row with no date
temp = inner_join(missing.df, noRest, by ='CAMIS') #1039 matches
#all restaurants without inspection dates are new restaurants that have never been inspected yet

rm(temp, noDate, noRest)




#Cuisine?
sort(unique(missing.df$CUISINE.DESCRIPTION))

cuisineNA = filter(missing.df, CUISINE.DESCRIPTION == "Not Listed/Not Applicable") %>% #104 rows with cuisine of NA
  group_by(., CAMIS) %>%
  summarise(., n()) #13 restaurants with cuisine of NA
# 0.05% ~ 13/25920 restaurants with cuisine of NA

cuisineOther = filter(missing.df, CUISINE.DESCRIPTION == "Other") %>% #1989 rows with cuisine of NA
  group_by(., CAMIS) %>%
  summarise(., n()) #1132 restaurants with cuisine of NA
 # 4.4% ~ 1132/25920 restaurants with cuisine of other

rm(cuisineNA, cuisineOther)

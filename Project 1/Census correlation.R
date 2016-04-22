library(openxlsx)
library(zipcode)


meanZIP = read.xlsx("MeanZIP-3.xlsx")
medianZIP = read.xlsx("MedianZIP-3.xlsx")

head(meanZIP)
head(medianZIP)

meanZIP$Zip = clean.zipcodes(meanZIP$Zip)

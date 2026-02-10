library(lubridate)
library(dplyr)

streamH <- read.csv("/cloud/project/activity02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activity02/site_info.csv")


exampleDate <- c("2021-01-10 05:23:30")
#parse date with year, month, day hour:minute:second
ymd_hms(exampleDate)

#parse date with timezone so that it is always in NY time
#will account for daylight savings. Time local to NY
ymd_hms(exampleDate, tz="America/New_York")

#eastern standard time (note this doesn't work during daylight savings)
ymd_hms(exampleDate, tz="EST")

streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

#extracting information from dates w/ lubridate

#extract just the year
streamH$year <- year(streamH$dateF )

#checking for a leap year
#leap_year(streamH$dateF )

# find the day of the year thats not a leap year
#yday(streamH$dateF )

#get the decimal date
#decimal_date(streamH$dateF )


#subset stream height to just include peace river
peaceH <- streamH %>% 
  filter(siteID == 2295637)

#plot date vs height with both lines and filled in points
# types -> b = points and lines, l = lines, p = points
# pch = numbers for different shapes
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#preview streamH table 
str(streamH)

#print siteInfo
siteInfo

# join site info and stream heights into a new data frame floods
# left_join keeps structure on the left table, doesnt add aditional 
# observations from right table
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier
head(floods)


#summary statistic
mean(floods$gheight.ft)


height.ave <- floods %>% # data frame with pipe
  group_by(names) %>% # group data frame by unique names
  summarise(mean.height = mean(gheight.ft)) # next summarize using mean
height.ave

#making date of year variable
floods$doy <- yday(floods$dateF)

height.day <- floods %>% # data frame with pipe
  group_by(names, doy) %>% # group data frame by unique names and doy
  summarise(mean.height = mean(gheight.ft), max.height = max(gheight.ft)) # next summarize using mean and max

max.cat <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft) #observations with height more than or equal to the major flood height

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n()) # count number of observations for each name

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = ((n()*15)/60)/24) # find out how many days the 15 minute periods add up to

#Prompt 3
flood_cat <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise( min_date = min(dateF))
flood_cat

#HW QUESTION 1

# seperate data from each stream 
fisheatingH <- streamH %>% 
  filter(siteID == 2256500)
santafeH <- streamH %>%
  filter(siteID == 2322500)
withlacoocheeH <- streamH %>%
  filter(siteID == 2312000)

# plot each river
plot(peaceH$dateF, # x data
     peaceH$gheight.ft, 
     type = "b", 
     pch = 19,
     ylab = "Peace River Stream Stages (ft)", 
     xlab = "Date")

plot(santafeH$dateF, # x data
     santafeH$gheight.ft, 
     type = "b", 
     pch = 19,
     ylab = "Santa Fe River Stream Stages (ft)", 
     xlab = "Date")

plot(fisheatingH$dateF, # x data
     fisheatingH$gheight.ft, 
     type = "b", 
     pch = 19,
     ylab = "Fisheating creek Stream Stages (ft)", 
     xlab = "Date")

plot(withlacoocheeH$dateF, # x data
     withlacoocheeH$gheight.ft, 
     type = "b", 
     pch = 19,
     ylab = "Withlacoochee river Stream Stages (ft)", 
     xlab = "Date")

#HW QUESTION 2
action_cat <- floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise( min_date = min(dateF))

moderate_cat <- floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise( min_date = min(dateF))

major_cat <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise( min_date = min(dateF))
action_cat
flood_cat
moderate_cat
major_cat

# HW Question 3
major_catH <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise( max_height = max(gheight.ft))
major_catH

# HW QUESTION 4
select(floods,datetime)

ifelse(floods$gheight.ft > 7, "greater than 7", "less than 7")

hist(floods$gheight.ft, xlab = "flood heights (ft)", ylab = "counts", col = "blue", freq = NULL)
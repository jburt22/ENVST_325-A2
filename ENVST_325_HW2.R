library(dplyr)
library(lubridate)
library(ggplot2)

streamH <- read.csv("stream_gauge.csv")
View(streamH)

siteInfo <- read.csv("site_info.csv")

#--Prompt 1--

exampleDate <- c("2021-01-10 05:23:30")
#parse date with year, month, day hour:minute:second
ymd_hms(exampleDate)

# join site info and stream heights into a new data frame floods
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier
head(floods)

#--Prompt2--

streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

floods$dateF <- ymd_hm(floods$datetime, tz="America/New_York")

height.ave <- floods %>% # data frame with pipe
  group_by(names) %>% # group data frame by unique names
  summarise(mean.height = mean(gheight.ft)) # next summarize using mean


#--Prompt 3--
max.cat <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))


#--HW 2--
#--Q1--
Santa_Fe <- floods[floods$names == "SANTA FE RIVER NEAR FORT WHITE",]
Fisheating_Creek <- floods[floods$names == "FISHEATING CREEK AT PALMDALE",]
Peace_River <- floods[floods$names == "PEACE RIVER AT US 17 AT ZOLFO SPRINGS",]
Withlacoochee_River <- floods[floods$names == " WITHLACOOCHEE RIVER AT US 301 AT TRILBY",]


ggplot(data = Santa_Fe, aes(x= dateF, y = gheight.ft)) +
       geom_point() + # make points at data point
         geom_line() + # use lines to connect data points
         labs(x="Year", y="Stream Stage(ft)",
              title = "Stream Stage Height in Santa Fe River") # make axis labels

ggplot(data = Fisheating_Creek, aes(x= dateF, y = gheight.ft)) +
  geom_point() + # make points at data point
  geom_line() + # use lines to connect data points
  labs(x="Year", y="Stream Stage(ft)",
       title = "Stream Stage Height in Fisheating Creek") # make axis labels

ggplot(data = Peace_River, aes(x= dateF, y = gheight.ft)) +
  geom_point() + # make points at data point
  geom_line() + # use lines to connect data points
  labs(x="Year", y="Stream Stage(ft)",
       title = "Stream Stage Height in Peace River") # make axis labels

ggplot(data = Withlacoochee_River, aes(x= dateF, y = gheight.ft)) +
  geom_point() + # make points at data point
  geom_line() + # use lines to connect data points
  labs(x="Year", y="Stream Stage(ft)",
       title = "Stream Stage Height in Withlacoochee River") # make axis labels

#--Q2--
action <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= action.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))

flood_level <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= flood.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))

moderate <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= moderate.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))

major <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))

rivers_first_flood <- action %>%
                    full_join(flood_level, by="names")%>%
                    full_join(moderate, by="names")%>%
                    full_join(major, by="names")

#--Q3--
floods <- mutate(floods, flood_diff = gheight.ft - major.ft)

highest_flooding <- floods %>%
  group_by(names) %>%
  summarise(max(flood_diff))

library(dplyr)
library(lubridate)

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
streamH$dateF

floods$dateF <- ymd_hm(floods$datetime, tz="America/New_York")
floods$dateF

height.ave <- floods %>% # data frame with pipe
  group_by(names) %>% # group data frame by unique names
  summarise(mean.height = mean(gheight.ft)) # next summarize using mean

#--Prompt 3--
max.cat <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft)%>% #observations with height more than or equal to the major flood height
  summarise(min(dateF))

max.cat



Floods_Mutated <- mutate(floods,.before = gheight.ft,
                         stage_meters = gheight.ft * 0.3048,
                         percent_flood = (gheight.ft / major.ft) * 100)


Floods_Mutated %>%
  mutate(Floods_Mutated, 
         major_flood = )

#Group Presentations
#Group 1 Select Function

variableA <- select(floods, names, gheight.ft, datetime)
variableB <- select(floods, gheight.ft:dateF)
variableC <- select(floods, -datetime, -siteID)

variableF <- select(floods, (!gheight.ft:dateF))

#Group 3 ifelse

floods$RiverLocation <- ifelse(floods$names == 
                                 "SANTA FE RIVER NEAR FORT WHITE", 
                               "Northern Florida", "Central Florida")

floods$RiverLocation <- ifelse(floods$names == 
                                 "SANTA FE RIVER NEAR FORT WHITE", 
                               1, 0)

floods$RiverLocationNA <- ifelse(floods$names == 
                                 "SANTA FE RIVER NEAR FORT WHITE", 
                               NA, "Not missing")

floods$RiverLocationBool <- ifelse(floods$names == 
                                 "SANTA FE RIVER NEAR FORT WHITE", 
                               TRUE, FALSE)

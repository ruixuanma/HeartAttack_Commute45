install.packages("tigris")
install.packages("blscrapeR")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tigris)
library(blscrapeR)
library(leaflet)


######### 1. The Data ############

# go to https://data.census.gov/ to extract population data for each county of FL from table S0101: Age and Sex with 2012-2016 5-years estimate
rawPop <- read.csv('/Users/maruixuan/Documents/GitHub/HeartAttack_Commute45/shiny_presentation/FLpopulation.csv')

#remove irrelevant rows 
pop_rm_rows <- rawPop[-c(1, 69), ]

#change the values for GEO_ID variable, in order to match with countyFIPS on latter process, and convert factor to numeric
pop_rm_rows$GEO_ID <- as.numeric(gsub("0500000US", "", as.character(pop_rm_rows$GEO_ID))) 

#only keep the nessesary variables and rename them
dt_pop <- pop_rm_rows %>%
  select("GEO_ID", "NAME", "S0101_C01_001E") %>%
  rename(countyFIPS = GEO_ID) %>%
  rename(countyPop = S0101_C01_001E)


########## 2. Calculate Rates ##########

# read commute over 45 min data
dt_com45 <- read.csv('/Users/maruixuan/Documents/GitHub/HeartAttack_Commute45/shiny_presentation/commuteTime45_2012_16.csv')

# convert factor to numeric
dt_com45$Value <- as.numeric(gsub(",", "", as.character(dt_com45$Value)))
class(dt_com45$Value)

# read heart attack data
dt_ha <- read.csv('/Users/maruixuan/Documents/GitHub/HeartAttack_Commute45/shiny_presentation/heartAttackER_2012_16.csv')

# convert factor to numeric
dt_ha$Value <- as.numeric(gsub(",", "", as.character(dt_ha$Value)))
class(dt_ha$Value)

# get mean of numbers of people in each county for 5 years estimate for grouped heartAttack data
mean_5year<- dt_ha %>%
  group_by(County) %>%
  summarize(Value = mean(Value))

# merge com45 data and mean of 5 years data
merge_45_ha <- merge(dt_com45, mean_5year, by = "County", all = TRUE)

# merge population data for each county between 2012-2016 with another merged dataset
mergeAll <- merge(merge_45_ha, dt_pop, by = "countyFIPS", all = TRUE)

# convert factor to numeric
mergeAll$countyPop <- as.numeric(as.character(mergeAll$countyPop))

# rename columns, keep and reorder those colomns we want, canculate rounded rates for heart attack ang long time commute 
mergeDt <- mergeAll %>%
  rename(num_45 = Value.x) %>%
  rename(num_ha = Value.y) %>%
  select("stateFIPS","State","countyFIPS", "NAME", "Year", "countyPop", "num_45", "num_ha") %>%
  mutate(perc_ha = num_ha / countyPop * 100) %>%
  mutate(perc_45 = num_45 / countyPop * 100) %>%
  mutate(hospRate = round(perc_ha, 2)) %>%
  mutate(commRate = round(perc_45, 2)) %>%
  select("stateFIPS","State","countyFIPS", "NAME", "Year", "countyPop", "num_ha", "num_45", "hospRate", "commRate")

# filter specified rows and columns and report
reportDt <- mergeDt %>%
  filter(countyFIPS == 12011 | countyFIPS ==12086 | countyFIPS == 12087 | countyFIPS == 12099) %>%
  select("NAME", "hospRate", "commRate")

# save merged data as rds file, to use as a data source in shiny
saveRDS(mergeDt, file = "mergeDt.rds")



############ 3. graphing correlates of commute time and heart attack ##########

# create scatter plot for exploring relationship between long commute time and heart attack
com_ha_plot <- ggplot(mergeDt, aes(x = num_45 , y = num_ha)) + geom_point(color = "#69b3a2") +
  labs(title = 'Number of People Who Commute 45+ Minutes vs. Hospitalization from Heart Attack \n All FL Counties: 5-year estimates 2012-2016', 
       x = 'Number of People Who Commute 45+ Minutes', y = "Number of Heart Attack Hospitalizations") +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme_classic()

plot(com_ha_plot)


########### 4. Mapping Estimates ##############

#download Florida counties boundary files using tigris package
fl <- counties(12, cb = TRUE, year = 2016)

#calculate the rounded ratio of heart attack / long time commute for all FL counties.
mapRatio <- mergeDt %>%
  mutate(ratio_unround = num_ha / num_45) %>%
  mutate(ratio = round(ratio_unround, 3)) %>%
  select("countyFIPS", "ratio") %>%
  rename(GEOID = countyFIPS)

#convert numeric to character
mapRatio$GEOID = as.character(mapRatio$GEOID)

#merge ratio dataset with spatial object
leafmap <- geo_join(fl, mapRatio, by = "GEOID")

#format popup data for leaflet map and click the counties on map to check the specific ratio
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Heart Attacks / Long Commute: </strong>", 
                    leafmap$ratio)

# create palette function for ratio and set 6 bins
pal3 <- colorBin(palette="YlOrRd", domain=c(min(leafmap$ratio), max(leafmap$ratio)), bins = 6, na.color = NULL, pretty=FALSE, alpha = TRUE)

# mapping using leaflet with polygon for counties and legend with different bins
leaflet(data = leafmap) %>% addTiles() %>%
  addPolygons(fillColor = ~pal3(leafmap$ratio), 
              fillOpacity = 1, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat) %>%
  addLegend(pal = pal3,
            values  = leafmap$ratio,
            position = "bottomleft",
            title = "Heart Attacks / Long Commute ")









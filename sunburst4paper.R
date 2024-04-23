################################################################################
#######  libraries

library(flexdashboard)
library(plotly)
library(lubridate)
library(vroom)
library(tidyverse)
library(DT)
library(crosstalk)
library(kableExtra)

###############################################################################

###############################################################################
#########  Loading data
df_Seafood <- vroom("Seafood-database_preliminary.csv")

## processing data
# Turning Character columns in Factors. But first selecting all columns of class = Character
# https://stackoverflow.com/questions/33180058/coerce-multiple-columns-to-factors-at-once
df_Seafood[map_lgl(df_Seafood, is.character)] <- lapply(df_Seafood[map_lgl(df_Seafood, is.character)], factor)

#### turning Year in to date.year
## Issue here. We only have years, and that is not a valid date format for plotting. But, since the values are annual sum. But, for plotting it looks better using the 1st of Jan. If we would use e.g. the 31.12.2019, it would look like the data is for 2020 :( 

df_Seafood$Year <-dmy(as.character(paste("01-01",df_Seafood$Year)))


df_UKPOP <- vroom("ONS_UK_population-19712020.csv")
df_UKPOP$Year <-dmy(as.character(paste("01-01",df_UKPOP$CDID)))

###############################################################################




###############################################################################
####### Data wrangling

### We are trying to build a sunburst plot from our import export dataset
## the data needs to have the following structure ids, labels, parents, values
## values need to be the total of each ids e.g. Export all fish exported

## Start with import and exports

df_Seafood %>% group_by(ids = Commodity) %>%  summarise(values = sum(na.omit(Volume)))
df_Seafood %>% group_by(parents = Commodity, ids = SpeciesType) %>%  summarise(values = sum(na.omit(Volume)))
df_Seafood %>% group_by(Commodity, parents = SpeciesType, ids = Species) %>%  summarise(values = sum(na.omit(Volume)))


## Import & Export sums
df_sunburst <- df_Seafood %>% 
  group_by(ids = as.character(Commodity)) %>% 
  summarise(values = sum(na.omit(Volume))) %>% 
  mutate(labels = ids, parents ="")


## Species Type sums
df_sunburst.temp <-df_Seafood %>% 
  group_by(parents = as.character(Commodity), labels = as.character(SpeciesType)) %>% 
  summarise(values = sum(na.omit(Volume))) %>% 
  mutate(ids = paste(parents," - ", labels)) # to create unique ids


df_sunburst <- rbind(df_sunburst,df_sunburst.temp)

## removing labels from small values
df_sunburst <- df_sunburst %>% 
  mutate(labels = case_when(
    values >= 1.3e+11 ~ labels,
    values < 1.3e+11 ~ ""
  ))



###############################################################################



###############################################################################
### Chart 1 Sunburst chart

layout(plot_ly(df_sunburst,
               ids = ~ids,
               labels = ~labels,
               parents = ~parents,
               values = ~values,
               type = 'sunburst',
               branchvalues = 'total'
), colorway = c("#009E73", "#0072B2", "#56B4E9" ,"#E69F00", "#F0E442" ))

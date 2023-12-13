
### We are trying to build a sunburst plot from our import export dataset
## the data needs to have the following structure ids, labels, parents, values
## values need to be the total of each ids e.g. Export all fish exported
df_Seafood <- Seafood_database_preliminary
df_Seafood$Year <-dmy(as.character(paste("01-01",df_Seafood$Year)))
## Start with import and exports

df_Seafood %>% group_by(ids = Commodity) %>% summarise(values = sum(na.omit(Volume)))
df_Seafood %>% group_by(parents = Commodity, ids = SpeciesType) %>% summarise(values = sum(na.omit(Volume)))
df_Seafood %>% group_by(Commodity, parents = SpeciesType, ids = Species) %>%summarise(values = sum(na.omit(Volume)))

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

## Species sums
df_sunburst.temp <- df_Seafood %>% 
  group_by(Commodity, SpeciesType, labels = as.character(Species)) %>% 
  summarise(values = sum(na.omit(Volume))) %>% 
  ungroup() %>% 
  mutate(parents = paste(Commodity," - ", SpeciesType),
         ids = paste(Commodity," - ", SpeciesType, " - ", labels)) %>% 
  select(ids, labels, parents, values)

df_sunburst <- rbind(df_sunburst,df_sunburst.temp)

### last one years (no calucation)
df_sunburst.temp <- df_Seafood %>% 
  select(Commodity, SpeciesType, Species, Year, Volume) %>% 
  mutate(parents = paste(Commodity," - ", SpeciesType, " - ", Species),
         ids = paste(Commodity," - ", SpeciesType, " - ", Species, " - ", Year),
         labels = lubridate::year(Year),
         values = Volume) %>% 
  select(ids, labels, parents, values)

df_sunburst <- rbind(df_sunburst,df_sunburst.temp)




plot_ly(df_sunburst,
        ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  type = 'sunburst',
  branchvalues = 'total'
)


layout(plot_ly(df_sunburst,
               ids = ~ids,
               labels = ~labels,
               parents = ~parents,
               values = ~values,
               type = 'sunburst',
               branchvalues = 'total'
), colorway = c("#009E73", "#56B4E9", "#F0E442" ,"#0072B2", "#E69F00" ))


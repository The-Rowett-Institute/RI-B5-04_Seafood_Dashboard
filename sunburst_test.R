
### We are trying to build a sunburst plot from our import export dataset
## the data needs to have the following structure ids, labels, parents, values
## values need to be the total of each ids e.g. Export all fish exported

## Start with import and exports

df_Seafood %>% group_by(ids = Commodity) %>% summarise(values = sum(Value))
df_Seafood %>% group_by(parents = Commodity, ids = SpeciesType) %>% summarise(values = sum(Value))
df_Seafood %>% group_by(Commodity, parents = SpeciesType, ids = Species) %>% summarise(values = sum(Value))

## Import & Export sums
df_sunburst <- df_Seafood %>% 
  group_by(ids = as.character(Commodity)) %>% 
  summarise(values = sum(Value)) %>% 
  mutate(labels = ids, parents ="")


## Species Type sums
df_sunburst.temp <-df_Seafood %>% 
  group_by(parents = as.character(Commodity), labels = as.character(SpeciesType)) %>% 
  summarise(values = sum(Value)) %>% 
  mutate(ids = paste(parents," - ", labels)) # to create unique ids


df_sunburst <- rbind(df_sunburst,df_sunburst.temp)

## Species sums
df_sunburst.temp <- df_Seafood %>% 
  group_by(Commodity, SpeciesType, labels = as.character(Species)) %>% 
  summarise(values = sum(Value)) %>% 
  ungroup() %>% 
  mutate(parents = paste(Commodity," - ", SpeciesType),
         ids = paste(Commodity," - ", SpeciesType, " - ", labels)) %>% 
  select(ids, labels, parents, values)

df_sunburst <- rbind(df_sunburst,df_sunburst.temp)

### last one years (no calucation)
df_sunburst.temp <- df_Seafood %>% 
  select(Commodity, SpeciesType, Species, Year, Value) %>% 
  mutate(parents = paste(Commodity," - ", SpeciesType, " - ", Species),
         ids = paste(Commodity," - ", SpeciesType, " - ", Species, " - ", Year),
         labels = lubridate::year(Year),
         values = Value) %>% 
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

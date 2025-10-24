library(tidyverse)

d = read_csv("C:/Users/wrjam/Dropbox/WorkDocs/Lab/UL/Classes/Habitat suitability modeling/LDWF Project 1 Biological Data_050319.csv")

f = read_csv("C:/Users/wrjam/Dropbox/WorkDocs/Lab/UL/Classes/Habitat suitability modeling/LDWF Project 2 Biological Data_050319.csv")

df = bind_rows(d,f) 

c = df |> 
  group_by(Gear_Desc, BASIN, latitude, longitude) |> 
  summarize(n = n_distinct(DATE),
            min = min(DATE, na.rm = T),
            max = max(DATE, na.rm = T)) |> 
  mutate(min = ymd(min),
         max = ymd(max),
         time = as.duration(interval(min, max)))

d = c |> 
  filter(n>400,
         max > ymd('2018-1-1'))

b = d |> group_by(Gear_Desc, BASIN) |> count()

ggplot(d, aes(longitude, latitude, color = BASIN))+
  geom_point()+
  facet_wrap(~Gear_Desc)
  scale_color_viridis(option = 'turbo')

sites = d |> 
  filter(Gear_Desc %in% c("16' flat otter trawl", "150' 2\" bar mono gill")) |> 
  select(BASIN, latitude, longitude, Gear_Desc)


dat = sites |> 
  left_join(df)

k = dat |> 
  filter(Scientific_Name != 'No Catch',
         t_num_m_description != 'Erroneous Entry') |> 
  group_by(STATION, DATE, BASIN, latitude, longitude, Gear_Desc, Scientific_Name, Common_Name) |> 
  summarize(n = sum(T_NUM),
            .groups = 'drop') |> 
  mutate(Date = ymd(DATE),
         Basin = if_else(BASIN == 'Terrebonne' & longitude < -91.5, 'Vermilion-Teche', BASIN),
         gear = if_else(Gear_Desc == "16' flat otter trawl", 'Otter trawl','Gillnet'))

ggplot(k, aes(longitude, latitude, color = STATION))+
  geom_point()+
  facet_wrap(~Basin)

fin = k |> 
  select(basin = Basin, date = Date, 
         lat = latitude, lon = longitude,
         gear, sci_name = Scientific_Name,
         species = Common_Name, n)

nn = unique(fin[,c('basin', 'lat','lon', 'gear')])

ggplot(nn, aes(lon, lat, color = basin))+
  geom_point(size = 5)+
  facet_wrap(~gear)

nn$station |> duplicated()

write_csv(fin, 'data/LDWF_trawl_gillnet.csv')
saveRDS(fin, 'data/LDWF_trawl_gillnet.rds')

g1 = fin |> 
  filter(basin == 'Calcasieu',
         gear == 'Otter trawl')

g2 = fin |> 
  filter(basin == 'Calcasieu',
         gear == 'Gillnet')

g3 = fin |> 
  filter(basin == 'Barataria',
         gear == 'Otter trawl')

g4 = fin |> 
  filter(basin == 'Barataria',
         gear == 'Gillnet')

g5 = fin |> 
  filter(basin == 'Pontchartrain',
         gear == 'Otter trawl')

g6 = fin |> 
  filter(basin == 'Pontchartrain',
         gear == 'Gillnet')

write_csv(g1, 'data/group1.csv')
write_csv(g2, 'data/group2.csv')
write_csv(g3, 'data/group3.csv')
write_csv(g4, 'data/group4.csv')
write_csv(g5, 'data/group5.csv')
write_csv(g6, 'data/group6.csv')

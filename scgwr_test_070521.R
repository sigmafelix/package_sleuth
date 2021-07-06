# Last revised 07/05/2021
library(pacman)
p_load(tidyverse, scgwr, sf)

# download
setwd('~')
dir.create('GeoDa')
download.file('https://geodacenter.github.io/data-and-lab/data/kingcounty.zip',
              'GeoDa/kingcounty.zip')
download.file('https://geodacenter.github.io/data-and-lab/data/election.zip',
              'GeoDa/election.zip')
unzip('GeoDa/kingcounty.zip', exdir = 'GeoDa')
unzip('GeoDa/election.zip', exdir = 'GeoDa')

# King county housing transaction example
king <- st_read('GeoDa/KingCounty HouseSales2015.shp') %>% 
  st_set_geometry(NULL) %>% 
  group_by(price, bedrooms, bathrooms, sqft__ving) %>% 
  summarize_at(.vars = vars(everything()),
               .funs = list(~.[1])) %>% 
  ungroup %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_transform(2163) %>% 
  bind_cols(data.frame(st_coordinates(.)))


king.s <- king %>% 
  sample_n(5000)
system.time(king_scgwr <- scgwr(king.s[,c('X', 'Y')] %>% st_set_geometry(NULL) %>% as.matrix, 
                                y = king.s$price, 
                                x = king.s[,4:11] %>% st_set_geometry(NULL) %>% as.matrix))


# Election example
elec <- st_read('GeoDa/County_election_2012_16.shp') %>% 
  st_transform(2163) %>% 
  st_centroid %>% 
  bind_cols(data.frame(st_coordinates(.)))
system.time(elec_scgwr <- scgwr(coords = elec[,c('X', 'Y')] %>% st_set_geometry(NULL) %>% as.matrix, 
                                y = elec[,'diff_2016'] %>% st_set_geometry(NULL) %>% as.matrix, 
                                x = elec[,13:62] %>% st_set_geometry(NULL) %>% as.matrix))


p_load(GWmodel)
system.time(elec_gwm <- gwr.basic(as.formula(str_c('diff_2016~', str_c(colnames(elec)[13:62], collapse = '+'))),
                                  data = st_centroid(elec) %>% as('Spatial'),
                                  bw = 100000, longlat = T))
            
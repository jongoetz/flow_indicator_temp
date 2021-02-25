

library(bcdata)
library(bcmaps)
library(dplyr)
library(sf)
library(stringr)


## Hydrometric Station Spatial layers
## ----

# Points for all hydrometric stations
hydrometric_stations <- bcdc_get_data('4c169515-6c41-4f6a-bd30-19a1f45cad1f')
# Upstream drainage basin areas for hydrometric stations
hydrometric_basins <- readRDS("WSC_basins_BC_polygon.rds") #https://open.canada.ca/data/en/dataset/0c121878-ac23-46f5-95df-eb9960753375


## Other Grouping Spatial Layers
### ----

# Hydrologoic Zones https://catalogue.data.gov.bc.ca/dataset/hydrology-hydrologic-zone-boundaries-of-british-columbia
hydrologic_zones <- hydrozones() 
# Freshwater Atlas Groups https://www2.gov.bc.ca/assets/gov/data/geographic/topography/fwa/fwa_user_guide.pdf
fwa_groups <- bcdc_get_data('51f20b1a-ab75-42de-809d-bf415a0f9c62')
# Natural Resource Regions
resource_regions <- nr_regions()
# Ecoprovince and Ecoregion https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/ecosystems/broad-ecosystem/an_introduction_to_the_ecoregions_of_british_columbia.pdf
eco_provinces <- ecoprovinces()
eco_regions <- ecoregions()
# Water Survey Canada Drainage Basin Areas (contains attributes for various sub drainages)
wsc_drainage_areas <- wsc_drainages()
wsc_subsub_drainages <- st_intersection(wsc_drainage_areas, bc_bound())
wsc_sub_drainages <- wsc_subsub_drainages %>% 
  group_by(SUB_DRAINAGE_AREA_CD, SUB_DRAINAGE_AREA_NAME) %>% 
  summarise(geometry = st_union(geometry))


## Create table of joined attributes
## ----

## Each station with each grouping, with point spatial geometry
stations_joined_all_attributes_spatial <- hydrometric_stations %>% 
  st_intersection(hydrologic_zones) %>%
  st_intersection(fwa_groups) %>%
  st_intersection(wsc_drainage_areas) %>%
  st_intersection(resource_regions) %>%
  st_intersection(eco_provinces) %>%
  st_intersection(eco_regions)
# Create spatial with just some of the attributes
stations_joined_spatial <- stations_joined_all_attributes_spatial %>% 
  mutate(STATION_NAME = str_to_title(STATION_NAME),
         FLOW_TYPE = str_to_title(FLOW_TYPE),
         STATION_OPERATING_STATUS = ifelse(STATION_OPERATING_STATUS == "ACTIVE-REALTIME", "ACTIVE", STATION_OPERATING_STATUS),
         STATION_OPERATING_STATUS = str_to_title(STATION_OPERATING_STATUS),
         HYDROLOGICZONE_NAME = str_to_title(HYDROLOGICZONE_NAME),
         WSC_SUB_DRAINAGE_AREA = paste0(SUB_DRAINAGE_AREA_NAME, " (", SUB_DRAINAGE_AREA_CD, ")"),
         WSC_SUBSUB_DRAINAGE_AREA = paste0(SUB_SUB_DRAINAGE_AREA_NAME, " (", SUB_SUB_DRAINAGE_AREA_CD, ")"),
         REGION_NAME = str_remove(REGION_NAME, " Natural Resource Region"),
         ECOPROVINCE_NAME = str_to_title(ECOPROVINCE_NAME),
         ECOREGION_NAME = str_to_title(ECOREGION_NAME)) %>% 
  select(STATION_NUMBER, # unique ID
         STATION_NAME, # unique name
         FLOW_TYPE, # regulated (controlled flow) or naturally flowing
         STATION_OPERATING_STATUS, # Discontinued or active (likely just using Active)
         HYDROLOGIC_ZONE = HYDROLOGICZONE_NAME, # hydrologic zones (potential grouping)
         FWA_WATERSHED_GROUP = WATERSHED_GROUP_ID, # FWA groups (potential grouping)
         WSC_SUBSUB_DRAINAGE_AREA, # WSC drainage areas, will mutate below (potential grouping)
         WSC_SUB_DRAINAGE_AREA, # WSC drainage areas, will mutate below (potential grouping)
         NR_REGION = REGION_NAME, # Natural resource region (potential grouping)
         ECOPROVINCE = ECOPROVINCE_NAME, # BC Ecoprovince and region (potential grouping)
         ECOREGION = ECOREGION_NAME) # BC Ecoprovince and region (potential grouping)
# Create a table without the spatial geometry
stations_joined_table <- st_drop_geometry(stations_joined_spatial)

# Save the table as a csv
#write.csv(stations_joined_table, "station_layers.csv", row.names = FALSE)


### Get upstream and downstream stations
### Requires several steps
### ---

# Make the dataframe of all stations to join later
station_basins <- st_drop_geometry(hydrometric_stations) %>% 
  select(STATION_NUMBER) 

# Intersect hydrometric points and upstream basin areas to get all basin area overlaying points
# and then remove duplicates (some stations didn't catch its own basin area)
stations_overlap_spatial <- st_intersection(hydrometric_stations,
                                            hydrometric_basins)
stations_overlap <- st_drop_geometry(stations_overlap_spatial) %>% 
  select(STATION_NUMBER, Station) %>% 
  mutate(DUPLICATE = ifelse(STATION_NUMBER == Station, TRUE, FALSE)) %>% 
  filter(!DUPLICATE) %>% select(-DUPLICATE)

# List the stations up and downstream of each station and merge into the station_basins df
down_stations <- stations_overlap %>% 
  group_by(STATION_NUMBER) %>% 
  summarise(DOWNSTREAM_STATIONS = list(unique(Station)))
up_stations <- stations_overlap %>% 
  group_by(Station) %>% 
  summarise(UPSTREAM_STATIONS = list(unique(STATION_NUMBER))) %>% 
  rename(STATION_NUMBER = Station)
station_basins <- left_join(station_basins, up_stations, by = "STATION_NUMBER") %>% 
  left_join(down_stations, by = "STATION_NUMBER")

# Join with joined_table
stations_joined_table2 <- left_join(stations_joined_table, station_basins, by = "STATION_NUMBER")

# Write the file with lists (and how to read it back in)
openxlsx::write.xlsx(stations_joined_table2, "station_layers.xlsx")
file <- openxlsx::read.xlsx("station_layers.xlsx") %>% 
  mutate(UPSTREAM_STATIONS = as.list(strsplit(UPSTREAM_STATIONS, ", ")),
         DOWNSTREAM_STATIONS = as.list(strsplit(DOWNSTREAM_STATIONS, ", ")))

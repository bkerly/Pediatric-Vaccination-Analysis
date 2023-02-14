
source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")
library(measurements)
library(geosphere)



# Load Data ---------------------------------------------------------------


tract_characteristics <- read_csv("data/Colorado_data_mr.csv") %>%
  select(-PERCENT_VAX_0_17)

bus_data <- read_csv("data/CDPHE Data/Bus Data Extract 9-14.csv") %>%
  filter(VaxType %!in% c("Flu","Monkey Pox")) %>%
  group_by(date1,`Bus ID`,UniqueID,ScheduleID,`Dose Per Hour`,`Start Time`,`End Time`,`Site Name`,`Site Address`) %>%
  summarize(Total_COVID_Doseas = sum(Actual)) %>%
  ungroup()

bus_location <- read_csv("data/CDPHE Data/Bus_address_geocode.csv") %>%
  filter(!is.na(long_mean),
         !is.na(lat_mean)) %>%
   sf::st_as_sf(coords = c("long_mean","lat_mean"),
                crs = 4326) %>%
  sf::st_transform("WGS84")

bus_data <- left_join(bus_data,bus_location)

tract_vax_rate_date <- read_csv("data/CDPHE Data/Predicted Tract Vaccination By Day.csv")

suppressMessages({
  geom_with_age <- get_acs(
    geography = "tract",
    state = "CO",
    variables = c(
      Under_18 = "B09001_001"
      
    ),
    year = 2017,
    geometry = TRUE
  ) %>%
    mutate(TractID = as.numeric(GEOID)) %>%
    mutate(Under_18 = estimate) %>%
    select(TractID, NAME,Under_18,geometry)%>%
    mutate(centroid = sf::st_centroid(geometry) %>%
             sf::st_transform("WGS84"))
    
}
) 
  
# Calculate Bus Tract Characteristics by 1, 10, and 20 miles ---------------

bus_tract_cross <- expand.grid(bus_location$`Location ID`,geom_with_age$TractID) %>%
  `colnames<-`(c("Location ID","TractID")) %>%
  left_join(bus_location %>%
              select(`Location ID`,geometry),
            by = "Location ID") %>%
  left_join(geom_with_age %>%
              select(TractID,centroid) %>%
              select(-geometry), 
            by = "TractID") %>%
  rename(bus_location = geometry.x,
         tract_poly = geometry.y)

# Going to do a pretty good but not great distance calculation

  
bus_tract_dist <- bus_tract_cross %>%
  mutate(distance_mi = 
           distHaversine(as(bus_location,"Spatial"),as(centroid,"Spatial"),
                         r = 3958.8
           )
  ) %>%
  group_by(`Location ID`) %>%
  mutate(minimum_dist = (distance_mi == min(distance_mi,na.rm=TRUE)),
         mi_1 = distance_mi <=8.1,
         mi_10 = distance_mi <= 17.8,
         mi_20 = distance_mi <= 20) %>%
  select(-centroid, -tract_poly,-bus_location) %>%
  ungroup()

write_csv(bus_tract_dist,"data/bus tract distance.csv")

tracts_per_bus <- bus_tract_dist%>%
  group_by(`Location ID`) %>%
  summarize(nearest_tract = list(TractID[minimum_dist==TRUE]),
            mi_1_tracts = list(TractID[mi_1==TRUE]),
            mi_10_tracts = list(TractID[mi_10==TRUE]),
            mi_20_tracts = list(TractID[mi_20==TRUE])) %>%
  rowwise() %>%
  mutate(mi_1_tract_count = length(mi_1_tracts)) %>%
  mutate(mi_1_tracts = case_when(
    mi_1_tract_count == 0 ~ list(nearest_tract),
    TRUE ~ list(mi_1_tracts)
  )
  )

saveRDS(tracts_per_bus,"data/bus nearest tracts.rds")




source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")
library(measurements)
library(geosphere)



# Load Data ---------------------------------------------------------------


tract_characteristics <- read_csv("data/Colorado_data_mr.csv") %>%
  select(-PERCENT_VAX_0_17)

bus_data <- read_csv("data/CDPHE Data/Bus Data Extract 9-14.csv") %>%
  filter(VaxType %!in% c("Flu","Monkey Pox")) %>%
  group_by(date1,`Bus ID`,ScheduleID,`Site Name`,`Site Address`) %>%
  summarize(Total_COVID_Doses = sum(Actual),
            duration = difftime(`End Time`,`Start Time`,units="hours") %>%
                         as.numeric(),
            doses_per_hour = Total_COVID_Doses/duration,
            date = mdy(date1),
            early = (`Start Time` < hms("8:00:00")),
            late = (`End Time` > hms("17:00:00"))) %>%
  ungroup() %>%
  unique()

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

Urban_Rural_by_Tract_Messy <- read_csv("data/Urban Rural by Tract Messy.csv",
                                       skip = 1)

Urban_Rural_by_Tract_Clean <- Urban_Rural_by_Tract_Messy %>%
  transmute(TractID = str_remove(as.character(Geography),"1400000US") %>%
              as.numeric(),
         urban = (`Total!!Urban` > 0)
  )

rm(Urban_Rural_by_Tract_Messy)

bus_nearest_tracts <- read_rds("data/bus nearest tracts.rds")


# Combine Data ------------------------------------------------------------

bus_relevant_tracts <- bus_nearest_tracts %>%
  mutate(nearest_tract = as.numeric(nearest_tract)) %>%
  left_join(Urban_Rural_by_Tract_Clean,by=c("nearest_tract" = "TractID")) %>%
  rowwise() %>%
  mutate(
    relevant_tracts = case_when(
      urban = TRUE ~ list(mi_1_tracts),
      urban = FALSE ~ list(mi_10_tracts)
    )
  ) %>%
  ungroup() %>%
  select(`Location ID`,urban,relevant_tracts,nearest_tract)

bus_tract_data <- data.frame()

for(i in 1:nrow(bus_relevant_tracts)){
  print(i)
  averaging_tracts <- bus_relevant_tracts[i,3] %>%
    unlist() %>%
    as.numeric()
  
  service_area_characteristics <- tract_characteristics %>%
    filter(TractID %in% averaging_tracts) %>%
    summarize(
      `Location ID` = bus_relevant_tracts[i,1] %>% as.character(),
      urban = bus_relevant_tracts[i,2],
      nearest_tract = bus_relevant_tracts[i,4] %>% as.numeric(),
      population_served = sum(TRACT_POP),
      population_served_018 = sum(Under_18),
      providers_tract = sum(providers_tract),
      HOUSING_UNITS = sum(HOUSING_UNITS),
      HOUSEHOLDS = sum(HOUSEHOLDS),
      
      SES_BELOW_POV = weighted.mean(SES_BELOW_POV,TRACT_POP,na.rm=TRUE),
      SES_UNEMPLOYED = weighted.mean(SES_UNEMPLOYED,TRACT_POP,na.rm=TRUE),
      SES_INCOME = weighted.mean(SES_INCOME,TRACT_POP,na.rm=TRUE),
      
      HHCOMP_AGE65 = weighted.mean(HHCOMP_AGE65,TRACT_POP,na.rm=TRUE),
      HHCOMP_AGE17 = weighted.mean(HHCOMP_AGE17,TRACT_POP,na.rm=TRUE),
      
      HHCOMP_DISABILITY = weighted.mean(HHCOMP_DISABILITY,TRACT_POP,na.rm=TRUE),
      HHCOMP_SING_PARENT = weighted.mean(HHCOMP_SING_PARENT,TRACT_POP,na.rm=TRUE),
      MINORITY_MINORITY = weighted.mean(MINORITY_MINORITY,TRACT_POP,na.rm=TRUE),
      MINORITY_NONENGLISH = weighted.mean(MINORITY_NONENGLISH,TRACT_POP,na.rm=TRUE),
      HOUSING_MOBILE = weighted.mean(HOUSING_MOBILE,TRACT_POP,na.rm=TRUE),
      HOUSING_CROWDED = weighted.mean(HOUSING_CROWDED,TRACT_POP,na.rm=TRUE),
      HOUSING_NO_VEHICLE = weighted.mean(HOUSING_NO_VEHICLE,TRACT_POP,na.rm=TRUE),
      HOUSING_GROUP_QUARTERS = weighted.mean(HOUSING_GROUP_QUARTERS,TRACT_POP,na.rm=TRUE),
      UNINSURED = weighted.mean(UNINSURED,TRACT_POP,na.rm=TRUE),
      MMR = weighted.mean(MMR,Under_18,na.rm=TRUE),
      HPV = weighted.mean(HPV,Under_18,na.rm=TRUE),
      PedsFlu = weighted.mean(PedsFlu,Under_18,na.rm=TRUE),
      AllFlu = weighted.mean(AllFlu,TRACT_POP,na.rm=TRUE),
      HSA_beds = weighted.mean(HSA_beds,TRACT_POP,na.rm=TRUE),
      pctBiden = weighted.mean(pctBiden,TRACT_POP,na.rm=TRUE),
      providers_county = weighted.mean(providers_county,TRACT_POP,na.rm=TRUE)
    ) 
  
  bus_tract_data <- bind_rows(service_area_characteristics,bus_tract_data)
  
}


# Add in date and locaton info --------------------------------------------

bus_data_for_analysis <- bus_data %>%
  select(date,
    `Bus ID`,`ScheduleID`,
    date,
    `Location ID`,`Site Name`,`Site Address`,
    duration,Total_COVID_Doses,doses_per_hour,
    early,late
  ) %>%
  left_join(bus_tract_data,by="Location ID") %>%
  left_join(tract_vax_rate_date,by=c(
    "date"="date",
    "nearest_tract"="TractID"
  ))

write_rds(bus_data_for_analysis,"data/bus_data_for_analysis.rds")


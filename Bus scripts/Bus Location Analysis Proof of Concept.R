

# Load libraries ----------------------------------------------------------


source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

library(units)
library(measurements)


# Load geo reference data -------------------------------------------------


county_tract_ids <- read_csv("data/county_tract_ids.csv",
                             show_col_types = FALSE)

all_hazard_region <- read_csv("data/CDPHE Data/AllHazardRegions.csv",
                              show_col_types = FALSE) 


options(tigris_use_cache = TRUE)

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
    select(TractID, NAME,Under_18,geometry)
}
)

predictions <- read_csv("data/model predictions.csv",
                        show_col_types = FALSE)

Pediatric_Vaccine_Index <- read_csv("data/Pediatric_Vaccine_Index.csv",
                                    show_col_types = FALSE) %>%
  left_join(county_tract_ids) %>%
  left_join(geom_with_age) %>%
  left_join(predictions)

Pediatric_Vaccine_Index$geometry <- sf::st_transform(Pediatric_Vaccine_Index$geometry,"WGS84") #attempting to look for a crs (I know there are none, but this is to check)
Pediatric_Vaccine_Index$centroid <- sf::st_centroid(Pediatric_Vaccine_Index$geometry)


# Load Bus Locations ------------------------------------------------------



# Add hypothetical bus locations ------------------------------------------

bus1 <- data.frame(lon = -105.5123209, lat = 37.435193
          ) %>%
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326) %>%
  sf::st_transform("WGS84") # Lu's Main Street Cafe 609 Main St Blanca CO 81123 USA

bus2 <- data.frame(lon = -108.486053, lat = 37.4764214
                   )%>%
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326) %>%
  sf::st_transform("WGS84") #Joe Rowell Park 18005 CO-145 Dolores CO 81323 USA

bus3 <- data.frame(lon = -107.006156, lat = 37.268723
          )%>%
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326) %>%
  sf::st_transform("WGS84") # Pagosa Springs Farmers Market 235 E Pagosa St. Pagosa Springs CO 81147 USA

bus4 <- data.frame(lon = -107.879641, lat = 37.276617
          )%>%
  sf::st_as_sf(coords = c("lon","lat"),
               crs = 4326) %>%
  sf::st_transform("WGS84") # Durango Pride Festival Buckley Park 1201 Main St Durango CO 81301 USA


# Now, for just the first bus, we'll see what's nearby. -------------------

Pediatric_Vaccine_Index %>%
  ggplot() +
  geom_sf(aes(geometry = centroid))+
  geom_sf(aes(geometry = bus1$geometry),color = "red",size = 5)+
  theme_void() 

bus1_dist <- Pediatric_Vaccine_Index %>%
  mutate(dist_to_bus = sf::st_distance(centroid,bus1[1]) %>%
           as.numeric() %>%
           conv_unit("m","mile"))

bus1_tracts <- if(min(bus1_dist$dist_to_bus) > 10){
  bus1_dist %>% 
    arrange(dist_to_bus) %>% 
    slice_head(n=1)
  }else{
  bus1_dist %>% 
    filter(dist_to_bus <= 10)
    }

# make it a function


bus_tract_finder <-function(busdf = bus1){

  
  bus_dist <- Pediatric_Vaccine_Index %>%
    mutate(dist_to_bus = sf::st_distance(centroid,busdf[1]) %>%
             as.numeric() %>%
             conv_unit("m","mile"))
  
  bus_tracts <- if(min(bus_dist$dist_to_bus) > 10){
    bus_dist %>% 
      arrange(dist_to_bus) %>% 
      slice_head(n=1)
  }else{
    bus_dist %>% 
      filter(dist_to_bus <= 10)
  }
  
  return(bus_tracts)
}

bus_tract_finder(bus4)

bus_tract_mapper <- function(busdf = bus1){
  bus_tracts_i <- bus_tract_finder(busdf)
  bus_tracts_i_tractid <- bus_tracts_i %>% select(TractID) %>% unlist()
  bus_tracts_i_geometry <- busdf
  
  Pediatric_Vaccine_Index %>%
    mutate(bus_tracts_i = case_when(
      TractID %in% bus_tracts_i_tractid ~ TRUE,
      TRUE ~ FALSE)
    ) %>%
    ggplot(aes(geometry = geometry,fill = bus_tracts_i)) +
    geom_sf()+
    geom_sf(aes(geometry = bus_tracts_i_geometry$geometry),color = "red",size = 5,shape = 18)+
    scale_fill_viridis(discrete = TRUE)+
    theme_nothing()+
    labs(title = "Bus Location and Census Tracts Serviced",
         subtitle = "")
}

bus_tract_mapper(bus1)

# Characterize Nearby Tracts ----------------------------------------------

bus4_tracts <- bus_tract_finder(bus4)



bus1_tracts %>%
  as.data.frame() %>%
  select(Belief_pct:SE_Disadv_pct,-geometry) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  mutate(name = factor(name,
                       levels = c(
                         "Belief_pct",
                         "Perc_Vuln_pct",
                         "HC_access_pct",
                         "SE_Disadv_pct"
                       ),
                       labels = c(
                         "Vaccine Belief",
                         "Perceived Vulnerability" ,
                         "Healthcare Access",
                         "Socioeconomic Advantage"
                       )
  )) %>%
  ggplot(aes(x=name,y=value)) +
  geom_boxplot(fill="#fde725",alpha=.6, width=.4)+
  scale_y_percent(limits = c(0,1))+
  coord_flip()+
  xlab("") +
  ylab("")+
  theme_bw()

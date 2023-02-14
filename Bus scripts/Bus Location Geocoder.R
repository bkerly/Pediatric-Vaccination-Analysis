

source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

Bus_data_extract <- read_csv("data/CDPHE Data/Bus Data Extract 9-14.csv")

#devtools::install_github("hrbrmstr/nominatim")
# https://github.com/hrbrmstr/nominatim


library(tidygeocoder)

# geocode(tribble(~address,
#                "2019 S Pennsylvania St, Denver, CO",
#                "6 Garden Center, Broomfield, CO, 80020"),
#         method = "osm",
#         address)

Bus_addresses <- Bus_data_extract %>%
  select(`Location ID`,`Site Name`,`Site Address`) %>%
  unique()

Bus_addresses_geocode_pass1 <- Bus_addresses %>%
  geocode(`Site Address`,
          method = "osm")

Bus_addresses_geocode_pass2 <- Bus_addresses %>%
  geocode(`Site Address`,
          method = "census")


# Join and fix ------------------------------------------------------------

long_lower_bound <- -109.060253
long_upper_bound <- -102.041524	

long_bound_check <- function(x){between(x,long_lower_bound,long_upper_bound)}

long_bound_filter <- function(x){case_when(long_bound_check(x) ~ x,
                                          TRUE ~ NA_real_)}

lat_lower_bound <- 36.992426
lat_upper_bound <- 41.003444

lat_bound_check <- function(x){between(x,lat_lower_bound,lat_upper_bound)}

lat_bound_filter <- function(x){case_when(lat_bound_check(x) ~ x,
                                          TRUE ~ NA_real_)}

Bus_addresses_geocode_pass1 <- Bus_addresses_geocode_pass1 %>%
  rename(lat_osm = lat_bound_filter,
         long_osm = long)

Bus_addresses_geocode_pass2 <- Bus_addresses_geocode_pass2 %>%
  rename(lat_census = lat,
         long_census = long)

Bus_address_geocode <- left_join(Bus_addresses_geocode_pass1,Bus_addresses_geocode_pass2) %>%
  # Filter out addresses outside of Colorado
  mutate(lat_osm = lat_bound_filter(lat_osm),
         lat_census = lat_bound_filter(lat_census),
         long_osm = long_bound_filter(long_osm),
         long_census = long_bound_filter(long_census)
         )%>% #/ mutate

  mutate(lat_similar = abs(lat_osm-lat_census) < 0.05,
         long_similar = abs(long_osm-long_census) < 0.05
  ) %>%
  rowwise() %>%
  mutate(lat_mean = case_when(
    is.na(lat_osm)&is.na(lat_census) ~ NA_real_,
    TRUE ~  mean(c(lat_osm,lat_census),na.rm=TRUE)),
    long_mean = case_when(
      is.na(long_osm)&is.na(long_census) ~ NA_real_,
      TRUE ~  mean(c(long_osm,long_census),na.rm=TRUE))
  ) %>%
  ungroup()

ggplot(Bus_address_geocode) +
  geom_point(aes(x=long_osm,y=lat_osm),alpha = 0.5,color = "red")+
  geom_point(aes(x=long_census,y=lat_census),alpha = 0.5,color = "green")+
  geom_point(aes(x=long_mean,y=lat_mean),color = "blue")

write_csv(Bus_address_geocode,"data/CDPHE Data/Bus_address_geocode.csv")

source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

library(DBI)
#con <- dbConnect(odbc::odbc(), "CDPHESQD03", timeout = 99)

# Set up server connection ------------------------------------------------
ciisr::connect(database = "covid_vaccine",
               dsn = "CDPHESQD03",
               server = "10.48.24.100")


# Get data from server ----------------------------------------------------
vaccine_data <- tbl(conn, dbplyr::in_schema("tab", "LPHA_Patients")) %>% 
  
  select(patient_id, 
         vaccination_date,
         dosage_num,
         vaccine_type, umbrella_vaccine_type, Vaccine_Specific_Valid_Dose_Number,
         clinic_id, clinic_desc, clinic_county, clinic_county_reports,
         age_at_1stvaccination, age_group_10, race_ethnicity,
         Invalidated_Dose, dose_type,
         CO_County) %>% # Choose the columns you want
  
  
  collect() %>% # Pull the data you selected down from the server
  
  distinct(patient_id,.keep_all = TRUE) 

patientid_censustract <- read_csv("data/CDPHE Data/patientid_censustract_2020_080322.csv",
                                  col_types = "nc") %>%
  mutate(GEOID = paste0("0",GEOID))

# Get census tract baseline data
tract_race <- tidycensus::get_decennial(
  geography = "tract",
  state = "CO",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

# Combine and clean -------------------------------------------

vaccine_data <- vaccine_data %>%
  mutate(race_ethnicity = recode(race_ethnicity,
                                 "American Indian or Alaskan Native - Non Hispanic" = "Native",
                                 "Asian - Non Hispanic" = "Asian",
                                 "Hispanic, All Races" = "Hispanic",
                                 "Multi Race - Non Hispanic" = "Other/Unknown",
                                 "Native Hawaiian or Other Pacific Islander - Non Hispanic" = "Native",
                                 "Other" = "Other/Unknown",
                                 "Unknown" = "Other/Unknown",
                                 "White - Non Hispanic" = "White",
                                 "Black or African American - Non Hispanic" = "Black"
  )
  ) %>%

  left_join(patientid_censustract) %>%
  
  # Divide into bus and nonbus
  mutate(bus = (str_detect(clinic_desc,"MOBILE COVID VAX")))


vaccine_data %>% 
  filter(bus) %>%
  summary()

vaccine_data %>%
  filter(bus) %>%
  group_by(race_ethnicity) %>%
  tally()%>%
  ggplot() +
  geom_bar(aes(x=race_ethnicity,y=n),
           stat="identity") +
  theme_covid()


vaccine_data %>% 
  filter(!bus) %>%
  summary()

vaccine_data %>%
  filter(!bus) %>%
  group_by(race_ethnicity) %>%
  tally() %>%
  ggplot() +
  geom_bar(aes(x=race_ethnicity,y=n),
           stat="identity") +
  theme_covid()


# Tractwise ---------------------------------------------------------------

vax_by_bus <- vaccine_data %>%
  group_by(bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("NonBus","Bus"))  %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

vax_by_bus

vax_by_bus_age <- vaccine_data %>%
  group_by(bus,age_at_1stvaccination) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("Age","NonBus","Bus"))  %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

ggplot(vax_by_bus_age)+
  geom_bar(aes(x=Age,y=pct_bus/100),
           stat = "identity")+
  theme_covid() +
  xlab("Age in Years")+
  ylab("")+
  labs(title = "Percent of Vaccianations Administered in MVU")+
  scale_y_continuous(labels = scales::percent)+
  xlim(0,90)

vax_by_re_bus <- vaccine_data %>%
  group_by(race_ethnicity,bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("RaceEthnicity","NonBus","Bus")) %>%
    rowwise() %>%
    mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
    ungroup()

vax_by_re_bus

ggplot(vax_by_re_bus)+
  geom_bar(aes(x=RaceEthnicity,y=pct_bus/100),
           stat = "identity")+
  theme_covid() +
  xlab("")+
  ylab("")+
  labs(title = "Percent of Vaccianations Administered in MVU",
         subtitle = "All Ages")+
  scale_y_continuous(labels = scales::percent)
  

vax_by_tract <- vaccine_data %>%
  group_by(GEOID,bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("GEOID","NonBus","Bus")) %>%
  left_join(tract_race,by="GEOID") %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

hist(vax_by_tract$pct_bus)

ggplot(vax_by_tract) +
  geom_sf(aes(fill = pct_bus,geometry = geometry))+
  theme_fivethirtyeight()+
  labs(title = "Percent of All Vaccinations Occuring in MVU")+
  scale_fill_viridis(limits = c(0, 15), oob = scales::squish)+
  xlab("")+
  ylab("")+
  theme(axis.text = element_blank(),line=element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggplot(vax_by_tract) +
  geom_sf(aes(fill = pct_bus,geometry = geometry))+
  theme_fivethirtyeight()+
  labs(title = "Percent of All Vaccinations Occuring in MVU")+
  scale_fill_viridis(limits = c(0, 15), oob = scales::squish)+
  xlab("")+
  ylab("")+
  theme(axis.text = element_blank(),line=element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
  ylim(39,40.5)+
  xlim(-106,-104)
  

# Tractwise ---------------------------------------------------------------

vax_by_bus_peds <- vaccine_data %>%
  filter(age_at_1stvaccination<=17) %>%
  group_by(bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("NonBus","Bus"))  %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

vax_by_bus_peds

vax_by_re_bus_peds <- vaccine_data %>%
  filter(age_at_1stvaccination<=17) %>%
  group_by(race_ethnicity,bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("RaceEthnicity","NonBus","Bus")) %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

vax_by_re_bus_peds

ggplot(vax_by_re_bus_peds)+
  geom_bar(aes(x=RaceEthnicity,y=pct_bus/100),
           stat = "identity")+
  theme_covid() +
  xlab("")+
  ylab("")+
  labs(title = "Percent of Vaccinations Administered in MVU",
       subtitle = "17 and Under")+
  scale_y_continuous(labels = scales::percent)


vax_by_tract_peds <- vaccine_data %>%
  filter(age_at_1stvaccination<=17) %>%
  group_by(GEOID,bus) %>%
  count() %>%
  pivot_wider(names_from  = bus,values_from = n) %>%
  `colnames<-`(c("GEOID","NonBus","Bus")) %>%
  left_join(tract_race,by="GEOID") %>%
  rowwise() %>%
  mutate(pct_bus = 100*Bus / (Bus+NonBus)) %>%
  ungroup()

hist(vax_by_tract_peds$pct_bus)

ggplot(vax_by_tract_peds) +
  geom_sf(aes(fill = pct_bus,geometry = geometry))+
  theme_fivethirtyeight()+
  labs(title = "Percent of Under 17 Vaccinations Administered in MVU")+
  scale_fill_viridis(limits = c(0, 50), oob = scales::squish)+
  xlab("")+
  ylab("")+
  theme(axis.text = element_blank(),line=element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggplot(vax_by_tract_peds) +
  geom_sf(aes(fill = pct_bus,geometry = geometry))+
  theme_fivethirtyeight()+
  labs(title = "Percent of Under 17 Vaccinations Administered in MVU")+
  scale_fill_viridis(limits = c(0, 15), oob = scales::squish)+
  xlab("")+
  ylab("")+
  theme(axis.text = element_blank(),line=element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
ylim(39,40.5)+
  xlim(-106,-104)



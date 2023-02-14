

# Get shapefiles for counties in Colorado ---------------------------------

COloradoCounties <- tigris::counties(state="CO")


# Get and Recode ACS Variables --------------------------------------------

# Get a master list of acs variables
acs5_concepts <- load_variables(year = "2019",dataset = "acs5") %>% 
  filter(geography %in% "county") %>%
  select(concept) %>%
  unique()

acs1_concepts<- load_variables(year = "2019",dataset = "acs1") %>% 
  select(concept) %>%
  unique()

# Import a dataframe of concepts that we thought might be interesting to look at
conceptChoiceDf <- read_csv("data/ACS Concepts for Pediatric Analysis - all acs concepts.csv") %>%
  filter(Use = TRUE)

# Filter the master list of acs variables for just the ones in the list of what we thougtht might be interesting 
selectedAcs5Concepts <- acs5_concepts %>% 
  filter(concept %in% conceptChoiceDf$Concept)

selectedAcs1Concepts <- acs1_concepts %>% 
  filter(concept %in% conceptChoiceDf$Concept)

# Import the ACS data (right now for just Adams)
acs5DataRaw <- get_acs(geography = "county",
        state = "CO",
        county = "Adams",
        variables = selectedAcs5Concepts$concept) 

acs1DataRaw <- get_acs(geography = "county",
                       state = "CO",
                       county = "Adams",
                       variables = selectedAcs1Concepts$concept) 

# Recode the ACS data
acsDataRecoded <- acsDataRaw %>%
  
  # Add in the variable information from the "acs5Concepts" dataframe
  left_join(acs5Concepts,by=c("variable" = "name")) %>%
  
  # Perform summary calculatons by county (identified both by GEOID and Name)
  group_by(GEOID,NAME) %>%
  
  # Summarize 
  summarize(
    popNotMobile = estimate[variable == "B07401_017"],
    popNotMobileUnder17 = estimate[variable == "B07401_018"] + estimate[variable == "B07401_019"],
    
    popPrivateVehicle = estimate[variable == "B08406_002"],
    popPublicTranspo = estimate[variable == "B08406_008"],
    popBicycle = estimate[variable == "B08406_014"],
    popWalked = estimate[variable == "B08406_015"],
    popWorkFromHome = estimate[variable == "B08406_017"],
    
    popShortCommute = estimate[variable == "B08412_002"] + estimate[variable == "B08412_003"] + estimate[variable == "B08412_004"],
    popLongCommute = estimate[variable == "B08412_001"] - popShortCommute,
    
    medianHouseholdIncome = estimate[variable == "B19013E_001"],
    
    
    
    
    ) #/ summarize

            
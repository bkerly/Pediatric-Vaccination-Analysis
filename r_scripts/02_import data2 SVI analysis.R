

# Get shapefiles for counties in Colorado ---------------------------------

ColoradoCountyShapes <- tigris::counties(state="CO") %>%
  mutate(FIPS = paste0(STATEFP,COUNTYFP))


# Get and Rename SVI Data --------------------------------------------
# Source: https://www.atsdr.cdc.gov/placeandhealth/svi/index.html

SVI_data <- read_csv("data/SVI/SVI2018_US_COUNTY.csv")

SVI_Four_Themes <- SVI_data %>%
  transmute(COUNTY = toupper(COUNTY),
            FIPS = FIPS,
            SES = SPL_THEME1,
            HOUSEHOLD_COMPOSITION = SPL_THEME2,
            MINORITY_STATUS = SPL_THEME3,
            HOUSING_TYPE_TRANSPO = SPL_THEME4) %>%
  filter(SES > 0,
         HOUSEHOLD_COMPOSITION > 0,
         MINORITY_STATUS > 0,
         HOUSING_TYPE_TRANSPO > 0) 

# Import election data -------------------------------------------------

pres_election_data <- read_csv("data/Presidential/countypres_2000-2020.csv")
 # from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

election_2020 <- pres_election_data %>%
  filter(year == 2020,
         office == "US PRESIDENT",
         candidate == "JOSEPH R BIDEN JR") %>%
  select(state,state_po,county_name,county_fips,
         candidatevotes,totalvotes) %>%
  mutate(BidenPct = candidatevotes / totalvotes) %>%
  transmute(FIPS = county_fips,
            BidenPct = BidenPct)

# Get vaccination rate data -----------------------------------------------

Colorado_County_Vax_Rate <- read_csv("data/county_and_age_group_uptodate.csv") %>%
  transmute(COUNTY = toupper(County),
            Total_12older_People = `12+ Population`,
            Total_12older_Vax_Pct = `Percent of 12+ Pop Fully Vaccinated - Resident`,
            Total_18older_People = `18+ Population`,
            Total_18older_Vax_Pct = `Percent of 18+ Pop Fully Vaccinated - Resident`) %>%
  
  mutate(Total_12older_Vaxed = Total_12older_People * Total_12older_Vax_Pct/100,
         Total_18older_Vaxed = Total_18older_People * Total_18older_Vax_Pct/100) %>%
  
  mutate(Total_1217_Vaxed = Total_18older_Vaxed - Total_12older_Vaxed,
         Total_1217_People = Total_18older_People-Total_12older_People) %>%
  
  mutate(Total_1217_Vax_Pct = 100*Total_1217_Vaxed/Total_1217_People)

Colorado_Vax_Rate_1217 = Colorado_County_Vax_Rate %>%
  select(COUNTY,Total_1217_Vax_Pct)




# Get national vax data ---------------------------------------------------------------
National_Vax_Data <- read_csv("data/COVID-19_Vaccinations_in_the_United_States_County.csv") %>%
  
  group_by(FIPS) %>%
  
  mutate(Date = mdy(Date)) %>%
  arrange(desc(Date)) %>%
  slice_head(n=1) %>%
  ungroup()

# Add in Hospital capacity Data -------------------------------------------

capacity_data <- read_csv("data/HSA/FIPS_HSA_capacity_data.csv") %>%
  group_by(FIPS,HSA_id,HSA_desc) %>%
  summarize(beds = sum(beds,na.rm=TRUE)) %>%
  left_join(National_Vax_Data %>% 
              select(FIPS,Census2019),
            by="FIPS") 

health_service_areas <- read_csv("data/HSA/HSA_FIPS_CW.csv")

HSA_capacity <- capacity_data %>%
  group_by(HSA_id,HSA_desc) %>%
  summarize(population = sum(Census2019,na.rm=TRUE),
            beds = sum(beds,na.rm=TRUE)) %>%
  mutate(HSA_beds_per_100K = 100000 * beds/population)

FIPS_capacity <- health_service_areas %>%
  left_join(HSA_capacity %>%
              select(HSA_id,HSA_beds_per_100K),by="HSA_id")


# Do vax rate math ----------------------------------------------------



National_Ped_Vax_Rate <- National_Vax_Data %>%

  # Select the interesting columns
  select(FIPS, Recip_County, Recip_State,
         
         Series_Complete_5Plus,
         Series_Complete_5PlusPop_Pct,
         
         Series_Complete_12Plus,
         Series_Complete_12PlusPop_Pct,
         
         Series_Complete_18Plus,
         Series_Complete_18PlusPop_Pct,
         
         Series_Complete_Yes,
         Series_Complete_Pop_Pct) %>%
  
  arrange(Recip_State) %>%
  
  # Calculate the population of each group
  mutate(Pop_5Plus = 100*Series_Complete_5Plus / Series_Complete_5PlusPop_Pct,
         
         Pop_12Plus = 100*Series_Complete_12Plus / Series_Complete_12PlusPop_Pct,
         
         Pop_18Plus = 100*Series_Complete_18Plus / Series_Complete_18PlusPop_Pct,
         
         Pop_Tot = 100*Series_Complete_Yes / Series_Complete_Pop_Pct) %>%
  
  mutate(PopGood1 = (Pop_Tot >= Pop_5Plus) & (Pop_5Plus >= Pop_12Plus) & (Pop_12Plus >= Pop_18Plus)) %>%
  
  mutate(Pop_512 = Pop_5Plus-Pop_12Plus,
         Pop_1218 = Pop_12Plus-Pop_18Plus,
         Pop_518 = Pop_5Plus-Pop_18Plus,
         
         Dose_512 = Series_Complete_5Plus-Series_Complete_12Plus,
         Dose_1218 = Series_Complete_12Plus-Series_Complete_18Plus,
         Dose_518 = Series_Complete_5Plus-Series_Complete_18Plus) %>%
  
  mutate(PopGood2 = (Pop_Tot >= Pop_518) & (Pop_518 >= Pop_1218) & (Pop_518 > Pop_512),
         DoseGood = (Series_Complete_Yes >= Dose_518) & (Dose_518 >= Dose_1218) & (Dose_518 >= Dose_512)) %>%
  
  filter(PopGood1,PopGood2,DoseGood,
        (Dose_512 >0)) %>%
  
  mutate(Pct_512= Dose_512/Pop_512,
         Pct_1218 = Dose_1218/Pop_1218,
         Pct_518 = Dose_518/Pop_518) %>%
  
  filter(Pct_512<1,
         Pct_1218 < 1,
         Pct_518 < 1,
         !(Series_Complete_5PlusPop_Pct == Series_Complete_12PlusPop_Pct)) %>%

  select(FIPS, Recip_County,Recip_State,Pop_Tot,
         Pct_512,Pct_1218,Pct_518,
         Pop_512,Pop_1218,Pop_518,
         Dose_512,Dose_1218,Dose_518) 




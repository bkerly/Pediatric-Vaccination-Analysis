# The purpose of this script is to find the SVI variables that are most important for our Colorado model.


SVI_data <- read_csv("data/SVI2018_US_COUNTY.csv")

SVI_Seventeen_Domains <- SVI_data %>%
  transmute(COUNTY = toupper(COUNTY),
            FIPS = FIPS,
            POP = E_TOTPOP,
            HOUSING_UNITS = E_HU,
            HOUSEHOLDS = E_HH,
            SES_BELOW_POV = EP_POV	,
            SES_UNEMPLOYED = EP_UNEMP,
            SES_INCOME = EP_PCI,
            SES_NO_HS = EP_NOHSDP,
            HHCOMP_AGE65 = EP_AGE65,
            HHCOMP_AGE17 = EP_AGE17,
            HHCOMP_DISABILITY = EP_DISABL,
            HHCOMP_SING_PARENT = EP_SNGPNT,
            MINORITY_MINORITY = EP_MINRTY,
            MINORITY_NONENGLISH = EP_LIMENG,
            HOUSING_MULTIUNIT = EP_MUNIT,
            HOUSING_MOBILE = EP_MOBILE,
            HOUSING_CROWDED = EP_CROWD,
            HOUSING_NO_VEHICLE = EP_NOVEH,
            HOUSING_GROUP_QUARTERS = EP_GROUPQ,
            UNINSURED = EP_UNINSUR,
            DAYTIME_POP = E_DAYPOP,
            SES = SPL_THEME1,
            HHCOMP = SPL_THEME2,
            MINORITY = SPL_THEME3,
            HOUSING = SPL_THEME4) %>%
  na_if(.,-999)


# Import election data -------------------------------------------------

pres_election_data <- read_csv("data/countypres_2000-2020.csv")
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


# Get national vax data ---------------------------------------------------------------
National_Vax_Data <- read_csv("data/COVID-19_Vaccinations_in_the_United_States_County.csv") %>%
  
  group_by(FIPS) %>%
  
  mutate(Date = mdy(Date)) %>%
  arrange(desc(Date)) %>%
  slice_head(n=1) %>%
  ungroup()



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


# Big merger --------------------------------------------------------------


model_data <- National_Ped_Vax_Rate %>%
  inner_join(SVI_Seventeen_Domains, by = "FIPS") %>%
  inner_join(election_2020,by="FIPS") 

model <- 
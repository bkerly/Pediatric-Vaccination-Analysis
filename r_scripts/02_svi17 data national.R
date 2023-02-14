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
         Dose_518 = Series_Complete_5Plus-Series_Complete_18Plus,
         Dose_18Plus = Series_Complete_18Plus,
         Dose_Tot = Series_Complete_Yes) %>%
  
  mutate(PopGood2 = (Pop_Tot >= Pop_518) & (Pop_518 >= Pop_1218) & (Pop_518 > Pop_512),
         DoseGood = (Series_Complete_Yes >= Dose_518) & (Dose_518 >= Dose_1218) & (Dose_518 >= Dose_512)) %>%
  
  filter(PopGood1,PopGood2,DoseGood,
         (Dose_512 >0)) %>%
  
  mutate(Pct_512= Dose_512/Pop_512,
         Pct_1218 = Dose_1218/Pop_1218,
         Pct_518 = Dose_518/Pop_518,
         Pct_18Plus= Series_Complete_18Plus/Pop_18Plus,
         Pct_All = Series_Complete_Yes/Pop_Tot) %>%
  
  filter(Pct_512<1,
         Pct_1218 < 1,
         Pct_518 < 1,
         Pct_18Plus < 1,
         Pct_All < 1,
         !(Series_Complete_5PlusPop_Pct == Series_Complete_12PlusPop_Pct)) %>%
  
  select(FIPS, Recip_County,Recip_State,
         Pct_512,Pct_1218,Pct_518,Pct_18Plus, Pct_All,
         Pop_512,Pop_1218,Pop_518, Pop_18Plus, Pop_Tot,
         Dose_512,Dose_1218,Dose_518,Dose_18Plus,Dose_Tot) 


# Merge data --------------------------------------------------------------

model_data <- National_Ped_Vax_Rate %>%
  inner_join(SVI_Seventeen_Domains, by = "FIPS") %>%
  inner_join(election_2020,by="FIPS") 


# Create model ------------------------------------------------------------

model_518 <- glm(
  Dose_518 ~ 
  SES_BELOW_POV  +
  SES_UNEMPLOYED  +
  SES_INCOME  +
  SES_NO_HS  +
  HHCOMP_AGE65  +
  HHCOMP_AGE17  +
  HHCOMP_DISABILITY  +
  HHCOMP_SING_PARENT  +
  MINORITY_MINORITY  +
  MINORITY_NONENGLISH  +
  HOUSING_MULTIUNIT  +
  HOUSING_MOBILE  +
  HOUSING_CROWDED  +
  HOUSING_NO_VEHICLE  +
  HOUSING_GROUP_QUARTERS  +
  UNINSURED  +
  DAYTIME_POP
  
  +

    Pct_18Plus
  + 
    
    BidenPct,
  
  offset = log(Pop_518),
  
  data = model_data,
  
  family = "poisson"
)

summary(model_518)


# Predict -----------------------------------------------------------------



Predictions <- model_data %>%
  mutate(Pred_518 = 
           exp(
             coef(model_518)[1] +
               coef(model_518)[2] * SES_BELOW_POV            +
               coef(model_518)[3] * SES_UNEMPLOYED          +
               coef(model_518)[4] * SES_INCOME               +
               coef(model_518)[5] * SES_NO_HS               + 
               coef(model_518)[6] * HHCOMP_AGE65            +
               coef(model_518)[7] * HHCOMP_AGE17            +
               coef(model_518)[8] * HHCOMP_DISABILITY       +
               coef(model_518)[9] * HHCOMP_SING_PARENT      +
               coef(model_518)[10] * MINORITY_MINORITY        +
               coef(model_518)[11] * MINORITY_NONENGLISH      +
               coef(model_518)[12] * HOUSING_MULTIUNIT       +
               coef(model_518)[13] * HOUSING_MOBILE           + 
               coef(model_518)[14] * HOUSING_CROWDED          +
               coef(model_518)[15] * HOUSING_NO_VEHICLE      +
               coef(model_518)[16] * HOUSING_GROUP_QUARTERS   +
               coef(model_518)[17] * UNINSURED               +
               coef(model_518)[18] * DAYTIME_POP              +
               coef(model_518)[19] * Pct_18Plus               +
               coef(model_518)[20] * BidenPct                 
             
             
           ), #/ exp
  ) %>% #/ mutate 
  mutate(Resid_518 = Pred_518 - Pct_518) %>%
  mutate(Colorado = (Recip_State == "CO")) 


# Graph -------------------------------------------------------------------


ggplot(Predictions,aes(x=Pct_518,y=Pred_518)) +
  geom_point(aes(color=BidenPct*100,size = Pop_Tot)) +
  geom_abline(slope = 1) +
  theme_economist_white() +
  xlab("Observed Value")+
  ylab("Predicted Value") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-18") +
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  scale_color_gradient(low = "red",
                       high = "blue",
                       name = "Biden 2020 Vote Share")+
  guides(size = "none")


# Graph Colorado ----------------------------------------------------------

MODELINGREGIONS<-read_csv("data/ModelingRegions.csv")

COPredictions <- Predictions %>% 
  filter(Colorado) %>%
  mutate(COUNTY = gsub(" County.*","",Recip_County) %>%
           toupper()) %>%
  left_join(MODELINGREGIONS,by="COUNTY") %>%
  mutate(Metro = (MODELING_REGION == "METRO")) 

find_hull518 <- function(df) df[chull(df$Pct_518, df$Pred_518), ]
hulls518 <- plyr::ddply(COPredictions, "MODELING_REGION", find_hull518)



g1<- ggplot(data = COPredictions,aes(x=Pct_518,y=Pred_518,color = MODELING_REGION,
                                     fill = MODELING_REGION)) +
  geom_abline(slope = 1,size = 2, linetype = "dotdash") +
  geom_polygon(data = hulls518,alpha = 0.25) +
  geom_point(aes(size = Pop_Tot)) +
  geom_text(aes(label = ifelse(Resid_518 > quantile(Resid_518, 0.95),
                               as.character(Recip_County),'')),
            hjust=0,vjust=0,color = "black") + 
  #theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  xlab("")+
  ylab("Predicted Value") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-18") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

g1

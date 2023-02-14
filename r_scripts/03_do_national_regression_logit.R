#National model

model_data <- National_Ped_Vax_Rate %>%
  inner_join(SVI_Four_Themes, by = "FIPS") %>%
  inner_join(election_2020,by="FIPS") %>%
  inner_join(FIPS_capacity,by="FIPS")


# Make a summary graph ----------------------------------------------------

model_data %>%
  ggplot()+
  theme_covid()+
  geom_histogram(aes(x=Pct_512),fill="darkgreen",alpha = 0.5,bins = 30)+
  geom_histogram(aes(x=Pct_1218),fill="darkorange",alpha = 0.5,bins = 30) +
  geom_vline(aes(xintercept = mean(Pct_512)),color="green")+
  geom_vline(aes(xintercept = mean(Pct_1218)),color = "darkorange")+
  xlab("Percent Vaccinated")+
  ylab("")+
  scale_x_continuous(labels = scales::percent,limits = c(0,NA))+
  labs(title = "Histogram Percent Vaccinated by Age Group and County",
       subtitle = "5-11 in Green, 12-17 in Orange",
       caption = "Mean shown in vertical line.")

glue("
     {round(mean(model_data$Pct_512),4)*100}% of 5-11 year olds are fully vaccinated.
     StDev = {round(sd(model_data$Pct_512),4)*100}%
     
     {round(mean(model_data$Pct_1218),4)*100}% of 12-17 year olds are fully vaccinated.
     StDev = {round(sd(model_data$Pct_1218),4)*100}%
     ")

# Multiple linear regression ------------------------------------------------

# Look for collinearity
model_data %>% select(-Recip_County, -Recip_State,-COUNTY,
                      -FIPS,
                      -Pct_518,
                      -starts_with("Pop"),
                      -starts_with("Dose"),
                      Pop_Tot,
                      -HSA_id,
                      -HSA_desc) %>% plot()


# Create your model
# Include counties as indicator variables
# Identify counties with sig coefficients
# Make reference county a big one, like Denver.
# Exp(estimate) is rate ratio, for the estimate of the county effect!
# Make a chloropleth of rate ratios (for Colorado)
# Maybe pick the median, compare the characteristics of top vs bottom 20th pcentile
# Chi-squared test (or logistic) for hypothetical new characteristics (politics, urban/rural, etc.)

# Run it twice, once adjusted for the SVI variables and once not, to see if the SVI adds anything.

model_512 <- glm(
  Dose_512 ~ 
    
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES 
  
  + 
    
    BidenPct  
    
  + 
  
   Pop_Tot
  
  + 
    HSA_beds_per_100K,
  
  offset = log(Pop_512),
  
  data = model_data,
  
  family = "poisson"
)


model_1218 <- glm(
  Dose_1218 ~ 
    
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
   + 
     HOUSING_TYPE_TRANSPO  
  + 
    SES
  
  + BidenPct 
  
  + 
    
    Pop_Tot
  
  + 
    HSA_beds_per_100K,
  
  data = model_data,
  offset = log(Pop_1218),
  family = "poisson"
)

model_518 <- glm(
  Dose_518 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES
  
  + BidenPct 
  
  + 
    
    Pop_Tot
  
  + 
    HSA_beds_per_100K,
  
  data = model_data,
  offset = log(Pop_518),
  family = "poisson"
)

# Look at your model
summary(model_512)

summary(model_1218)

summary(model_518)

# Plot diagnostics
# After :http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

# Check for linearity
# Ideally, the residual plot will show no fitted pattern. That is, the red line should be approximately horizontal at zero. The presence of a pattern may indicate a problem with some aspect of the linear model.
plot(model_512,1)
plot(model_1218,1)

# Check for homogenaity of variance
# This plot shows if residuals are spread equally along the ranges of predictors.
plot(model_512,3)
plot(model_1218,3)

# Check for normality of residuals
# The QQ plot of residuals can be used to visually check the normality assumption. The normal probability plot of residuals should approximately follow a straight line.
plot(model_512,2)
plot(model_1218,2)


# Check for outliers and high leerage points
# Outliers can be identified by examining the standardized residual (or studentized residual), which is the residual divided by its estimated standard error. Standardized residuals can be interpreted as the number of standard errors away from the regression line.
# 
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers (James et al. 2014).
plot(model_512,5)
plot(model_1218,5)

# Check for influential values
# Statisticians have developed a metric called Cook's distance to determine the influence of a value. This metric defines influence as a combination of leverage and residual size.
plot(model_512,4)
plot(model_1218,4)


# Goodness of fit ---------------------------------------------------------
# To deviance here is labelled as the 'residual deviance' by the glm function, and here is 1110.3. There are 1,000 observations, and our model has two parameters, so the degrees of freedom is 998, given by R as the residual df. To calculate the p-value for the deviance goodness of fit test we simply calculate the probability to the right of the deviance value for the chi-squared distribution on 998 degrees of freedom:

pchisq(model_512$deviance, df=model_512$df.residual, lower.tail=FALSE) %>% sprintf("%.10f",.)

pchisq(model_1218$deviance, df=model_1218$df.residual, lower.tail=FALSE) %>% sprintf("%.10f",.)


with(model_512, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(model_1218, cbind(res.deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# Look at just the colorado counties --------------------------------------

Predictions <- model_data %>%
  mutate(Pred_512 = 
           exp(
             
             coef(model_512)[1] +
             coef(model_512)[2] * HOUSEHOLD_COMPOSITION +
             coef(model_512)[3] * MINORITY_STATUS +
             coef(model_512)[4] * HOUSING_TYPE_TRANSPO +
             coef(model_512)[5] * SES + 
             coef(model_512)[6] * BidenPct +
             coef(model_512)[7] * Pop_Tot +
             coef(model_512)[8] * HSA_beds_per_100K 
               
            
           ), #/ exp
         Pred_1218 = 
           exp(
             
             coef(model_1218)[1] +
               coef(model_1218)[2] * HOUSEHOLD_COMPOSITION +
               coef(model_1218)[3] * MINORITY_STATUS +
               coef(model_1218)[4] * HOUSING_TYPE_TRANSPO +
               coef(model_1218)[5] * SES+ 
               coef(model_1218)[6] * BidenPct +
               coef(model_1218)[7] * Pop_Tot +
               coef(model_1218)[8] * HSA_beds_per_100K 
             
           ) #/ exp
  ) %>% #/ mutate 
  mutate(Resid_512 = Pred_512 - Pct_512,
         Resid_1218 = Pred_1218 - Pct_1218) %>%
  mutate(Colorado = (Recip_State == "CO")) 

ggplot(Predictions,aes(y=Pct_512,x=Pred_512)) +
  geom_point(aes(
          #color=BidenPct*100,
                 size = Pop_Tot)) +
  geom_abline(slope = 1) +
  theme_economist_white() +
  ylab("Observed Value")+
  xlab("Predicted Value") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-11") +
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  # scale_color_gradient(low = "red",
  #                      high = "blue",
  #                      name = "Biden 2020 Vote Share")+
  guides(size = "none")

ggplot(Predictions,aes(y=Pct_1218,x=Pred_1218)) +
  geom_point(aes(
    #color=BidenPct*100,
    size = Pop_Tot)) +  geom_abline(slope = 1) +
  theme_economist_white() +
  ylab("Observed Value")+
  xlab("Predicted Value") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 12-17")+
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  #scale_color_gradient(low = "red",
   #                    high = "blue",
   #                    name = "Biden 2020 Vote Share")+
  guides(size = "none")

# Colorado predictions ----------------------------------------------------
MODELINGREGIONS<-read_csv("data/ModelingRegions.csv")

COPredictions <- Predictions %>% 
  filter(Colorado) %>%
  mutate(COUNTY = gsub(" County.*","",Recip_County) %>%
           toupper()) %>%
  left_join(MODELINGREGIONS,by="COUNTY") %>%
  mutate(Metro = (MODELING_REGION == "METRO")) %>%
  filter(!is.na(HSA_beds_per_100K))
  
find_hull512 <- function(df) df[chull(df$Pct_512, df$Pred_512), ]
hulls512 <- plyr::ddply(COPredictions, "MODELING_REGION", find_hull512)



g1<- ggplot(data = COPredictions,aes(y=Pct_512,x=Pred_512,color = MODELING_REGION,
                              fill = MODELING_REGION)) +
  geom_abline(slope = 1,size = 2, linetype = "dotdash") +
  geom_polygon(data = hulls512,alpha = 0.25) +
   geom_point(size = 2) +
  # geom_text(aes(label = ifelse(Resid_512 > quantile(Resid_512, 0.95),
  #                             as.character(Recip_County),'')),
  #           hjust=0,vjust=0,color = "black") + 
  #theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  ylab("")+
  xlab("Predicted Vaccination Rate") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 5-11") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

g1

find_hull1218 <- function(df) df[chull(df$Pct_1218, df$Pred_1218), ]
hulls1218 <- plyr::ddply(COPredictions, "MODELING_REGION", find_hull1218)

g2<- ggplot(COPredictions %>% filter(Colorado),aes(y=Pct_1218,x=Pred_1218,
                                                   color = MODELING_REGION,
                                                   fill = MODELING_REGION)) +
  geom_abline(slope = 1,size = 2, linetype = "dotdash") +
  geom_polygon(data = hulls1218,alpha = 0.25) +
  geom_point(size = 2) +
  # geom_text(aes(label = ifelse(Resid_1218 > quantile(Resid_1218, 0.95),
  #                              as.character(Recip_County),'')),
  #           hjust=0,vjust=0,color = "black") + 
  #theme_economist_white() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) + 
  ylab("Observed Value")+
  xlab("Predicted Vaccination Rate") +
  labs(title = "Observed vs Predicted Percent Vaccination",
       subtitle = "Ages 12-17")+
  theme(legend.position = "bottom",
        legend.title = element_blank())

g2

  gridExtra::grid.arrange(g1,g2)
  

# Make a high residual table ----------------------------------------------

library(kableExtra)  
  
  percenter <- function(x){
    paste0(round(x*100,2),"%")
  }

COPredictions %>%
  select(Recip_County, MODELING_REGION, Pct_512, Pred_512, Pct_1218, Pred_1218, Resid_512, Resid_1218) %>%
  arrange(desc(Resid_512)) %>%
  mutate(across(Pct_512:Resid_1218,percenter)) %>%
  select(-Resid_1218,-Resid_512) %>%
  transmute(
    County = Recip_County,
    Region = gsub("\\b([a-z])", "\\U\\1", tolower(MODELING_REGION), perl=TRUE),
    `5-11 Vaccination Rate` = Pct_512,
    `5-11 Vaccination Rate (Predicted)` = Pred_512,
    `12-18 Vaccination Rate` = Pct_1218,
    `12-18 Vaccination Rate (Predicted)` = Pred_1218
  ) %>%
  head(10) %>%
  kable() %>%
  kable_styling(latex_options = "bootstrap")

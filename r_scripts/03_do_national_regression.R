# National Model

model_data <- National_Ped_Vax_Rate %>%
  inner_join(SVI_Four_Themes, by = "FIPS") 


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
                      -Pct_518) %>% plot()


# Create your model

model_512 <- lm(
  Pct_512 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
   + 
     HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data
)


model_1218 <- lm(
  Pct_1218 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
   + 
     HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data
)

model_518 <- lm(
  Pct_518 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data
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


# Plot residuals more pretty, in case that is useful
model.diag.metrics_512 <- broom::augment(model_512)
model.diag.metrics_1218 <- broom::augment(model_1218)


ggplot(model.diag.metrics_512, aes(x= Pct_512, y= .fitted)) +
  geom_point() +
  stat_smooth(method = lm, se = TRUE) +
  geom_segment(aes(xend = Pct_512, yend = .fitted), color = "red", size = 0.3)

pred_512 <- model.diag.metrics_512 %>%
  select(.fitted:.std.resid) %>%
  rename_with(~paste0(.,"_512"))

pred_1218 <- model.diag.metrics_1218 %>%
  select(.fitted:.std.resid) %>%
  rename_with(~paste0(.,"_1218"))


# Colorado predictions ----------------------------------------------------
CO_predictions <- model_data %>%
  bind_cols(pred_512) %>%
  bind_cols(pred_1218) %>%
  filter(Recip_State %in% "CO") 

CO_predictions %>%
  mutate(mean_512 = mean(Pct_512),
         mean_1218 = mean(Pct_1218)) %>%
  summarize(SSR_512 = sum(.resid_512^2),
         SSR_1218 = sum(.resid_1218^2),
         SST_512 = sum((Pct_512-mean_512)^2),
         SST_1218 = sum((Pct_1218-mean_1218)^2)) %>%
  mutate(Rsq_512 = SSR_512/SST_512,
         Rsq_1218 = SSR_1218/SST_1218) %>%
  mutate(adjRsq_512 = ((1-Rsq_512) * (nrow(model_data)-1)) / (nrow(model_data)-4-1),
         adjRsq_1218 = ((1-Rsq_1218) * (nrow(model_data)-1)) / (nrow(model_data)-4-1)
)

g_512<- CO_predictions %>%
  ggplot(aes(x=Pct_512,y=.fitted_512)) +
  geom_point() +
  geom_abline(intercept = 0) +
  theme_covid()+
  geom_text(aes(label= ifelse(.resid_512 > quantile(abs(.resid_512), 0.95),
                              Recip_County,""),hjust=0,vjust=0)) +
  scale_y_continuous(labels = scales::percent,limits = c(0,NA)) +
  scale_x_continuous(labels = scales::percent,limits = c(0,NA)) +
  theme(axis.title.y = ggplot2::element_text(angle= 90))+
  ylab("Predicted Percent Vaccinated")+
  xlab("Actual Percent Vaccinated")+
  labs(title = "Predicted vs Actual Percent Vaccinated, National Model",
       subtitle =  "Ages 5-11")

g_1218 <- CO_predictions %>%
  ggplot(aes(x=Pct_1218,y=.fitted_1218)) +
  geom_point() +
  geom_abline(intercept = 0) +
  theme_covid()+
  geom_text(aes(label= ifelse(.resid_1218 > quantile(abs(.resid_1218), 0.95),
                Recip_County,""),hjust=0,vjust=0)) +
  scale_y_continuous(labels = scales::percent,limits = c(0,NA))+
  scale_x_continuous(labels = scales::percent,limits = c(0,NA))+
  theme(axis.title.y = ggplot2::element_text(angle= 90))+
  ylab("Predicted Percent Vaccinated")+
  xlab("Actual Percent Vaccinated")+
  labs(
       subtitle =  "Ages 12-17",
       caption = glue::glue("Predictions based on national dataset of vaccination rates and CDC's Social Vulnerability Index"))

gridExtra::grid.arrange(g_512,g_1218)


# Colorado Model ----------------------------------------------------------

model_data_CO <- model_data %>% 
  filter(Recip_State == "CO")

model_512_CO <- lm(
  Pct_512 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data_CO
)


model_1218_CO  <- lm(
  Pct_1218 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data_CO
)

model_518_CO  <- lm(
  Pct_518 ~ 
    HOUSEHOLD_COMPOSITION 
  + 
    MINORITY_STATUS 
  + 
    HOUSING_TYPE_TRANSPO  
  + 
    SES,
  data = model_data_CO
)

glue("
     {round(mean(model_data_CO$Pct_512),4)*100}% of 5-11 year olds are fully vaccinated.
     StDev = {round(sd(model_data_CO$Pct_512),4)*100}%
     
     {round(mean(model_data_CO$Pct_1218),4)*100}% of 12-17 year olds are fully vaccinated.
     StDev = {round(sd(model_data_CO$Pct_1218),4)*100}%
     ")

summary(model_512_CO)

summary(model_1218_CO)

pred_512_CO <- broom::augment(model_512_CO) %>%
  select(.fitted:.std.resid) %>%
  rename_with(~paste0(.,"_512"))

pred_1218_CO <- broom::augment(model_1218_CO) %>%
  select(.fitted:.std.resid) %>%
  rename_with(~paste0(.,"_1218"))

CO_predictions_CO <- model_data_CO %>%
  bind_cols(pred_512_CO) %>%
  bind_cols(pred_1218_CO)

g_512_CO<- CO_predictions_CO %>%
  ggplot(aes(x=Pct_512,y=.fitted_512)) +
  geom_point() +
  geom_abline(intercept = 0) +
  theme_covid()+
  geom_text(aes(label= ifelse(.resid_512 > quantile(abs(.resid_512), 0.95),
                              Recip_County,""),hjust=0,vjust=0)) +
  scale_y_continuous(labels = scales::percent,limits = c(0,NA)) +
  scale_x_continuous(labels = scales::percent,limits = c(0,NA)) +
  theme(axis.title.y = ggplot2::element_text(angle= 90))+
  ylab("Predicted Percent Vaccinated")+
  xlab("Actual Percent Vaccinated")+
  labs(title = "Predicted vs Actual Percent Vaccinated, Colorado Model",
       subtitle =  "Ages 5-11")

g_1218_CO <- CO_predictions_CO %>%
  ggplot(aes(x=Pct_1218,y=.fitted_1218)) +
  geom_point() +
  geom_abline(intercept = 0) +
  theme_covid()+
  geom_text(aes(label= ifelse(.resid_1218 > quantile(abs(.resid_1218), 0.95),
                              Recip_County,""),hjust=0,vjust=0)) +
  scale_y_continuous(labels = scales::percent,limits = c(0,NA))+
  scale_x_continuous(labels = scales::percent,limits = c(0,NA))+
  theme(axis.title.y = ggplot2::element_text(angle= 90))+
  ylab("Predicted Percent Vaccinated")+
  xlab("Actual Percent Vaccinated")+
  labs(
    subtitle =  "Ages 12-17",
    caption = glue::glue("Predictions based on national dataset of vaccination rates and CDC's Social Vulnerability Index"))

gridExtra::grid.arrange(g_512_CO,g_1218_CO)

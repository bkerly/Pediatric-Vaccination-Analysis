# Load useful packages ----------------------------------------------------

# For opening and manipulating data
library(tidyverse)

# For making graphs look prettier
library(ggthemes)

# For reading dates efficiently
library(lubridate)

# For importing data from CSVs or Excel documents
library(readr)
library(readxl)

# For some extra regression diagnostics
library(broom)



# Load in data ------------------------------------------------------------

model_data <- SVI_Four_Themes %>%
  left_join(Vax_Rate_1217,by="COUNTY")

# Multiple linear regression ------------------------------------------------

# Look for collinearity
model_data %>% select(-COUNTY,-FIPS,-Total_1217_Vax_Pct) %>% plot()


# Create your model

model <- lm(
   Total_1217_Vax_Pct ~ 
     HOUSEHOLD_COMPOSITION 
    + 
      MINORITY_STATUS 
   # + 
   #   HOUSING_TYPE_TRANSPO  
    + 
     SES,
  data = model_data
)

# Look at your model
summary(model)

# Plot diagnostics
# After :http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

# Check for linearity
# Ideally, the residual plot will show no fitted pattern. That is, the red line should be approximately horizontal at zero. The presence of a pattern may indicate a problem with some aspect of the linear model.
plot(model,1)

  # Check for homogenaity of variance
# This plot shows if residuals are spread equally along the ranges of predictors.
plot(model,3)

# Check for normality of residuals
# The QQ plot of residuals can be used to visually check the normality assumption. The normal probability plot of residuals should approximately follow a straight line.
plot(model,2)

# Check for outliers and high leerage points
# Outliers can be identified by examining the standardized residual (or studentized residual), which is the residual divided by its estimated standard error. Standardized residuals can be interpreted as the number of standard errors away from the regression line.
# 
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers (James et al. 2014).
plot(model,5)

# Check for influential values
# Statisticians have developed a metric called Cook's distance to determine the influence of a value. This metric defines influence as a combination of leverage and residual size.
plot(model,4)


# Plot residuals more pretty, in case that is useful
model.diag.metrics <- broom::augment(model)

ggplot(model.diag.metrics, aes(x= Total_1217_Vax_Pct, y= .fitted)) +
  geom_point() +
  stat_smooth(method = lm, se = TRUE) +
  geom_segment(aes(xend = Total_1217_Vax_Pct, yend = .fitted), color = "red", size = 0.3)

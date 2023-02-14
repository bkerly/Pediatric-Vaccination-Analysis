source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

bus_data_for_analysis<- read_rds("data/bus_data_for_analysis.rds") %>%
  mutate(`Location ID` = toupper(`Location ID`)) %>%
  group_by(`Location ID`) %>%
  mutate(urban = as.logical(unlist(urban))) %>%
  mutate(repeat_location = date > min(date,na.rm=TRUE)) %>%
  ungroup() %>%
  unique()

bus_data_no_na <- bus_data_for_analysis %>%
  na.omit() %>%
  mutate(urban = as.logical(unlist(urban))) %>%
  filter(duration > 3)

formula <- doses_per_hour ~
  early +
  late +
  duration+
  urban +
  repeat_location + 
  population_served +
  providers_tract +
  SES_BELOW_POV +
  SES_UNEMPLOYED +
  SES_INCOME +
  HHCOMP_AGE65 +
  HHCOMP_AGE17 +
  HHCOMP_DISABILITY +
  HHCOMP_SING_PARENT +
  MINORITY_MINORITY +
  MINORITY_NONENGLISH +
  HOUSING_MOBILE +
  HOUSING_CROWDED +
  HOUSING_NO_VEHICLE +
  HOUSING_GROUP_QUARTERS +
  UNINSURED +
  MMR +
  HPV +
  PedsFlu +
  AllFlu +
  HSA_beds +
  (pctBiden>0.5) +
  Pred_PCT_Vax_All

full_lm <- lm(
  formula,
  data = bus_data_no_na
)

summary(full_lm)

step.model <- MASS::stepAIC(full_lm, direction = "forward", 
                      trace = FALSE)

summary(step.model)

rf_model <- randomForest::randomForest(
  formula,
  data=bus_data_no_na,
  importance = TRUE,
  ntree = 10000
)

library(rpart)
library(randomForest)
library(rpart.plot)

print(rf_model)

summary(rf_model)

#https://stats.stackexchange.com/questions/12605/measures-of-variable-importance-in-random-forests
importance(rf_model)

plot(rf_model)

importance(rf_model)

varImpPlot(rf_model,main = "Variable Importance, Random Forest Model",n.var = 10,type = 1)

tree <- rpart(formula, data=bus_data_for_analysis%>%
                filter(duration > 3))

rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

rsq.rpart(tree)

tmp <- printcp(tree)
rsq.val <- 1-tmp[,c(3,4)]  

tree2 <- rpart(
  formula ,
  
  data=bus_data_no_na,
   cp=.014
)

rpart.plot(tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE)


# Tree 3 is the one for me!
tree3 <- rpart(formula, data=bus_data_for_analysis%>%
                 mutate(urban = as.logical(unlist(urban))),
               maxdepth = 2,
               minbucket = 80)

rpart.plot(tree3, box.palette="RdBu", shadow.col="gray", nn=TRUE)

rsq.rpart(tree3)


# Operational analysis ----------------------------------------------------

formula2 <- doses_per_hour ~
  early +
  late +
  duration+
  repeat_location

full_lm2 <- lm(
  formula2,
  data = bus_data_no_na
)

summary(full_lm2)

# Tree 3 is the one for me!
tree4 <- rpart(formula2, data=bus_data_for_analysis%>%
                 mutate(urban = as.logical(unlist(urban))),
               maxdepth = 2,
               minbucket = 40)

rpart.plot(tree4, box.palette="RdBu", shadow.col="gray", nn=TRUE)

rsq.rpart(tree4)

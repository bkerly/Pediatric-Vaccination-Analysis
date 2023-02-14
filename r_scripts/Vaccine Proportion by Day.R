
source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

#Vaccine data used through 7/14/2022

end_date <- ymd("2022-7-14")

state_vaccine_metrics <- read_csv("data/CDPHE Data/State Vaccine metrics 9-14.csv")

cumulative_by_day <- state_vaccine_metrics %>%
  filter(category == "Administration",
         metric == "Cumulative Daily",
         type == "All COVID Vaccines") %>%
  mutate(proportion = value / max(value))

write_csv(cumulative_by_day,"data/CDPHE Data/vaccine_dose_proportion_by_day.csv")


# Impute daily tract vaccine rate -----------------------------------------

total_vax_tract <- read_csv("data/CDPHE Data/Tract COVID Vaccine Data.csv")

vax_tract_date <- expand.grid(cumulative_by_day$date,total_vax_tract$TractID) %>%
  `colnames<-`(c("date","TractID")) %>%
  left_join(total_vax_tract) %>%
  left_join(cumulative_by_day %>%
              select(date,proportion)) %>%
  mutate(Pred_PCT_Vax_All = proportion * PERCENT_VAX_ALL) %>%
  filter(date <= end_date) %>%
  select(TractID,date,Pred_PCT_Vax_All)

write_csv(vax_tract_date,"data/CDPHE Data/Predicted Tract Vaccination By Day.csv")

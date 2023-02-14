library(tidyverse)

hospital_info <- read_csv("data/CMS Data/Hospital_General_Information.csv")

capacity_data <- read_csv("data/CMS Data/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv") %>% 
  group_by(hospital_pk) %>%
  filter(collection_week == max(collection_week,na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(fips_code) &
         !is.na(total_beds_7_day_avg) &
           total_beds_7_day_avg > 0) %>%
  transmute(FIPS = fips_code,
            beds = total_beds_7_day_avg)

health_service_areas <- readxl::read_excel("data/Health.Service.Areas.xls") %>%
  rename(HSA_id = `HSA # (NCI Modified)`,
         HSA_desc = `Health Service Area (NCI Modified) Description`)

capacity_data<- left_join(capacity_data,health_service_areas,by="FIPS")

FIPS_capacity <- capacity_data %>% 
  group_by(FIPS, HSA_id, HSA_desc) %>%
  summarize(beds = sum(beds,na.rm = TRUE))

HSA_capacity <- capacity_data %>% 
  group_by(HSA_id, HSA_desc) %>%
  summarize(beds = sum(beds,na.rm = TRUE))

write_csv(capacity_data,"data/FIPS_HSA_capacity_data.csv")

write_csv(health_service_areas, "HSA_FIPS_CW.csv")

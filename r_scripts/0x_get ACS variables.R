# For variables, see
# https://www.census.gov/programs-surveys/acs/technical-documentation/code-lists.html
acs5_concepts <- load_variables(year = "2019",dataset = "acs5") %>% 
  filter(geography %in% "county") %>%
  select(concept) %>%
  unique()

acs1_concepts <- load_variables(year = "2019",dataset = "acs1") %>% 
  select(concept) %>%
  unique()

acs_concepts_combined <- bind_rows(acs5_concepts,acs1_concepts) %>%
  unique()

write_csv(acs_concepts_combined,"all acs concepts.csv")

# For variables, see
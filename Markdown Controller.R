# This is the master script for the pediatric vaccine report.
# The purpose of this script is to iteratively run a parameterized report of alerts by county. 


# Parameters --------------------------------------------------------------

# None

# Set up pseudo working directory -----------------------------------------

#Instead of manually setting your "pseudoWD" this just sets it using "here::here()" and then the rest of the script runs normally. It's better for portability!

pseudoWD <- here::here() # Sets the working directory to whereever "COPHS Master Controller.r" is. 


# This function just turns any relative path (in the source directory) into an absolute path.
file.Dir <- function(path){
  paste0(pseudoWD,"/",path)
}

# This function does the same thing, but for scripts.
source.Script <- function(path){
  source(paste0(pseudoWD,"/",path))
}

File.Mover <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

# LOAD LIBRARIES AND FUNCTIONS
# INPUTS: none
# OUTPUTS: Loads lots of packages and some custom functions Brian Erly likes (like "age", "%!in%", and "berra")
source.Script("r_scripts/01_libraries and functions.R")
source.Script("r_scripts/00_secrets.R")

# Pick out the alert counties
all_counties <- read_csv(
  file.Dir(
    "data/county_tract_ids.csv"
  )
) %>%
  select(COUNTY) %>%
  unique() %>%
  unlist()

all_hazard_region <- read_csv(
  file.Dir(
    "data/CDPHE Data/AllHazardRegions.csv"),
                              show_col_types = FALSE) %>%
  select(ALL_HAZARD_REGION) %>%
  unique() %>%
  unlist()

# Run the markdown script for each of those counties.
render_report = function(COUNTY,region,doc_title){
  OutputDir = paste0("Report - ", COUNTY, " Pediatric Vacccination - ",Sys.Date(),".pdf")
  
  rmarkdown::render(
    file.Dir("county_report.Rmd"), 
    params = list(
      county = COUNTY,
      county_is_region = region,
      doc_title = doc_title
    ),
    output_file = OutputDir
    
  )
  
  File.Mover(from = OutputDir,
             to = paste0("output/",
                         OutputDir))
  
}

# Test out Weld
render_report("WELD",region = FALSE, "WELD COUNTY Pediatric Vaccinaton Report")

for(i in 1:length(all_counties)){
  COUNTY = all_counties[i]
  
  print(
    paste0(
      "Running report for ",COUNTY, " County, number ",i," of ",length(all_counties),"."
    )
  )
  
  render_report(COUNTY = COUNTY,
                region = FALSE,
                doc_title = paste0(COUNTY, " COUNTY Pediatric Vaccination: ",Sys.Date(),".pdf")
  )
}

# Test out NORTHWEST
render_report("NORTHWEST",region = TRUE, "NORTHWEST ALL HAZARDS REGION Pediatric Vaccinaton Report")


for(i in 1:length(all_hazard_region)){
  REGION = all_hazard_region[i]
  
  print(
    paste0(
      "Running report for ",REGION, " County, number ",i," of ",length(all_hazard_region),"."
    )
  )
  
  render_report(COUNTY = REGION,
                region = TRUE,
                doc_title = paste0(REGION, " ALL HAZARDS REGION Pediatric Vaccination: ",Sys.Date(),".pdf")
  )
}


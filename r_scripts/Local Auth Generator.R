# Local Auth Generator

library(tidyverse)
library(redcapAPI)

# The survey link lives here:
#https://cdphe.redcap.state.co.us/surveys/?s=W37JXDXX3HTD8YPC


# Add local records -------------------------------------------------------



add_user <- function(user_base,new_name,new_user,new_password,new_permissions = "standard"){
  user_base %>%
    bind_rows(
      tibble::tibble(
        user = new_user,
        password = sapply(new_password,sodium::password_store),
        permissions = new_permissions,
        name = new_name
      ) 
    ) %>%
    return()
}

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("Berly", "Rseverson","Tpilonetti","Sobrien"),
  password = sapply(c("Sta.Mozarella", 
                      "Holy.Guac",
                      "Cheese&Crackers",
                      "Gnocchi"), sodium::password_store),
  permissions = c("admin", "admin","standard","standard"),
  name = c(
    "Brian Erly",
    "Rachel Severson",
    "Therese Pilonetti",
    "Shannon O'Brien"
  )
) %>%
  add_user(
    new_name = "Cara Bradbury",
    new_user = "cara@calpho.org",
    new_password = "HISTORY%bet"
  ) %>%
  add_user(
    new_name = "Grace Franklin",
    new_user = "gracef@sanmiguelcountyco.gov",
    new_password = "JUNIOR&dine"
  ) %>%
  add_user(
    new_name = "Meagan Hillman",
    new_user = "mhillman@prowerscounty.net",
    new_password = "PREMIUM#half"
  ) %>%
  add_user(
    new_name = "John Douglas",
    new_user = "jmdouglas@tchd.org",
    new_password = "LAUNCH@shift"
  ) %>%
  add_user(
    new_name = "Joni Reynolds",
    new_user = "jreynolds@gunnisoncounty.org",
    new_password = "HIKE!fling"
  ) %>%
  add_user(
    new_name = "Tom Gonzales",
    new_user = "tgonzales@larimer.org",
    new_password = "CAP?shock"
  ) %>% add_user(
    new_name = "Diana Herrero",
    new_user = "diana.herrero@state.co.us",
    new_password = "MODERATE.croissant"
  )


# Import redcap records ---------------------------------------------------

REDCapAPI <- "644CB8C4A20B4176412B0C39306FBAA4"

rcon <- redcapConnection(url = "https://cdphe.redcap.state.co.us/api/", token = REDCapAPI)

# may need to find and replace mutate and rename codes to dplyr::
RC_permission_request <- exportRecords(rcon = rcon) %>%
  #filter(access = TRUE) %>% # One day we can figure out how to do access requests and this will work good.
  transmute(
    name = user_name %>% as.character(),
    user = email %>% as.character(),
    password = sapply(password %>% as.character(),sodium::password_store),
    permissions = "standard"
  ) %>% 
  tibble()


# Bind it together --------------------------------------------------------

user_base_combined <- user_base %>% 
  bind_rows(RC_permission_request)

readr::write_rds(user_base_combined,"User Base.rds")

rm(user_base,RC_permission_request,user_base_combined)

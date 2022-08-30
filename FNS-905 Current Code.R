
#```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = T,
                      results = "hide")
#```

# 1 - Set Up

#```{r message=FALSE, warning=FALSE}
#rm(list = ls(all.names = T)) #clear all objects from environment.
options(max.print=100) # Limit output.
start.time <- Sys.time()   # Log start time.
options(scipen=999) # Prohibit use of scientific notation
gc() # Free up memory and report the memory usage.
curr_date <- as.numeric(format(Sys.Date(), "%Y%m%d")) # Format date as numeric
# date <- as.Date(date) # Use this code if date format is to be kept
library(lubridate)
library(janitor)
library(tidyverse)
library(openxlsx)
library(readr)
library(readxl)
library(naniar)
#```

# 2 - Data Import and Prep

## 2.1 - Merge Latitude & Longitude

#### When transitioning between school years, it may be necessary import both the new SY & the previous SY of Data1027 & Data1026.

#```{r message=FALSE, warning=FALSE}
# Import Previous submission, clean names, remove extraneous columns
#list <- file.info(list.files(
#  "P:/1_Internal/Reports/Summer/USDA 543a_FNS 905/2022_post COVID waivers",
#  full.names = T))
#View(list)

#filename <- rownames(list)[which.max(list$ctime)]

previous_submission <- read_excel("H:\\Documents\\USDA FNS 905 Submission 20220810.xlsx") %>%
  clean_names %>%
  select(-c(so_o:map_y)) %>%
  mutate(submission = "previous")

# Import both sheets of the Geocode Master List, clean names, rename long and lat to match FNS-905 Template, remove columns.
sso <- read_excel(
  "H:\\Documents\\Geocode Master List_3.xlsx",
  sheet = 1) %>%
  clean_names %>%
  dplyr::rename(soo_x = latitude, soo_y = longitude,
         state_site_id_number = site_number) %>% # Rename unique ID in order to merge()
  select(state_site_id_number, soo_x, soo_y)

sfsp <- read_excel(
  "H:\\Documents\\Geocode Master List_3.xlsx",
  sheet = 2) %>%
  clean_names %>%
  dplyr::rename(soo_x = latitude, soo_y = longitude,
         state_site_id_number = site_number) %>% # Rename unique ID in order to merge()
  select(state_site_id_number, soo_x, soo_y)

# Import Data1026 & Data1027, latin encoding, clean names, merge long and lat coordinates
data1026_sfsp <- read_csv(
  "H:/Downloads/Data1026 (76).csv", locale = locale(encoding = "Latin1")) %>%
  clean_names %>%
  select(site_name:state_sponsor_id_number, last_modified_date) %>%
  merge(sfsp, by = "state_site_id_number", all.x = T) %>%
  relocate(state_site_id_number, .before = state_sponsor_id_number)

data1027_sso <- read_csv(
  "H:/Downloads/Data1027 - 2022-08-15T085002.592.csv", locale = locale(encoding = "Latin1")) %>%
  rbind(read_csv(
    "H:/Downloads/Data1027 - 2022-08-15T084959.958.csv", locale = locale(encoding = "Latin1"))) %>%
  clean_names %>%
  select(site_name:state_sponsor_id_number, last_modified_date) %>%
  merge(sso, by = "state_site_id_number", all.x = T) %>%
  relocate(state_site_id_number, .before = state_sponsor_id_number)


nrow(data1026_sfsp)
View(data1027_sso)
# Note: When transitioning between school years, it may be necessary download
# both the new SY & the previous SY of Data1027 & Data1026. If the previous SY
# has sites with end dates that are in the future, check to see if those sites
# are in the current year's data. If not, both schools years should be imported.
# After importing both years, combine the data. For Example:

# data1026 <- rbind(data1026 (1), data1026 (2))
# data1027 <- rbind(data1027 (1), data1027 (2))
#```

## 2.2 - Combine 1026 & 1027

#```{r message=FALSE, warning=FALSE}
# Combine Data1026 & Data1027
all <- rbind(data1027_sso, data1026_sfsp)
#```

## 2.3 - Address Corrections

#```{r message=FALSE, warning=FALSE}
address_correct <- function(x, y, z) {
  a <- which(all$state_site_id_number == x) # Get location of row with specified site number
  b <- "site_name" # Get location of column site_name
  c <- y # Get location of column site_address1/site_city/site_zip
  print(all[a, b]) # Print school name for visual inspection
  print(paste("Old =", all[a, c])) # Print old address for visual inspection
  print(paste("New =", z)) # Print new address for visual inspection
  all[a, c] <- z # Modify address
  print(paste("Confirm change:", all[a, c] == z)) # Confirm change
  assign("all", all, envir = globalenv()) # Save object "all" to global environment
  print("")
}

address_correct("24-304", "site_address1", "54 E 100 S") # RICH JR HIGH/NRES
address_correct("21-704", "site_address1", "53 S 100 E") # NORTH SUMMIT HIGH
address_correct("21-110", "site_address1", "240 S BEACON DR") # NORTH SUMMIT ELEMENTARY
address_correct("21-304", "site_address1", "76 S 100 E") # NORTH SUMMIT MIDDLE
address_correct("30-740", "site_address1", "211 South Tooele Blvd") # BLUE PEAK HIGH
address_correct("Q8-1",   "site_address1", "24 Highway 98") # NAA TSIS' AAN COMMUNITY SCHOOL
address_correct("Q8-1",   "site_city",     "Tonalea") # NAA TSIS' AAN COMMUNITY SCHOOL
address_correct("Q8-1",   "site_zip",      86044) # NAA TSIS' AAN COMMUNITY SCHOOL
address_correct("03-140", "site_address1", "76785 W 11900 N") # GROUSE CREEK SCHOOL
address_correct("02-104", "site_address1", "510 North 650 East") # BELKNAP ELEMENTARY
address_correct("02-704", "site_address1", "195 E CENTER") # BEAVER HIGH
address_correct("05-154", "site_address1", "250 W 200 N") # WELLINGTON ELEMENTARY
address_correct("25-108", "site_address1", "Old Main Highway 191")  # BLUFF ELEMENTARY
#```

## 2.4 - Format Dates

#```{r message=FALSE, warning=FALSE}
# Format Dates
#all <- all %>%
#  mutate(end_date_mm_dd_yy   = mdy(end_date_mm_dd_yy),
#         start_date_mm_dd_yy = mdy(start_date_mm_dd_yy),
#         last_modified_date  = mdy(last_modified_date))
# Dates with Base R
all <- all %>%
  mutate(end_date_mm_dd_yy   = as.Date(format(end_date_mm_dd_yy), "%m/%d/%Y"),
         start_date_mm_dd_yy = as.Date(format(start_date_mm_dd_yy), "%m/%d/%Y"),
         last_modified_date  = as.Date(format(last_modified_date), "%m/%d/%Y"))
#```



# 3 - Filter Inactive Sites by Date

#```{r message=FALSE, warning=FALSE}
active <- all %>%
  filter(end_date_mm_dd_yy > Sys.Date() + 1) %>%
  filter(start_date_mm_dd_yy < Sys.Date() + 30)

View(active)


nrow(all)
nrow(active)
#```

# 4 - Edit Checks

## 4.1 - Duplicate Site Numbers

#```{r message=FALSE, warning=FALSE}




active <- active %>%
  group_by_all() %>%
  dplyr::mutate(duplicates = row_number()) %>%
  relocate(duplicates) %>%
  filter(duplicates == 1) %>%
  select(-duplicates) %>%
  ungroup()

duplicates <- active %>%
  mutate(a = duplicated(state_site_id_number, fromLast = T),
         b  = duplicated(state_site_id_number, fromLast = F),
         ab = paste(a, b))
duplicates <- duplicates %>%
  filter(ab != "FALSE FALSE")
duplicates <- duplicates %>%
  select(-c(last_modified_date, a, b, ab))

active.r <- active







#```

## 4.2 - Missing Latitude & Longitude

#### Write to a workbook list of sites with missing long and lat coordinates, get coordinates from Google, add to Geocode Master List_CURRENT.

#```{r message=FALSE, warning=FALSE}
# If NAs are present, save to file to add to Geocode Master List
NAs_1 <- active %>%
  select(site_program, sponsoring_organization,	state_sponsor_id_number,
         site_name,	state_site_id_number,	site_address1,	site_city,
         site_state,	site_zip, soo_x, soo_y)

write.xlsx(NAs_1, "H:/Documents/NAs_1.xlsx", overwrite=T)
#```

## 4.3 - Other Missing Values

#### Save a list of sites to workbook with NAs in other required fields.

#```{r message=FALSE, warning=FALSE}
# Identify NAs in the following columns

NAs_2 <- active %>%
  filter(is.na(site_address1) | is.na(site_zip) | is.na(site_city)
         | is.na(site_state) | is.na(sponsoring_organization)
         | is.na(state_site_id_number) | is.na(state_sponsor_id_number))

write.xlsx(NAs_2, "H:/Documents/NAs_2.xlsx", overwrite=T)

# Missingness Visualization
library(VIM)
active %>%
  select(site_address1, site_zip, site_city, site_state,
         sponsoring_organization, state_site_id_number,
         state_sponsor_id_number, soo_x, soo_y) %>%
  aggr(., prop=F, numbers=T)
#```

# 5 - Added & Removed

# Combine current and previous submissions and look for unique site numbers.Sites added are indicated by unique site numbers in the current submission, sites removed are indicated by unique site numbers in the previous submission.
# Filter unique site numbers by submission and save as separate data objects.

#```{r message=FALSE, warning=FALSE}
current_submission <- active %>%
  select(-last_modified_date) %>%
  mutate(submission = "current")

# Combine
added_removed <- rbind(previous_submission,current_submission)

# Identify unique values
unique <- added_removed %>%
  mutate(a  = duplicated(state_site_id_number, fromLast = T),
         b  = duplicated(state_site_id_number, fromLast = F),
         ab = paste(a, b)) %>%
  filter(ab == "FALSE FALSE")

added <- unique %>%
  select(site_program, sponsoring_organization, site_name,
         start_date_mm_dd_yy, end_date_mm_dd_yy, submission) %>%
  filter(submission == "current") %>%
  select(-submission)

removed <- unique %>%
  select(site_program, sponsoring_organization, site_name,
         start_date_mm_dd_yy, end_date_mm_dd_yy, submission)  %>%
  filter(submission == "previous")%>%
  select(-submission)
#```

## 5.1 - Edit Check

  #```{r message=FALSE, warning=FALSE}
# Make sure the number of sites corresponds with the number of sites added and removed
data.frame(c(
  paste(" ", nrow(previous_submission), "Previous"),
  paste("+", nrow(added), "Added"),
  paste("-", nrow(removed), "Removed"),
  paste("=", (nrow(previous_submission) + nrow(added) - nrow(removed))),
  paste(nrow(current_submission), "Current"))) %>%
  View()
#```

# 6 - Prepare to Save

#Add spaces to site numbers to prevent data from being converted to dates in Excel.
#Reformat column headers for viewing.

#```{r message=FALSE, warning=FALSE}
# Add spaces to site numbers to prevent them from turning into dates
all <- all %>%
  mutate(state_site_id_number = paste0(" ", state_site_id_number, " ")) %>%
  select(-last_modified_date)
active <- active %>%
  mutate(state_site_id_number = paste0(" ", state_site_id_number, " ")) %>%
  select(-last_modified_date)


#ctive$end_date_mm_dd_yy <- as.character(active$end_date_mm_dd_yy)
#all$end_date_mm_dd_yy <- as.character(all$end_date_mm_dd_yy)
#active$start_date_mm_dd_yy <- as.character(active$start_date_mm_dd_yy)
#all$start_date_mm_dd_yy <- as.character(all$start_date_mm_dd_yy)

# Column Names for Added / Removed Sheets
colnames(added) <- stringr::str_to_title(gsub("_", " ",colnames(added)))
colnames(removed) <- stringr::str_to_title(gsub("_", " ",colnames(removed)))

# Get the number of rows of each data frame and save it as a vector.
nrow_all<-as.vector(nrow(all))
nrow_active<-as.vector(nrow(active))
nrow_added<-as.vector(nrow(added))
nrow_removed<-as.vector(nrow(removed))
nrow_duplicates <-as.vector(nrow(duplicates))
#```

# 7 - Generate Report

## 7.1 - Create Workbook

#```{r message=FALSE, warning=FALSE}
# Create workbook
wb <- createWorkbook()
#```

## 7.2 - Add Worksheets

### 7.2.1 - Active Sites

### 7.2.2 - All Sites

### 7.2.3 - Sites Added

### 7.2.4 - Sites Removed

#```{r message=FALSE, warning=FALSE}
# Add Worksheets to Workbook and Name
addWorksheet(wb, paste0("Active Sites (", nrow_active, ")"))
addWorksheet(wb, paste0("All Sites (", nrow_all, ")"))
addWorksheet(wb, paste0("Added (", nrow_added, ")"))
addWorksheet(wb, paste0("Removed (", nrow_removed, ")"))
addWorksheet(wb, paste0("Duplicates (", nrow_duplicates, ")"))
#```

## 7.3 - Write Data

#```{r message=FALSE, warning=FALSE}
# Write Data to Worksheets
writeData(wb, sheet = paste0("Active Sites (", nrow_active, ")"), x = active)
writeData(wb, sheet = paste0("All Sites (", nrow_all, ")"), x = all)
writeData(wb, sheet = paste0("Added (", nrow_added, ")"), x = added)
writeData(wb, sheet = paste0("Removed (", nrow_removed, ")"), x = removed)
writeData(wb, sheet = paste0("Duplicates (", nrow_duplicates, ")"), x = duplicates)
#```

## 7.4 - Save Sheets to Workbook

#```{r message=FALSE, warning=FALSE}
# Save
saveWorkbook(wb, paste0("H:/Documents/USDA FNS 905 Submission ", curr_date, ".xlsx"), overwrite = T)
#```

# 8 - Operational Changes

#```{r message=FALSE, warning=FALSE}

active <- active.r
# Subset data
modifications <- active %>%
  filter(last_modified_date >= Sys.Date()-7)

modifications$last_modified_date <- as.character(modifications$last_modified_date)
# Save to P Drive
write.csv(modifications, paste0("P:/1_Internal/Contracts, MOUs, State Agreements, Purchase Requests (non-conference)/DOH SFSP Health Inspections/2022/FNS 905 Operational Changes ", curr_date, ".csv"),row.names=F, na="")
end.time <- Sys.time() # Stop Timer
duration <- end.time - start.time
duration
#```

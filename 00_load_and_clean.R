#==============================================================================#
#   Purpose: Read in Data; Review Data Structure                               #                                                      #
#==============================================================================#


# 1. Load required packages ----------------------------------------------------
pacman::p_load(tidyverse, janitor, stringi, rpivotTable, htmlwidgets, lubridate)

#==============================================================================#
# First Data Check:                                                            #
#ifelse(month(Sys.Date()) == 10 & day(Sys.Date()) == 1, stop("Today is 10/22, you need to update date filters before rendering the document"), "Good to go")

# Change names of files if the person downloading them has not done so manually [AJA]
todays_files <- list.files(path = "I:/data/", pattern = paste(Sys.Date()), full.names = TRUE) 
if (length(todays_files)>0){ # checking to see if date format has already been changed manually
  
  library(magrittr)
  new_names <- todays_files %>%
    gsub("export-", "", .) %>% 
    gsub("Inspection Manager", "Inspection_Manager", .) %>%
    gsub("Complaint Manager", "Complaint_Manager", .) %>%
    gsub("Plan Reviews", "Plan_Reviews", .) %>%
    gsub(paste0("-", lubridate::year(Sys.Date())), paste0("_", lubridate::year(Sys.Date())), .) %>%
    gsub(Sys.Date(), format(Sys.Date(), format = "%Y.%m.%d"), .)
  
  file.rename(from = todays_files, to = new_names) # rename files
  
}

# 1.1 Creating function for week of month - Saturday to Friday -----------------
week_of_month <- function(dt){
  dt=as.character(dt)
  Month1st=paste0(stri_datetime_fields(dt)$Year,"/",stri_datetime_fields(dt)$Month,"/",1)
  dayOf1st=stri_datetime_fields(Month1st)$DayOfWeek
  addDays=6-dayOf1st
  if(dayOf1st==7){addDays=6}
  firstFriday=stri_datetime_add(Month1st,addDays,units="days",tz='')
  diffFrom1st=as.numeric(as.Date(dt)-as.Date(as.character(firstFriday)))
  offSet=2
  if(diffFrom1st%%7==0){offSet=1}
  return(floor(diffFrom1st/7)+offSet)
}

#==============================================================================#

# 2. Read in files -------------------------------------------------------------
complaint_data <- read_csv(paste0("I:/data/Complaint_Manager_", gsub("-", ".", Sys.Date()), ".csv"), 
                           col_types = cols(`Date Received` = col_date(format = "%m/%d/%Y"), 
                                            `Investigate Within` = col_number(), 
                                            `Complaint Number` = col_number(), 
                                            `Site ID` = col_number()), na = "NA") %>%
  clean_names() %>%
  filter(date_received >= "2024-10-01" & date_received <= "2025-09-30") %>% 
  filter(!type %in% c("Smoking", "Game Rooms"))
  

inspection_data <- read_csv(paste0("I:/data/Inspection_Manager_", gsub("-", ".", Sys.Date()), ".csv"), 
                            col_types = cols(`Site ID` = col_number(), 
                                             `Inspection Date` = col_date(format = "%m/%d/%Y"), 
                                             Inventory = col_character(), `Next Routine Due` = col_date(format = "%m/%d/%Y"))) %>%
  clean_names() %>%
  filter(inspection_date >= "2024-10-01" & inspection_date <= "2025-09-30") 
  

plan_data <- read_csv(paste0("I:/data/Plan_Reviews_", gsub("-", ".", Sys.Date()), ".csv"), 
                      col_types = cols(`Date Received` = col_date(format = "%m/%d/%Y"))) %>%
  clean_names() %>%
  filter(date_received >= "2024-10-01" & date_received <= "2025-09-30") %>%
  filter(!status %in% c("Need/Awaiting Information", "Pending/Plans Received"))

#==============================================================================#
# Second Data Check:                                                            #
ifelse(exists('complaint_data'), "Good to go", stop("Complaints data file is missing. Re-download the data for today and try again."))
ifelse(exists('plan_data'), "Good to go", stop("Plan Reviews data file is missing. Re-download the data for today and try again."))
ifelse(exists('inspection_data'), "Good to go", stop("Inspection Manager data file is missing. Re-download the data for today and try again."))
#                                                                              #
#==============================================================================#


# 3. Create major categories ---------------------------------------------------
    ## Major categories should include: Food, Pool, Childcare, Mobile

#=================#
# INSPECTION DATA #
#=================#

food <- c("Food", "Temporary Food")

inspection_data <- inspection_data %>%
  mutate(inspection_type_major = case_when(
    program %in% food ~ "Food",
    program == "Childcare" ~ "Childcare",
    program == "Pool" ~ "Pool",
    program == "Mobile Food" ~ "Mobile",
    .default = NA
  )) %>%
  mutate(inspection_type_major = case_when(
    temp_program %in% food ~ "Food",
    .default = inspection_type_major
  )) 


#=================#
# COMPLAINT DATA  #
#=================#

### Vector complaints for FY 23-24 were all Food-related when manually reviewed                                                 ###
### but this assignment would need to be manually checked each fiscal year, currently                                           ###
### this script cannot be automated as it may inappropriately categorize some vectors into the "Food" category in the future    ###

complaint_data <- complaint_data %>%
  mutate(inspection_type_major = case_when(
    type %in% c("FBI", "Food", "Vector", "Nuisance") ~ "Food",
    type == "Pools" ~ "Pool",
    .default = NA
  ))


#=================#
#   PLAN DATA     #
#=================#

plan_data  <- plan_data %>%
  mutate(inspection_type_major = case_when(
    program %in% c("Food", "Mobile Food", "Temporary Food") ~ "Food",
    program == "Childcare" ~ "Childcare",
    program == "Pool" ~ "Pool",
    .default = NA
  )) %>%
  mutate(inspection_type_major = case_when(
    is.na(inspection_type_major) & establishment %in% c("RANDOM", "RANDOM BAR AND GRILL") ~ "Food",
    is.na(inspection_type_major) & establishment == "LODGE & RESORT" ~ "Food",
    is.na(inspection_type_major) & is.na(program) ~ "Unknown",
    .default = inspection_type_major
  ))


# 4. Create minor categories  --------------------------------------------------

    ## Minor categories for FOOD should include: 
        ## Routine, Follow-Up, Pre-Op, Emergency Response, Opening, 
        ## Complaints, Plan Reviews, Site Visits, Temps, & Other

#========================#
# INSPECTION DATA - FOOD #
#========================#

inspection_data <- inspection_data %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Food" & purpose %in% c("Routine", "Regular Inspection") ~ "Routine",
    inspection_type_major == "Food" & purpose == "Follow-Up" ~ "Follow-Up",
    inspection_type_major == "Food" & purpose == "Pre-Operational" ~ "Pre-Op",
    inspection_type_major == "Food" & purpose == "Emergency Response" ~ "Emergency Response",
    inspection_type_major == "Food" & purpose == "Opening" ~ "Opening",
    inspection_type_major == "Food" & purpose %in% c("Complaint", "Reported Illness") ~ "Complaints",
    inspection_type_major == "Food" & purpose == "Site Visit" ~ "Site Visit",
    inspection_type_major == "Food" & purpose %in% c("Other", "Out-of-Business", "Operating Without a Permit") ~ "Other",
    .default = NA
  )) %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Food" & program == "Temporary Food" ~ "Temps",
    inspection_type_major == "Food" & temp_program == "Temporary Food" ~ "Temps",
    .default = inspection_type_minor
  )) %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Food" & grepl("SCHOOL", permit_type) == TRUE ~ "School",
    .default = inspection_type_minor
  )) %>%
  mutate(inspection_type_minor = ifelse(inspection_type_major == "Food" & is.na(inspection_type_minor), "Unknown", inspection_type_minor))


    ## Minor categories for POOL should include:
        ## Routine, Follow-Up, Pre-Op, Emergency Response, Opening, 
        ## Complaints, Plan Reviews, Site Visits, & Other

#=========================#
# INSPECTION DATA - POOLS #
#=========================#

inspection_data <- inspection_data %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Pool" & purpose %in% c("Routine", "Regular Inspection") ~ "Routine",
    inspection_type_major == "Pool" & purpose == "Follow-Up" ~ "Follow-Up",
    inspection_type_major == "Pool" & purpose == "Pre-Operational" ~ "Pre-Op",
    inspection_type_major == "Pool" & purpose == "Emergency Response" ~ "Emergency Response",
    inspection_type_major == "Pool" & purpose == "Opening" ~ "Opening",
    inspection_type_major == "Pool" & purpose %in% c("Complaint", "Reported Illness") ~ "Complaints",
    inspection_type_major == "Pool" & purpose == "Site Visit" ~ "Site Visit",
    inspection_type_major == "Pool" & purpose %in% c("Other", "Out-of-Business", "Operating Without a Permit") ~ "Other",
    inspection_type_major == "Pool" & is.na(purpose) ~ "Unknown",
    .default = inspection_type_minor
  ))


    ## Minor categories for MOBILE should include:
        ## Opening, Pre-Op, Routine, Follow-Up, 
        ## Complaints, Site Visits, & Other

#==========================#
# INSPECTION DATA - MOBILE #
#==========================#

inspection_data <- inspection_data %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Mobile" & purpose %in% c("Routine", "Regular Inspection") ~ "Routine",
    inspection_type_major == "Mobile" & purpose == "Follow-Up" ~ "Follow-Up",
    inspection_type_major == "Mobile" & purpose == "Pre-Operational" ~ "Pre-Op",
    inspection_type_major == "Mobile" & purpose == "Emergency Response" ~ "Emergency Response",
    inspection_type_major == "Mobile" & purpose == "Opening" ~ "Opening",
    inspection_type_major == "Mobile" & purpose %in% c("Complaint", "Reported Illness") ~ "Complaints",
    inspection_type_major == "Mobile" & purpose == "Site Visit" ~ "Site Visit",
    inspection_type_major == "Mobile" & purpose %in% c("Other", "Out-of-Business", "Operating Without a Permit") ~ "Other",
    .default = inspection_type_minor
  ))


    ## Minor categories for CHILDCARE should include:
        ## Facility Routine, ISD Routine, Private Cafeteria Routine, & Other

#=============================#
# INSPECTION DATA - CHILDCARE #
#=============================#

inspection_data <- inspection_data %>%
  mutate(inspection_type_minor = case_when(
    inspection_type_major == "Childcare" & purpose  %in% c("Routine", "Regular Inspection") ~ "Routine",
    inspection_type_major == "Childcare" & is.na(purpose) ~ "Routine",
    inspection_type_major == "Childcare" & purpose %in% c("Out-of-Business", "Opening", "Pre-Operational", "Emergency Response", "Follow-Up", "Complaint", "Reported Illness", "Site Visit") ~ "Other",
    .default = inspection_type_minor
  ))


#=================#
# COMPLAINT DATA  #
#=================#

complaint_data <- complaint_data %>%
  mutate(inspection_type_minor = "Complaints")


#=================#
#   PLAN DATA     #
#=================#

plan_data <- plan_data %>%
  mutate(inspection_type_minor = "Plan Reviews")


# 5. Create month, week-of-month, and fiscal year columns for each df ----------

#=================#
# INSPECTION DATA #
#=================#

inspection_data <- inspection_data %>%
  rowwise()%>%
  mutate(month = month(inspection_date)) %>%
  #mutate(week_of_month = stri_datetime_fields(inspection_date)$WeekOfMonth) %>%
  mutate(week_of_month = week_of_month(inspection_date)) %>%
  mutate(fiscal_year = case_when(
    inspection_date >= "2023-10-01" & inspection_date <= "2024-09-30" ~ "2023-2024",
    inspection_date >= "2024-10-01" & inspection_date <= "2025-09-30" ~ "2024-2025",
    inspection_date >= "2025-10-01" & inspection_date <= "2026-09-30" ~ "2025-2026",
    inspection_date >= "2026-10-01" & inspection_date <= "2027-09-30" ~ "2026-2027",
    inspection_date >= "2027-10-01" & inspection_date <= "2028-09-30" ~ "2027-2028",
    inspection_date >= "2028-10-01" & inspection_date <= "2029-09-30" ~ "2028-2029",
    inspection_date >= "2029-10-01" & inspection_date <= "2030-09-30" ~ "2029-2030",
    .default = NA
  ))

#=================#
# COMPLAINT DATA  #
#=================#

complaint_data <- complaint_data %>%
  rowwise()%>%
  mutate(month = month(date_received)) %>%
  #mutate(week_of_month = stri_datetime_fields(date_received)$WeekOfMonth) %>%
  mutate(week_of_month=week_of_month(date_received))%>%
  mutate(fiscal_year = case_when(
    date_received >= "2023-10-01" & date_received <= "2024-09-30" ~ "2023-2024",
    date_received >= "2024-10-01" & date_received <= "2025-09-30" ~ "2024-2025",
    date_received >= "2025-10-01" & date_received <= "2026-09-30" ~ "2025-2026",
    date_received >= "2026-10-01" & date_received <= "2027-09-30" ~ "2026-2027",
    date_received >= "2027-10-01" & date_received <= "2028-09-30" ~ "2027-2028",
    date_received >= "2028-10-01" & date_received <= "2029-09-30" ~ "2028-2029",
    date_received >= "2029-10-01" & date_received <= "2030-09-30" ~ "2029-2030",
    .default = NA
  ))

#=================#
#   PLAN DATA     #
#=================#

plan_data <- plan_data %>%
  rowwise()%>%
  mutate(month = month(date_received)) %>%
  #mutate(week_of_month = stri_datetime_fields(date_received)$WeekOfMonth) %>%
  mutate(week_of_month = week_of_month(date_received)) %>%
  mutate(fiscal_year = case_when(
    date_received >= "2023-10-01" & date_received <= "2024-09-30" ~ "2023-2024",
    date_received >= "2024-10-01" & date_received <= "2025-09-30" ~ "2024-2025",
    date_received >= "2025-10-01" & date_received <= "2026-09-30" ~ "2025-2026",
    date_received >= "2026-10-01" & date_received <= "2027-09-30" ~ "2026-2027",
    date_received >= "2027-10-01" & date_received <= "2028-09-30" ~ "2027-2028",
    date_received >= "2028-10-01" & date_received <= "2029-09-30" ~ "2028-2029",
    date_received >= "2029-10-01" & date_received <= "2030-09-30" ~ "2029-2030",
    .default = NA
  ))

# 6. Fill blank "Employee" with "Unknown" --------------------------------------

#=================#
# INSPECTION DATA #
#=================#

inspection_data <- inspection_data %>%
  mutate(inspector = case_when(
    is.na(inspector) | inspector == "" ~ "Unknown",
    .default = inspector
  ))

#=================#
# COMPLAINT DATA  #
#=================#

complaint_data <- complaint_data %>%
  mutate(assigned_to = case_when(
    is.na(assigned_to) | assigned_to == "" ~ "Unknown",
    .default = assigned_to
  ))

#=================#
#   PLAN DATA     #
#=================#

plan_data <- plan_data %>%
  mutate(assigned_to = case_when(
    is.na(assigned_to) | assigned_to == "" ~ "Unknown",
    .default = assigned_to
  ))
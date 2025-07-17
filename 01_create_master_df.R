#==============================================================================#
#   Purpose: Combine 3 df into a master dataset                                #                                                  #
#==============================================================================#

# 1. Define "source" column for each individual dataset ------------------------

#=================#
# INSPECTION DATA #
#=================#

inspection_data <- inspection_data %>%
  mutate(source = "inspection data")

#=================#
# COMPLAINT DATA  #
#=================#

complaint_data <- complaint_data %>%
  mutate(source = "complaint data")

#=================#
#   PLAN DATA     #
#=================#

plan_data <- plan_data %>%
  mutate(source = "plan review data")


# 2. Narrow down columns to only those needed for tables -----------------------
    ## Required Columns include: employee,	inspection_type_major,	
    ## inspection_type_minor,	date,	month, week_of_month,	fiscal_year,	source
          ### Rename columns as necessary ###

#=================#
# INSPECTION DATA #
#=================#

inspection_data <- inspection_data %>%
  rename(
    "employee" = "inspector",
    "date" = "inspection_date"
  ) %>%
  select(c("employee", "inspection_type_major", "inspection_type_minor", "date", "month", "week_of_month", "fiscal_year", "source"))

#=================#
# COMPLAINT DATA  #
#=================#

complaint_data <- complaint_data %>%
  rename(
    "employee" = "assigned_to",
    "date" = "date_received"
  ) %>%
  select(c("employee", "inspection_type_major", "inspection_type_minor", "date", "month", "week_of_month", "fiscal_year", "source"))


#=================#
#   PLAN DATA     #
#=================#

plan_data <- plan_data %>%
  rename(
    "employee" = "assigned_to",
    "date" = "date_received"
  ) %>%
  select(c("employee", "inspection_type_major", "inspection_type_minor", "date", "month", "week_of_month", "fiscal_year", "source")) 


# 3. Combine into 1 master dataset ---------------------------------------------
master_eh_data <- rbind(inspection_data, complaint_data, plan_data) %>%
  rowwise()%>%
  mutate(month = lubridate::month(month, label=TRUE))

## change month order to match fiscal year
master_eh_data$month <- factor(master_eh_data$month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))


# 4. Write out master dataset --------------------------------------------------
write.csv(master_eh_data, file = "I:/data/master_eh_data.csv", row.names = FALSE)


# 5. Data checks for NA --------------------------------------------------------
ifelse(any(is.na(master_eh_data$employee)), "'employee' column contains missing values. Resolve this issue then try to re-render the report.", "Good to go")
ifelse(any(is.na(master_eh_data$inspection_type_major)), "'inspection_type_major' column contains missing values. Resolve this issue then try to re-render the report.", "Good to go")
ifelse(any(is.na(master_eh_data$inspection_type_minor)), "'inspection_type_minor' column contains missing values. Resolve this issue then try to re-render the report.", "Good to go")


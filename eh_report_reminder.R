library(dplyr)
# file.exists(paste0("I:/data/Inspection_Manager_", today, ".csv"))

setwd("I:/data")

eh_files <- list.files(pattern = "Inspection_Manager")

eh_files <- eh_files %>% gsub("Inspection_Manager_", "", .) %>% gsub(".csv", "", .)

today <- format(Sys.Date(), format = "%Y.%m.%d")

if (!today %in% eh_files){
    warning <- data.frame("Timestamp" = c(Sys.time()),"Statement" = c(paste0("File not found for daily Environmental Health metrics report. Please run before EOD.")), "RUser" = c(paste0(keyring::key_list("EpiTraxDB_Login")[1,2])), "Status" = c("Warning"))
    DSTR::POST_QB_Data(warning, tableId = "btq1n920y8w", userToken = Sys.getenv("QB_TOKEN"))
}


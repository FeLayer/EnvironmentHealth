library(taskscheduleR)
taskscheduleR::taskscheduler_create(taskname = "EH_Report_Reminder", rscript = "I:/eh_report_reminder.R", schedule = "DAILY", starttime = "11:35", startdate = format(Sys.Date(), "%m/%d/%Y"))


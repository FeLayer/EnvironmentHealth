#==============================================================================#
#   Purpose: Create visuals for EH Report                                      #                                                        #
#==============================================================================#

# 1. Load req. packages --------------------------------------------------------
pacman::p_load(echarts4r)

# 2. Define min and max date ---------------------------------------------------
min_date <- min(master_eh_data$date)
max_date <- max(master_eh_data$date)

# 3. Create color palettes -----------------------------------------------------
pal_4 <- c("#23513C", "#DEBC73", "#284E5F", "#803B21")
full_pal <- c("#2292D0","#284E5F", "#803B21", "#DEBC73","#23513C" , "#4C3113", "#A6E2BE", "#CA6A53", "#5D7E82", "#DCB59E")

# 4. Viz 1: Proportions of all inspections by inspector ------------------------
viz1 <- master_eh_data %>%
  group_by(employee) %>%
  mutate(count = n()) %>%
  arrange(count) %>%
  select(c(employee, count)) %>%
  distinct() %>%
  ungroup() %>%
  e_charts(employee) %>%
  e_pie(count, radius = c("50%", "70%")) %>%
  e_tooltip(formatter = htmlwidgets::JS("function(params){
        return `<strong>${params.name}</strong>
                                                    <br/>Total: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
        
  }
    ")) %>%
  e_title(text = paste0("All Inspections by Inspector ", min_date, " to ", max_date), left = "center") %>%
  e_legend(show = FALSE) %>%
  e_color(full_pal)


# 5. Viz 2: Proportions of all inspections by inspection type (major) ----------
viz2 <- master_eh_data %>%
  #filter(!is.na(inspection_type_major))%>%
  group_by(inspection_type_major) %>%
  mutate(count = n()) %>%
  select(c(inspection_type_major, count)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(count) %>%
  e_charts(inspection_type_major) %>%
  e_pie(count, radius = c("50%", "70%")) %>%
  e_tooltip(formatter = htmlwidgets::JS("function(params){
        return `<strong>${params.name}</strong>
                                                    <br/>Total: ${params.value}
                                                    <br/>Percent: ${params.percent}%`

  }
    ")) %>%
  e_title(text = paste0("All Inspections by Inspection Type (Major) ", min_date, " to ", max_date), left = "center") %>%
  e_legend(show = FALSE) %>%
  e_color(full_pal)

# 6. Viz 3: Proportions of all inspections by inspection type (minor) ----------
viz3 <- master_eh_data %>%
  group_by(inspection_type_minor) %>%
  mutate(count = n()) %>%
  arrange(count) %>%
  select(c(inspection_type_minor, count)) %>%
  distinct() %>%
  ungroup() %>%
  e_charts(inspection_type_minor) %>%
  e_pie(count, radius = c("50%", "70%")) %>%
  e_tooltip(formatter = htmlwidgets::JS("function(params){
        return `<strong>${params.name}</strong>
                                                    <br/>Total: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
        
  }
    ")) %>%
  e_title(text = paste0("All Inspections by Inspection Type (Minor) ", min_date, " to ", max_date), left = "center") %>%
  e_legend(show = FALSE)  %>%
  e_color(full_pal)

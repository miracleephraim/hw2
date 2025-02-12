
#package installation and loading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#loading dataset



# calculating total reports for each hopsital per year
hospital_report_counts <- final.hcris.data %>% 
    group_by(year,street) %>%
    summarise(report_count = n(), .groups = "drop") %>%
    arrange(desc(report_count))


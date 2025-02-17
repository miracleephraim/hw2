#
#
#
#
#
#
#
#
#
library(ggplot2)
library(dplyr)
library(MatchIt)
library(WeightIt)
library(sandwich)
library(lmtest)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
load("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/submission/hw_workspace.Rdata")
print(ls()) 

#
#
#
#
#| echo: false
ggplot(data = total_report_count, aes(x=year, y=total)) +
     geom_line()

#
#
#
#
#| echo: false
total_report_count <- hospital_report_counts %>%
     group_by(year) %>%
     summarise(total = n())

#
#
#
#
#| echo: false
ggplot(final_HCRIS_data_new, aes(x = factor(year), y = tot_charges)) +
  geom_violin(scale = 'width')

#
#
#
#
#| echo: false
ggplot(final_HCRIS_data_new, aes(as.factor(year), price)) +
geom_violin(scale = 'width')
#
#
#
#
#| echo: false
HCRIS_2012 %>%
filter(penalty > 0) %>%
group_by(year) %>%
summarize(avg_price_nopen = mean(price))

HCRIS_2012 %>%
filter(penalty == 0) %>%
group_by(year) %>%
summarize(avg_price_pen = mean(price))

#
#
#
#
#| echo: false
HCRIS_2012 %>%
group_by(bed_1) %>%
summarise(avg_price = mean(price))

avg_price2 <- HCRIS_2012 %>%
group_by(bed_2) %>%
summarise(avg_price = mean(price))

avg_price3 <- HCRIS_2012 %>%
group_by(bed_3) %>%
summarise(avg_price = mean(price))


avg_price4 <- HCRIS_2012 %>%
group_by(bed_4) %>%
summarise(avg_price = mean(price))

#
#
#
#
#| echo: falsew
print(match_iv_ate)
print(match_maha_ate)
print(ipw_ate)
print(lm_ate)

#
#
#
#
#
#
#

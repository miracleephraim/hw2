#
#
#
#
#
#
#
#
#
#
#| echo: false
#| label: tbl-plans
#| tbl-cap: All plan types in 2015
load("C:/Users/mirac/Documents/GitHub/econ470_ma/hw1/submission/hw_workspace.Rdata")
ggplot(data = total_report_count, aes(x=year, y=total)) +
     geom_line()

#
#
#
#

knitr::kable(unique_provider_nos, 
             col.names=c("Plan Type","Count"),
             format.args=list(big.mark=","), booktabs = TRUE)

#
#
#
#

ggplot(final_HCRIS_data_new, aes(x = factor(year), y = tot_charges)) +
  geom_violin(scale = 'width')

#
#
#
#
ggplot(final_HCRIS_data_new, aes(as.factor(year), price)) +
geom_violin(scale = 'width')
#
#
#
#

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

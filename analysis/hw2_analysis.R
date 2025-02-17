
#package installation and loading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
#loading dataset
final_HCRIS_data <- readRDS("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/data/HCRIS_Data.rds")


# calculating total reports for each hopsital per year
hospital_report_counts <- final_HCRIS_data %>% 
    group_by(year,street) %>%
    summarise(report_count = n(), .groups = "drop") %>%
    arrange(desc(report_count)) %>%
    filter(report_count > 1) %>%
    filter(street != "NA")

total_report_count <- hospital_report_counts %>%
     group_by(year) %>%
     summarise(total = n())

# graphing
ggplot(data = total_report_count, aes(x=year, y=total)) +
     geom_line()

# how many unique hospital ids exist after combining provider numbers
unique_provider_nos <- final_HCRIS_data %>%
group_by(provider_number) %>%
count(provider_number) %>%
summarize(total = n())
# total - 9,323

### violin_plot of total charges

quantile(final_HCRIS_data$tot_charges, c(.75, .90, .95, .99), na.rm=TRUE)

#filtering out outliers
final_HCRIS_data_new <- final_HCRIS_data %>%
    drop_na(tot_charges) %>%
    filter(0 < tot_charges) %>%
    filter(tot_charges < 3102118294) 
    
ggplot(final_HCRIS_data_new, aes(x = factor(year), y = tot_charges)) +
  geom_violin(scale = 'width')


### violin plots of prices
# calculating estimated prices
final_HCRIS_data <- final_HCRIS_data %>%
  mutate(
    discount_factor = 1 - tot_discounts / tot_charges,
    price_num = (ip_charges + icu_charges + ancillary_charges) * discount_factor - tot_mcare_payment,
    price_denom = tot_discharges - mcare_discharges,
    price = price_num / price_denom
  ) 

# filtering out outlier prices
quantile(final_HCRIS_data_new$price, c(0.75, .9, .95, .99), na.rm=TRUE)

final_HCRIS_data_new <- final_HCRIS_data_new %>%
    drop_na(price) %>%
    filter(0 < price) %>%
    filter(price < 25104.948) 

ggplot(final_HCRIS_data_new, aes(as.factor(year), price)) +
geom_violin(scale = 'width')

# filtering to 2012 + making penalty
final.hcris <- final_HCRIS_data %>% 
ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0))

table(HCRIS_2012$penalty)

# calculating ATE for penalized and non-penalized
HCRIS_2012 %>%
group_by(penalty) %>%
summarize(avg_price_nopen = mean(price))

# quartile making for bed size
summary(HCRIS_2012$beds)
sum(is.na(HCRIS_2012$beds))  # Count missing values

#making a concated variable for bed quartiles
HCRIS_2012 <- HCRIS_2012 %>%
    mutate(
        bed_quartile = case_when(
            beds > 0 & beds <= 91 ~ 1,
            beds > 91 & beds <= 167 ~ 2,
            beds > 167 & beds <= 293.5 ~ 3,
            beds > 293.5 ~ 4
        )
    )

# binary variable for quantiles
HCRIS_2012 <- HCRIS_2012 %>%
    mutate(
        bed_1 = ifelse((bed_quartile == 1),1,0),
        bed_2 = ifelse((bed_quartile == 2),1,0),
        bed_3 = ifelse((bed_quartile == 3),1,0),
        bed_4 = ifelse((bed_quartile == 4),1,0)
    )

table(HCRIS_2012$bed_quartile)

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

# calculating ATE
#install.packages(c("MatchIt", "WeightIt", "lmtest", "sandwich"))
library(MatchIt)
library(WeightIt)
library(sandwich)
library(lmtest)

# Create inverse variance weights for each bed_quartile
HCRIS_2012 <- HCRIS_2012 %>%
  group_by(bed_quartile) %>%
  mutate(inv_var = 1 / var(price)) %>%
  ungroup()

# Matching with inverse variance distance
match_iv <- matchit(penalty ~ bed_quartile,
                    method = "nearest",
                    distance = HCRIS_2012$inv_var,
                    data = HCRIS_2012)

# Estimate ATE
match_iv_ate <- summary(match_iv)$estimates["ATT"]

# Matching with Mahalanobis distance on bed_quartile
match_maha <- matchit(penalty ~ bed_quartile,
                      method = "nearest",
                      distance = "mahalanobis",
                      data = HCRIS_2012)

# Estimate ATE
match_maha_ate <- summary(match_maha)$estimates["ATT"]

# Estimate Propensity Scores using a logistic regression
weight_model <- weightit(penalty ~ bed_quartile, method = "ps", data = HCRIS_2012)

# Compute ATE using IPW
HCRIS_2012$weights <- weight_model$weights

ipw_model <- lm(price ~ penalty, weights = weights, data = HCRIS_2012)
ipw_ate <- coef(ipw_model)["penalty"]

# Fit the linear regression model
lm_model <- lm(price ~ penalty * factor(bed_quartile), data = HCRIS_2012)

# Extract ATE from the main treatment effect
lm_ate <- coef(lm_model)["penalty"]

save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/submission/hw_workspace.Rdata")


#package installation and loading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)
install.packages(c("MatchIt", "WeightIt", "lmtest", "sandwich", "Matching"))
library(MatchIt)
library(WeightIt)
library(sandwich)
library(lmtest)
library(tidyr)
library(knitr)

#loading dataset
final_HCRIS_data <- readRDS("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/data/HCRIS_Data.rds")

load("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/submission/hw_workspace.Rdata")

## QUESTION 1
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

## QUESTION 2

# how many unique hospital ids exist after combining provider numbers
unique_provider_nos <- final_HCRIS_data %>%
group_by(provider_number) %>%
count(provider_number) %>%
summarize(total = n())
# total - 9,323

tibble(unique_hospital_ids = nrow(unique_provider_nos))

### QUESTION 3 - violin_plot of total charges

quantile(final_HCRIS_data$tot_charges, c(0.01, .75, .90, .95, .99), na.rm=TRUE)

#filtering out outliers
final_HCRIS_data_new <- final_HCRIS_data %>%
    drop_na(tot_charges) %>%
    filter(1802430 < tot_charges) %>%
    filter(tot_charges < 3102118294) 
    
ggplot(final_HCRIS_data_new, aes(x = factor(year), y = tot_charges)) +
  geom_violin(scale = 'width') +
  scale_y_continuous(trans='log10')

## QUESTION 4

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
quantile(final_HCRIS_data$price, c(0.01, 0.75, .9, .95, .99), na.rm=TRUE)

final_HCRIS_data_new <- final_HCRIS_data %>%
    drop_na(price) %>%
    filter(984.6199 < price) %>%
    filter(price < 25104.948)

ggplot(final_HCRIS_data_new, aes(as.factor(year), price)) +
  geom_violin(scale = 'width') +
  scale_y_continuous(trans='log10')

# QUESTION 5 & 6

# filtering to 2012 + making penalty
hcris_2012 <- final_HCRIS_data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<

table(hcris_2012$penalty)

# calculating ATE for penalized and non-penalized
mean.pen <- round(mean(hcris_2012$price[which(hcris_2012$penalty==1)]),2)
mean.nopen <- round(mean(hcris_2012$price[which(hcris_2012$penalty==0)]),2)     

# quartile making for bed size
summary(hcris_2012$beds)
sum(is.na(hcris_2012$beds))  # Count missing values

#making a concated variable for bed quartiles
hcris_2012 <- hcris_2012 %>%
    mutate(
        bed_quartile = case_when(
            beds > 0 & beds <= 91 ~ 1,
            beds > 91 & beds <= 167 ~ 2,
            beds > 167 & beds <= 293.5 ~ 3,
            beds > 293.5 ~ 4
        )
    )

# binary variable for quantiles
hcris_2012 <- hcris_2012 %>%
    mutate(
        bed_1 = ifelse((bed_quartile == 1),1,0),
        bed_2 = ifelse((bed_quartile == 2),1,0),
        bed_3 = ifelse((bed_quartile == 3),1,0),
        bed_4 = ifelse((bed_quartile == 4),1,0)
    )

table(hcris_2012$bed_quartile)

hcris_2012 %>%
group_by(bed_1) %>%
summarise(avg_price = mean(price))

avg_price2 <- hcris_2012 %>%
group_by(bed_2) %>%
summarise(avg_price = mean(price))

avg_price3 <- hcris_2012 %>%
group_by(bed_3) %>%
summarise(avg_price = mean(price))

avg_price4 <- hcris_2012 %>%
group_by(bed_4) %>%
summarise(avg_price = mean(price))

# alt code price calculations

# Compute average price for each quartile group (inside and outside)
avg_price1 <- hcris_2012 %>%
  summarise(`Avg Price (In Group)` = mean(price[bed_1 == 1], na.rm = TRUE),
            `Avg Price (Not in Group)` = mean(price[bed_1 == 0], na.rm = TRUE)) %>%
  mutate(`Bed Quartile` = "bed_1")

avg_price2 <- hcris_2012 %>%
  summarise(`Avg Price (In Group)` = mean(price[bed_2 == 1], na.rm = TRUE),
            `Avg Price (Not in Group)` = mean(price[bed_2 == 0], na.rm = TRUE)) %>%
  mutate(`Bed Quartile` = "bed_2")

avg_price3 <- hcris_2012 %>%
  summarise(`Avg Price (In Group)` = mean(price[bed_3 == 1], na.rm = TRUE),
            `Avg Price (Not in Group)` = mean(price[bed_3 == 0], na.rm = TRUE)) %>%
  mutate(`Bed Quartile` = "bed_3")

avg_price4 <- hcris_2012 %>%
  summarise(`Avg Price (In Group)` = mean(price[bed_4 == 1], na.rm = TRUE),
            `Avg Price (Not in Group)` = mean(price[bed_4 == 0], na.rm = TRUE)) %>%
  mutate(`Bed Quartile` = "bed_4")

# Combine results into one table
final_table <- bind_rows(avg_price1, avg_price2, avg_price3, avg_price4) %>%
  relocate(`Bed Quartile`, `Avg Price (In Group)`, `Avg Price (Not in Group)`)  # Ensure correct order

# Display table
kable(final_table, caption = "Average Price by Bed Quartile Group")


# QUESTION 6 - calculating ATE

# Select relevant variables and filter missing values
lp.vars <- hcris_2012 %>%
  dplyr::select(beds, mcaid_discharges, penalty, ip_charges, 
         mcare_discharges, tot_mcare_payment, price, bed_1, bed_2, bed_3, bed_4, bed_quartile) %>%
  filter(complete.cases(.))

# Remove 'penalty' and 'price' columns
lp.covs <- lp.vars %>% dplyr::select(-penalty, -price)


# Ensure X is extracted from the filtered dataset (lp.vars)
m.nn.var2 <- Matching::Match(Y = lp.vars$price,  
                             Tr = lp.vars$penalty,  
                             X = as.matrix(lp.vars[, c("bed_1", "bed_2", "bed_3", "bed_4")]),  
                             M = 1,   
                             Weight = 2,  
                             estimand = "ATE")


# matching with mahalanobis
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")      

# inverse propensity score
logit.model <- glm(penalty ~ factor(bed_quartile) + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

# regression technique
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + bed_1 + bed_2 + bed_3 + bed_4 + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)

# compiling table
ate_nn_var2 <- m.nn.var2$est
ate_nn_md <- m.nn.md$est
ate_nn_ps <- m.nn.ps$est
ate_reg <- coef(summary(reg))["penaltyTRUE", "Estimate"]

# Compile results into a table
estimands_table <- tibble(
  Method = c("Inverse Variance Distance", "Mahalanobis Distance", "Inverse Propensity Weighting", "Simple Linear Regression"),
  ATE = c(ate_nn_var2, ate_nn_md, ate_nn_ps, ate_reg)
)

# Display the table
kable(estimands_table, caption = "Estimated Treatment Effects by Method")

save.image("C:/Users/mirac/Documents/GitHub/econ470_ma/hw2/submission/hw_workspace.Rdata")

rm(list=ls())
setwd("/home/torent/CIS 671/CIS 671 Project")
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidymodels)
library(GGally)

# Load datasets
support <- read_csv("support-to-help-to-quit-tobacco-use.csv")
adults_smoking <- read_csv("adults-smoking-2007-2018-change-new.csv") #was adults-smoking-2007-2018.csv
affordability <- read_csv("affordability-cigarettes-new.csv")
prevalence <- read_csv("daily-smoking-prevalence-bounds.csv")
taxes <- read_csv("taxes-as-share-of-cigarette-price.csv")
daily_sales_per_adult <- read_csv("sales-of-cigarettes-per-adult-per-day.csv")
gender_smoking <- read_csv("gender_smoking.csv")
ad_bans <- read_csv("enforcement-of-bans-on-tobacco-advertising.csv")
income <- read_csv("smoking-deaths-1990-2017.csv")
deaths <- read_csv("smoking-death-rate-1990-2017.csv")

# Possible independent variables
affordability <- affordability %>%
  mutate(Percent_of_GDP = .[[4]]) %>%
  select(-c(2,4:8)) %>%
  mutate(Relative_Increase_In_Price_Per_Year = as.numeric(sub("%", "", Relative_Increase_In_Price_Per_Year)))

support <- support %>%
  mutate(Support_for_Quitting = .[[4]]) %>%
  select(-c(2,4))

taxes <- taxes %>%
  mutate(Tax_Rate = .[[4]]) %>%
  select(-c(2,4))

income <- income %>%
  mutate(Income_Class = .[[5]], Deaths = .[[4]], Current_Deaths = .[[6]], Year = Year...3, Current_Year = Year...7) %>% 
  select(-c(2:7))
  
income <- income %>% select(-c("Current_Deaths", "Current_Year"))

deaths <- deaths %>%
  mutate(Death_Rate = .[[4]], Current_Death_Rate = .[[6]], Year = Year...3, Current_Year = Year...7) %>% 
  select(-c(2:7))

deaths <- deaths %>% select(-c("Current_Death_Rate", "Current_Year"))

# Possible dependent variables
adults_smoking <- adults_smoking %>%
  mutate(Percent_Smokers = .[[4]], Current_Percent_Smokers = .[[6]], Year = Year...3, Current_Year = Year...7) %>% 
select(-c(2:11)) %>%
mutate(Relative_Decline_Per_Year = as.numeric(sub("%", "", Relative_Decline_Per_Year)))

prevalence <- prevalence %>%
  mutate(Prevalence = .[[4]]) %>%
  select(-c(2,4))

daily_sales_per_adult <- daily_sales_per_adult %>%
  mutate(Daily_Sales_per_Adult = .[[4]]) %>%
  select(-c(2,4))

ad_bans <- ad_bans %>%
  mutate(Bans_on_Advertising = .[[4]]) %>%
  select(-c(2,4))

# Combine the dataframes
#smokingList <- list(adults_smoking, affordability, support, taxes)
smokingList <- list(adults_smoking, income)
smoking_income <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE

write.csv(smoking, "smoking.csv")

# testing
# smoking_aff_sales_full <- drop_na(smoking_aff_sales)

#Years Available
yrs_av_adults <- as.data.frame(unique(adults_smoking$Year)) #2000 - 2015 (5), 2018 - 2020
yrs_av_sales <- as.data.frame(unique(daily_sales_per_adult$Year)) #1875 - 2015
yrs_av_gen <- as.data.frame(unique(gender_smoking$Year)) #1980 - 2012
yrs_av_prev <- as.data.frame(unique(prevalence$Year)) #1980 - 2012

yrs_av_tax <- as.data.frame(unique(taxes$Year)) #2012, 2014
yrs_av_aff <- as.data.frame(unique(affordability$Year)) #2010 - 2020, even years
yrs_av_sup <- as.data.frame(unique(support$Year)) #2007 - 2018, even years
yrs_av_ban <- as.data.frame(unique(ad_bans$Year)) #2007 - 2018, even years

# daily_sales_per_adult, affordability: 
# 2767 obs. w/ NAs
# 62 obs. w/o NAs
# Years: 2010, 2012, 2014

#Distributions

adults_smoking %>% 
  ggplot(aes(x=Percent_Smokers)) + 
  geom_histogram(bins = 24) + 
  theme_bw()

deaths %>% 
  ggplot(aes(x=Death_Rate)) + 
  geom_histogram(bins = 24) + 
  theme_bw()

#Regression Test:

# Percent_of_GDP vs. Percent_Smokers
smokingList <- list(adults_smoking, affordability)
smoking_aff_percent <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE

ggplot(smoking_aff_percent, aes(Percent_of_GDP, Relative_Decline_Per_Year))+ # Was Percent_of_GDP, Percent_Smokers
  geom_point()
aff_model <- lm(Relative_Decline_Per_Year~Percent_of_GDP,data=smoking_aff_percent) # Was Percent_of_GDP ~ Percent_Smokers
summary(aff_model)

# Percent_Smokers = 22.94929 - 0.15174(Percent_of_GDP)
# R^2=.01286
# p-value=.00799
# R^2 value is too low

# Relative_Decline_Per_Year = 1.69450 + 0.04236(Percent_of_GDP)
# R^2=.04477
# p-value=.004617
# R^2 value is too low

# Bans_on_Advertising vs. Percent_Smokers
smokingList <- list(adults_smoking, ad_bans)
smoking_bans_percent <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE
smoking_bans_percent$Bans_on_Advertising <- as.character(smoking_bans_percent$Bans_on_Advertising) # Treat as character
#smoking_bans_percent$Bans_on_Advertising <- as.numeric(smoking_bans_percent$Bans_on_Advertising) # Don't treat as numeric

ggplot(smoking_bans_percent, aes(Bans_on_Advertising, Relative_Decline_Per_Year))+ # Was Percent_Smokers
  geom_point()
bans_model <- lm(Relative_Decline_Per_Year~Bans_on_Advertising,data=smoking_bans_percent) # Was Percent_Smokers
summary(bans_model)

# Percent_Smokers = 18.56207 + 7.16266(Bans_on_Advertising = "4"), p-value=4.37e-08
# Percent_Smokers = 18.56207 - 0.03751(Bans_on_Advertising = "5"), p-value=0.982
# R^2=.1124
# p-value=1.605e-09
# R^2 value is too low

# Relative_Decline_Per_Year = 2.2870 - .5670(Bans_on_Advertising = "4"), p-value=.00997
# Relative_Decline_Per_Year = 2.2870 - .1226(Bans_on_Advertising = "5"), p-value=.73618
# R^2=.03135
# p-value=.02897
# R^2 value is too low

# Support_For_Quitting vs. Percent_Smokers
smokingList <- list(support, adults_smoking)
smoking_support_percent <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE
smoking_support_percent$Support_for_Quitting <- as.character(smoking_support_percent$Support_for_Quitting) # Treat as character
#smoking_support_percent$Support_for_Quitting <- as.numeric(smoking_support_percent$Support_for_Quitting) # Don't treat as numeric

ggplot(smoking_support_percent, aes(Support_for_Quitting, Relative_Decline_Per_Year))+
  geom_point()
support_model <- lm(Relative_Decline_Per_Year~Support_for_Quitting,data=smoking_support_percent)
summary(support_model)

# Do not use with Percent_Smokers

# Relative_Decline_Per_Year = 2.6253 - .6691(Support_for_Quitting = "3"), p-value = 5.11e-13
# Relative_Decline_Per_Year = 2.6253 - .8711(Support_for_Quitting = "4"), p-value = .0717
# Relative_Decline_Per_Year = 2.6253 - .3880(Support_for_Quitting = "5"), p-value = .4120
# R^2=.02023
# p-value=.1004
# R^2 value is too low

#New Variable - Change_in_Percent_Smokers

# Risk Factors

# Income vs. Percent_Smokers
smokingList <- list(income, adults_smoking)
smoking_income <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE
smoking_income$Income_Class <- factor(smoking_income$Income_Class, levels = c("Low income", "Lower-middle income", "Upper-middle income", "High income")) # Treat as factor and define levels
relevel(smoking_income$Income_Class, ref = "Low income")

ggplot(smoking_income, aes(Income_Class, Percent_Smokers))+
  geom_point()
income_model <- lm(Percent_Smokers~Income_Class,data=smoking_income)
summary(income_model)

# Percent_Smokers = 23.0857 + 3.1453 (Income_Class = "Lower-middle income"), p-value = 0.00942
# Percent_Smokers = 23.0857 + 3.0376 (Income_Class = "Upper-middle income"), p-value = 0.01985
# Percent_Smokers = 23.0857 + 3.9692 (Income_Class = "High income"), p-value = 0.00146
# R^2 = 0.01342
# p-value = 0.008574
# R^2 value is too low, p-values are solid, use non-linear model?

# Use the log transformation
log_smoking_income <- smoking_income %>%
  mutate(Log_Percent_Smokers = log(Percent_Smokers))

log_income_model <- lm(Log_Percent_Smokers~Income_Class,data=log_smoking_income)
summary(log_income_model)

# Log_Percent_Smokers = 2.97357 + 0.17824 (Income_Class = "Lower-middle income"), p-value = 0.000818
# Log_Percent_Smokers = 2.97357 + 0.20149 (Income_Class = "Upper-middle income"), p-value = 0.000447
# Log_Percent_Smokers = 2.97357 + 0.20149 (Income_Class = "High income"), p-value = 2.47e-06
# R^2 = 0.03296
# p-value = 1.938e-05
# Better, but stil not great

# Year vs. Percent_Smokers
# smoking_income_new <- na.omit(smoking_income)
smoking_income$Year = as.numeric(smoking_income$Year) - 2000
ggplot(smoking_income, aes(Year, Percent_Smokers))+
  geom_point()
year_model <- lm(Percent_Smokers~Year,data=smoking_income)
summary(year_model)

# Percent_Smokers = 29.66779 - 0.48863(Year)
# R^2=0.08378
# p-value=2.2e-16
# Best R^2 so far (still not great)

# Year vs Deaths
# smokingList <- list(income, adults_smoking, deaths)
# smoking_income_deaths <- Reduce(function(x, y) merge(x, y), smokingList) #, all = TRUE

deaths$Year = as.numeric(deaths$Year) - 1990

ggplot(deaths, aes(Year, Death_Rate))+
  geom_point()
year_deaths_model <- lm(Death_Rate~Year,data=deaths)
summary(year_deaths_model)

# Percent_Smokers = 137.45827 - 1.79362(Year)
# R^2 =0.07613
# p-value = 2.2e-16
# R^2 value isn't terrible

# Use the log transformation
log_year_death <- deaths %>%
  mutate(Log_Death_Rate = log(Death_Rate))

log_year_death_model <- lm(Log_Death_Rate~Year,data=log_year_death)
summary(log_year_death_model)

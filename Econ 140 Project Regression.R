# =========================================================
# Title: Econ 140 Project Regression.R
# Author: Sofia Guo
# Date: 11/25/2018
# Description: R script to produce regressions for Econ 140 Term Project
# =========================================================

#load packages
library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(openxlsx)
library(tidyr)
library(broom)
library(fBasics)
library(stargazer)
library(lubridate)
library(outliers)
library(lmtest)
library(sandwich)
library(robust)
library(car)
library(modelr)
library(fRegression)
library(MASS)
library(ivpack)

#read in the afcars table
afcars <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\AFCARS Data.xlsx')


#rename columns to years

colnames(afcars) <- c('State',
                      2008,
                      2009,
                      2010,
                      2011,
                      2012)

#melt for years columns

afcars_melt <- melt(afcars, id.vars = "State")

#rename columns 
colnames(afcars_melt) <- c('State',
                           'Year',
                           'Exit.Rate')


#read in the wage table
wage_2008 <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Wage Data.xlsx', sheet = 1)
wage_2009 <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Wage Data.xlsx', sheet = 2)
wage_2010 <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Wage Data.xlsx', sheet = 3)
wage_2011 <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Wage Data.xlsx', sheet = 4)
wage_2012 <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Wage Data.xlsx', sheet = 5)
#bind into master table
wage <- rbind(wage_2008,
              wage_2009,
              wage_2010,
              wage_2011,
              wage_2012)

#filter for cw wage
wage_filtered <- wage %>%
  dplyr::filter(occ_titl == 'Child, Family, and School Social Workers' |
           occ_titl == "Child, family, and school social workers") %>%
  dplyr::select(c(1, 3:4))

#rename columns
colnames(wage_filtered) <- c('State',
                                  'Hourly.Wage',
                                  'Year')

#convert wage to numeric
wage_filtered$`Hourly.Wage` <- as.numeric(wage_filtered$`Hourly.Wage`)

#read in caseload data
caseload <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Caseload Data.xlsx', sheet = 1)

#read in poverty data
poverty <- read.xlsx('C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Poverty Data.xlsx', sheet = 1)

#bind all to a master data frame
master_wageexit <- merge(wage_filtered, afcars_melt, by=c("State", 'Year'), all = T)

master_wec <- merge(master_wageexit, caseload, by= c('State', 'Year'), all = T)

master_foster <- merge(master_wec, poverty, by=c('State', 'Year'), all = T)

master_foster$Year <- as.numeric(master_foster$Year)

write.xlsx(master_foster, 'C:\\Users\\sofia\\Box\\Cal (sofiaguo@berkeley.edu)\\2018-19\\Econ 140\\Term Project\\Project Data\\Master Data 2008-2011.xlsx')

# ================================================================================================

#run regression 

#create dummy variable
master_foster_post <- master_foster %>%
  mutate(Post = as.numeric(master_foster$Year > 2009))

State_FE <- lm(log(Exit.Rate) ~ Post + log(Caseload) + (Hourly.Wage) + (Poverty.Percent.Under.Age.18)  + factor(State) + factor(Year), data = master_foster_post)

State_FE_robust <- robust.se(State_FE)

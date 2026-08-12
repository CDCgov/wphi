##############################
#
# Psychometric Analyses (DIF)
#
# Code written by Ro Nadreau
#
##############################

# read in RANDS 11 data set (probability + non-probability) 
rands11 = read.csv()
	# n = 19,816 qualified completes

# load packages
library(tidyverse)
library(psych)
library(ggplot2)
library(lavaan)
library(lordif)

##### SELECT VARIABLES

vars = c(
  'PHSTAT', 'WPH_QOL_1', 'WPH_SOC_1', 
  'WPH_DIET', 'WPH_PHYS', 'WPH_STRESS', 'WPH_SLEEP', 
  'WPH_SPIRIT', 'WPH_HEALTH',
  
  'SEX', 'AGE4', 'RACETHNICITY', 'EDUC4', 'MARITAL', 'EMPLOY',
  'INCOME4', 'METRO', 'REGION4',
  
  'CASEID', 'P_QOLEXP', 'P_SOCEXP','SAMPLE_SOURCE', # SAMPLE_SOURCE 1 = AmeriSpeak
  'group', 'exper_group', 'split_group'
)

rands_sub <- rands11[vars]

# Aggregate race/ethnicity category so that 
# Category 3: 'Other race' includes 'other race', '2 or more races', and 'Asian'
rands_sub <- rands_sub %>%
  mutate(RACETH4 = case_when(RACETHNICITY %in% c(3,5,6) ~ 3L,
                             RACETHNICITY == 1 ~1L,
                             RACETHNICITY == 2 ~2L,
                             RACETHNICITY == 4 ~4L,
                             TRUE ~ RACETHNICITY))

# set 77,98,99 values (Don't know, Skipped on web, Refused) = NA
cleaned_sub <- rands_sub
cleaned_sub[,1:11][cleaned_sub[,1:11] > 76] <- NA

######################################
# Differential Item Functioning (DIF)
######################################

# use the data subset from confirmatory group 1 (split_group = 'confirm')
	# n = 2473

group1_cfa <- cleaned_sub %>% 
  filter(exper_group ==1 & split_group =='confirm')

# using lordif package to do an iterative hybrid ordinal logistic regression
# based on IRT estimates from the graded response model

# DIF for Sex (male, female)
dif_sex <- lordif(group1_cfa[,1:9], 
                      group = group1_cfa$SEX,
                      criterion = c('Chisqr', 'R2', 'Beta'),
                      pseudo.R2 = c('McFadden'),
                      model = 'GRM',
                      MonteCarlo = FALSE,
                      R2.change = 0.035,
                      alpha = 0.01)

summary(dif_sex)
plot.lordif(dif_sex, label = c('Male', 'Female'))

# used DFIT to calculate the differential functioning of items and test statistics
# includes CDIF, NCDIF, and DTF
# these statistics were reviewed to support the interpretation of DIF magnitude
DFIT(dif_sex)

# DIF for Age (18-29, 30-44, 45-59, 60+)
dif_age <- lordif(group1_cfa[,1:9], 
                      group = group1_cfa$AGE4,
                      criterion = c('Chisqr', 'R2', 'Beta'),
                      model = 'GRM',
                      MonteCarlo = FALSE,
                      R2.change = 0.035,
                      alpha = 0.01)

summary(dif_age)
plot.lordif(dif_age, label = c('18-29', '30-44', '45-59', '60+'))

DFIT(dif_age)

# DIF for Race/ethnicity (White, Black, Other, Hispanic)
dif_raceth <- lordif(group1_cfa[,1:9], 
                  group = group1_cfa$RACETH4,
                  criterion = c('Chisqr', 'R2', 'Beta'),
                  model = 'GRM',
                  MonteCarlo = FALSE,
                  R2.change = 0.035,
                  alpha = 0.01)

summary(dif_race)
plot.lordif(dif_race, label = c('White', 'Black', 'Other', 'Hispanic'))

DFIT(dif_race)

# DIF for Education (No HS diploma, HS diploma/GED, Some college, Bachelors or higher)
dif_edu <- lordif(edu_split[,1:9], 
                   group = edu_split$EDUC4,
                   criterion = c('Chisqr', 'R2', 'Beta'),
                   model = 'GRM',
                   MonteCarlo = FALSE,
                   R2.change = 0.035)
summary(dif_edu)
plot.lordif(dif_edu, label = c('No HS diploma', 'HS diploma', 'Some college', 'Bachelor or higher'))

DFIT(dif_edu)
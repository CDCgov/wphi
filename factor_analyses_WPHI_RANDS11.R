######################################################
## RANDS 11 WPH Analyses: EFA, CFA, bifactor models ##
### created by Valerie Ryan and Catherine Lamoreaux ##
########### last updated July 16, 2026 ###############
######################################################

# load libraries
library(psych)
library(lavaan)
library(BifactorIndicesCalculator)

# read in your RANDS 11 data file
rands = read.csv()

######################
## Data Preparation ##
######################

# there were 2 versions of the quality of life and social items
# resulting in 4 experimental groups, we used group 1 for analysis
# group 1 uses the question wording used on the NHIS
table(rands$P_QOLEXP)
table(rands$P_SOCEXP)
table(rands$P_QOLEXP, rands$P_SOCEXP)

# create variable for experimental group
rands$exper_group = ifelse(rands$P_QOLEXP == 1 & rands$P_SOCEXP == 1, 1,
                           ifelse(rands$P_QOLEXP == 1 & rands$P_SOCEXP == 2, 2,
                                  ifelse(rands$P_QOLEXP == 2 & rands$P_SOCEXP == 1, 3, 4)))

table(rands$exper_group)

# randomly assign respondents to exploratory or confirmatory groups
set.seed(72)

rands$split_group = NA

for(g in 1:4){
  rands$split_group[rands$exper_group == g] = sample(c("explore", "confirm"), sum(rands$exper_group == g), replace = T)
}

table(rands$split_group, rands$exper_group)

# create exploratory, confirmatory, and full subsets for group 1
rands_subset1_exp = rands[which(rands$exper_group == 1 & rands$split_group == "explore"),]
rands_subset1_con = rands[which(rands$exper_group == 1 & rands$split_group == "confirm"),]
rands_subset1_all = rands[which(rands$exper_group == 1),]

# specify the variables from the WPHI to use for analyses
vars = c("PHSTAT", "WPH_QOL_1", "WPH_SOC_1", "WPH_DIET", "WPH_PHYS", "WPH_STRESS", 
         "WPH_SLEEP", "WPH_SPIRIT", "WPH_HEALTH")

wph_exp = rands_subset1_exp[vars] # exploratory
wph_con = rands_subset1_con[vars] # confirmatory
wph_all = rands_subset1_all[vars] # full

# set 77, 98, and 99 as missing 
# 77 is don't know, 98 is skipped on web, 99 is refused
wph_exp[wph_exp == 77] = NA
wph_exp[wph_exp == 98] = NA
wph_exp[wph_exp == 99] = NA

wph_con[wph_con == 77] = NA
wph_con[wph_con == 98] = NA
wph_con[wph_con == 99] = NA

wph_all[wph_all == 77] = NA
wph_all[wph_all == 98] = NA
wph_all[wph_all == 99] = NA

##########################
## Internal Consistency ##
##########################

# calculate omega and guttman's split-half
alpha(wph_all)
omega(wph_all)

#########
## EFA ##
#########

# try 1, 2, and 3 factor solutions
result_1fac = fa(wph_exp, nfactors = 1)
result_2fac = fa(wph_exp, nfactors = 2, rotate = "promax")
result_3fac = fa(wph_exp, nfactors = 3, rotate = "promax")

print(result_1fac)
print(result_2fac)
print(result_3fac)

#########
## CFA ##
#########

# make variables ordered factors, necessary for WLSMV estimator
wph_con$srh = as.factor(wph_con$PHSTAT)
wph_con$qol = as.factor(wph_con$WPH_QOL_1)
wph_con$soc = as.factor(wph_con$WPH_SOC_1)
wph_con$diet = as.factor(wph_con$WPH_DIET)
wph_con$phys_act = as.factor(wph_con$WPH_PHYS)
wph_con$stress = as.factor(wph_con$WPH_STRESS)
wph_con$sleep = as.factor(wph_con$WPH_SLEEP)
wph_con$spirit = as.factor(wph_con$WPH_SPIRIT)
wph_con$health_manage = as.factor(wph_con$WPH_HEALTH)

wph_con$srh_o = ordered(wph_con$srh, levels = c("1", "2", "3", "4", "5"))
wph_con$qol_o = ordered(wph_con$qol, levels = c("1", "2", "3", "4", "5"))
wph_con$soc_o = ordered(wph_con$soc, levels = c("1", "2", "3", "4", "5"))
wph_con$diet_o = ordered(wph_con$diet, levels = c("1", "2", "3", "4", "5"))
wph_con$phys_act_o = ordered(wph_con$phys_act, levels = c("1", "2", "3", "4", "5"))
wph_con$stress_o = ordered(wph_con$stress, levels = c("1", "2", "3", "4", "5"))
wph_con$sleep_o = ordered(wph_con$sleep, levels = c("1", "2", "3", "4", "5"))
wph_con$spirit_o = ordered(wph_con$spirit, levels = c("1", "2", "3", "4", "5"))
wph_con$health_manage_o = ordered(wph_con$health_manage, levels = c("1", "2", "3", "4", "5"))

# specify the general model
cfa_model = ' 
  wph =~ srh_o + qol_o + soc_o + diet_o + phys_act_o + stress_o
          + sleep_o + spirit_o + health_manage_o
  '

cfa_fit = cfa(cfa_model, data = wph_con, std.lv=TRUE, estimator = "WLSMV")
summary(cfa_fit, fit.measures = TRUE, standardized=TRUE)

# diet and physical activity residual correlation model
cfa_model_resid = ' 
  wph =~ srh_o + qol_o + soc_o + diet_o + phys_act_o + stress_o
          + sleep_o + spirit_o + health_manage_o
  
  # residual correlations
  diet_o ~~ phys_act_o
  '

cfa_fit_resid = cfa(cfa_model_resid, data = wph_con, std.lv=TRUE, estimator = "WLSMV")
summary(cfa_fit_resid, fit.measures = TRUE, standardized=TRUE)

#####################
## Bifactor Models ##
#####################

# bifactor models are tricky - can have trouble converging

# exploratory
# 3 factors
bi_model1 = omega(wph_exp, nfactors = 3, poly = T)
bi_model1

# 2 factors
bi_model2 = omega(wph_exp, nfactors = 2, poly = T)
bi_model2

# confirmatory
# 3 factor model with spirit cross-loading
bifactor_model_1 = ' 
  # general factor
  wph =~ srh_o + qol_o + soc_o + diet_o + phys_act_o + stress_o 
         + sleep_o + spirit_o + health_manage_o
  
  # group factors
  group1 =~ phys_act_o + diet_o + srh_o + health_manage_o
  group2 =~ soc_o + qol_o + spirit_o
  group3 =~ spirit_o + stress_o + sleep_o
  '
# orthogonal = T sets covariances to zero
bi_fit_1 = cfa(bifactor_model_1, data = wph_con, std.lv=TRUE, estimator = "WLSMV", orthogonal = T)
summary(bi_fit_1, fit.measures = TRUE, standardized=TRUE)

bifactorIndices(bi_fit_1)
PUC(bi_fit_1)
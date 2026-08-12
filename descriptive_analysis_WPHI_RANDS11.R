# READ ME -----------------------------------------------------------------
#
#       Author: Sarah Forrest (tyw5@cdc.gov)
# Organization: CDC/NCHS/DRM
#      Purpose: Calculate response distributions (percentage per response
#               category) and pairwise polychoric intercorrelations for 
#               each of the 9 WPHI items 
#         Data: NCHS Research and Development Survey (RANDS) 11, subset to 
#               the experimental group that received the WPHI versions 
#               selected for the NHIS (n = 4,967)
#
# -------------------------------------------------------------------------

# Set up ------------------------------------------------------------------

# Load libraries
library(tidyverse) # for data manipulation
library(haven) # for as_factor()
library(survey) # for complex survey design
library(srvyr) # for survey_mean()
library(polycor) # for polychor()


# Load and prepare data ---------------------------------------------------

data <- readRDS("path/to/RANDS11.rds") %>%
  filter(exper_group == 1) # Subset to the experimental group that received the QOL & SOC versions selected for NHIS (n = 4,967)

# WPHI items
wphi_items <- c(
  "PHSTAT", "WPH_QOL_1", "WPH_SOC_1", "WPH_DIET", "WPH_PHYS", "WPH_STRESS", 
  "WPH_SLEEP", "WPH_SPIRIT", "WPH_HEALTH"
)

# Create dummy stratum and PSU variables to handle the opt-in sample:
# - Probability sample (SAMPLE_SOURCE == 1): use existing stratum/PSU
# - Opt-in sample: assign a single shared stratum (1000) and unique PSU
data <- data %>%
  mutate(
    S_VSTRAT_DUMMY = ifelse(SAMPLE_SOURCE == 1, S_VSTRAT, 1000),
    S_VPSU_DUMMY = ifelse(SAMPLE_SOURCE == 1, S_VPSU, row_number() + 4967)
  ) %>%
  select(S_VPSU_DUMMY, S_VSTRAT_DUMMY, all_of(wphi_items)) # Subset to variables needed for analysis

# Convert labelled integers to factors
data_factors <- haven::as_factor(data, only_labelled = TRUE)


# Define survey design ----------------------------------------------------

options(survey.lonely.psu = "adjust")

data_survey <- srvyr::as_survey_design(
  data_factors,
  ids = S_VPSU_DUMMY,
  strata = S_VSTRAT_DUMMY,
  weights = NULL,
  nest = TRUE
)


# Calculate response distributions ----------------------------------------

# For each WPHI item, calculate the percentage per response category
calc_pct_dists <- function(data_survey, item) {
  data_survey %>%
    filter(!is.na(!!sym(item)) & # Exclude missing responses
             !!sym(item) != "DON'T KNOW" & 
             !!sym(item) != "SKIPPED ON WEB" & 
             !!sym(item) != "REFUSED") %>%
    group_by(response = !!sym(item)) %>%
    summarize(percent = survey_mean())%>%
    mutate(percent = round(percent * 100, 1)
    ) %>%
    select(response, percent)
}

pct_dists <- wphi_items %>%
  set_names() %>%
  map(~ calc_dists(data_survey, .x))

print(pct_dists)


# Calculate frequencies of missing values response distributions ----------

freq_miss <- data %>%
  select(all_of(wphi_items)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "response") %>%
  filter(response %in% c("DON'T KNOW", "SKIPPED ON WEB", "REFUSED")) %>%
  count(item, response) %>%
  pivot_wider(names_from = response, values_from = n, values_fill = 0) %>%
  arrange(factor(item, levels = wphi_items)) %>%
  mutate(total_missing = rowSums(across(where(is.numeric))))

print(freq_miss)


# Calculate pairwise polychoric intercorrelations -------------------------

# Initialize an empty matrix
n <- length(wphi_items)
pcor_mat <- matrix(NA, nrow = n, ncol = n, 
                   dimnames = list(wphi_items, wphi_items))

for (i in 1:(n - 1)) {
  for (j in ( i + 1):n) {
    # Build a two-way survey table for this item pair
    tab <- survey::svytable(as.formula(paste("~", wphi_items[i], "+", wphi_items[j])), design = data_survey)
    
    # polychor() requires at least 2 observed levels for each item
    if (all(dim(tab) >= 2)) {
      pcor_mat[i, j] <- polychor(tab)
      pcor_mat[j, i] <- pcor_mat[i, j]
    }
  }
}

print(round(pcor_mat, 2))
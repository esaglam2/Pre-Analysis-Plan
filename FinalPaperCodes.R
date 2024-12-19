#EYLUL BEGUM SAGLAM
#QUANTITATIVE POLITICAL ANALYSIS II
#PRE-ANALYSIS PLAN
#DECEMBER 18, 2024



library(UsingR)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(car)
library(estimatr)
library(DeclareDesign)
library(MASS)
library(optmatch)
library(RItools)
library(robustbase)
library(highs) 
library(designmatch)
library(coin)
library(haven)
library(readxl)
library(PanelMatch)
library(sensemakr)
library(sensitivitymv)
library(sensitivitymult)
library(sensitivityfull)
library(senstrat)

# Load and examine the dataset
ess11 <- read.csv("C:/Users/Eylul Begum Saglam/qpa/531-explorations-fall2024/531-explorations-fall2024/ESS11-subset_2.csv")

# Check the structure and summary
str(ess11)
summary(ess11)

# Drop unnecessary variables
ess11_necessary_data <- ess11[, !(names(ess11) %in% c("psu", "stratum", "prob", "dweight", "pweight", "prtvgde1", "prtclgde", "ctzcntr"))]

# Verify the structure after dropping
str(ess11_necessary_data)

# STEP 1: RECODE VARIABLES

## Treatment: uemp3m
ess11_necessary_data$uemp3m <- ifelse(ess11_necessary_data$uemp3m == 1, 1, 
                                      ifelse(ess11_necessary_data$uemp3m == 2, 0, NA))
table(ess11_necessary_data$uemp3m)

## Outcome: prtvgde2
ess11_necessary_data$prtvgde2 <- ifelse(ess11_necessary_data$prtvgde2 == 6, 1, 
                                        ifelse(ess11_necessary_data$prtvgde2 %in% c(1, 2, 3, 4, 5, 7, 8, 9, 55), 0, NA))
table(ess11_necessary_data$prtvgde2)

## Gender: gndr
ess11_necessary_data$gndr <- ifelse(ess11_necessary_data$gndr == 1, 1, 
                                    ifelse(ess11_necessary_data$gndr == 2, 0, NA))
table(ess11_necessary_data$gndr)
# Recode prtvgde2 to make sure missing values are properly handled
ess11_necessary_data$prtvgde2[ess11_necessary_data$prtvgde2 %in% c(66, 77, 88, 99)] <- NA

## Religion: rlgdnade
# Recode religion into Christian and Non-Christian categories
ess11_necessary_data$rlgdnade_group <- ifelse(ess11_necessary_data$rlgdnade %in% c(1, 2, 3, 4, 290, 291), "Christian", 
                                              ifelse(ess11_necessary_data$rlgdnade %in% c(5, 6, 7, 8, 9), "Non-Christian", NA))

# Verify the result
table(ess11_necessary_data$rlgdnade_group)

## Immigration economy: imbgeco
# Recode opinion on immigration into Low/Medium and Good categories
# Recode 'imbgeco' into "Low-Medium" (0-5) and "Medium-High" (6-10)
ess11_necessary_data$imbgeco_group <- ifelse(ess11_necessary_data$imbgeco %in% c(0, 1, 2, 3, 4, 5), "Low-Medium", 
                                             ifelse(ess11_necessary_data$imbgeco %in% c(6, 7, 8, 9, 10), "Medium-High", NA))

# Verify the result
table(ess11_necessary_data$imbgeco_group)

## Education: edulvlb
ess11_necessary_data$edulvlb_group <- ifelse(ess11_necessary_data$edulvlb %in% c(0, 113, 129, 212, 213, 221, 222, 223), "Low",
                                             ifelse(ess11_necessary_data$edulvlb %in% c(229, 311, 312, 313, 321, 322, 323, 412, 413, 421, 422, 423), "Medium",
                                                    ifelse(ess11_necessary_data$edulvlb %in% c(510, 520, 610, 620, 710, 720, 800), "High", NA)))
table(ess11_necessary_data$edulvlb_group)

# Categorize age into groups
ess11_necessary_data$age_group <- cut(ess11_necessary_data$agea,
                                      breaks = c(0, 29, 49, 64, Inf),
                                      labels = c("Young adults", "Adults", "Middle-aged", "Seniors"),
                                      right = TRUE) # right = TRUE includes the upper bound
# Check the distribution
table(ess11_necessary_data$age_group)
colSums(is.na(ess11_necessary_data))



# STEP 2: FACTORIZE SOME VARIABLES FOR ANALYSIS

ess11_necessary_data$uemp3m <- as.factor(ess11_necessary_data$uemp3m)
ess11_necessary_data$prtvgde2 <- as.factor(ess11_necessary_data$prtvgde2)  # Convert variables to factors
ess11_necessary_data$gndr <- as.factor(ess11_necessary_data$gndr)
ess11_necessary_data$rlgdnade_group <- as.factor(ess11_necessary_data$rlgdnade_group)

ess11_necessary_data$edulvlb_group <- ordered(ess11_necessary_data$edulvlb_group)  # Convert variables to ordered factors
ess11_necessary_data$imbgeco_group <- ordered(ess11_necessary_data$imbgeco_group)

ess11_necessary_data$age_group <- ordered(ess11_necessary_data$age_group)

colSums(is.na(ess11_necessary_data))



# STEP 3: MISSINGNESS EXPLORATION

library(VIM)
aggr_plot <- aggr(ess11_necessary_data, col = c('navyblue', 'red'), 
                  numbers = TRUE, sortVars = TRUE, labels = names(ess11_necessary_data), 
                  cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))
library(mice)

# Impute missing values using mice (multiple imputation)
# Step a: Specify the variables for imputation 
imp_vars <- ess11_necessary_data[, c("rlgdnade_group", "imbgeco_group", "edulvlb_group", "uemp3m", "prtvgde2")]

# Step b: Perform multiple imputation using appropriate methods
imp_data <- mice(imp_vars, m = 5, method = c("logreg", "polr", "polr", "logreg", "logreg"), seed = 123)

# Step c: Create missingness indicators for imputed variables
ess11_necessary_data$uemp3m_missingness <- ifelse(is.na(ess11_necessary_data$uemp3m), 1, 0)
table(ess11_necessary_data$uemp3m_missingness)

ess11_necessary_data$prtvgde2_missingness <- ifelse(is.na(ess11_necessary_data$prtvgde2), 1, 0)
table(ess11_necessary_data$prtvgde2_missingness)

ess11_necessary_data$rlgdnade_group_missingness <- ifelse(is.na(ess11_necessary_data$rlgdnade_group), 1, 0)
table(ess11_necessary_data$rlgdnade_group_missingness)

ess11_necessary_data$imbgeco_group_missingness <- ifelse(is.na(ess11_necessary_data$imbgeco_group), 1, 0)
table(ess11_necessary_data$imbgeco_group_missingness)

ess11_necessary_data$edulvlb_group_missingness <- ifelse(is.na(ess11_necessary_data$edulvlb_group), 1, 0)
table(ess11_necessary_data$edulvlb_group_missingness)


# Step d: Impute missing values for the main dataset
ess11_necessary_data$rlgdnade_group <- complete(imp_data, action = 1)$rlgdnade_group
ess11_necessary_data$imbgeco_group <- complete(imp_data, action = 1)$imbgeco_group
ess11_necessary_data$edulvlb_group <- complete(imp_data, action = 1)$edulvlb_group
ess11_necessary_data$prtvgde2 <- complete(imp_data, action = 1)$prtvgde2  # Impute prtvgde2
ess11_necessary_data$uemp3m <- complete(imp_data, action = 1)$uemp3m  # Impute uemp3m if needed

# Step e: Check the summary of the imputed dataset
summary(ess11_necessary_data)

# Step f: Check for missing values again after imputation
colSums(is.na(ess11_necessary_data))

# Check the distribution of categorical variables after imputation

table(ess11_necessary_data$prtvgde2)
table(ess11_necessary_data$gndr)
table(ess11_necessary_data$rlgdnade_group)
table(ess11_necessary_data$imbgeco_group)
table(ess11_necessary_data$edulvlb_group)
table(ess11_necessary_data$age_group)
table(ess11_necessary_data$uemp3m)



# STEP 4: MATCHING

# Convert treatment variable to numeric (if necessary)
ess11_necessary_data$uemp3m <- as.numeric(as.character(ess11_necessary_data$uemp3m))  

# Ensure all covariates are formatted appropriately
str(ess11_necessary_data)

# Define an exact matching constraint for one or more variables
# Example: Exact matching within levels of `gndr` and `rlgdnade_group`
exact_constraint <- exactMatch(uemp3m ~ gndr + rlgdnade_group, data = ess11_necessary_data)

# Compute propensity scores using a logistic regression model
ps_mod <- glm(uemp3m ~ imbgeco_group + edulvlb_group + age_group, 
              data = ess11_necessary_data, family = binomial)

# Add the propensity scores to the dataset
ess11_necessary_data$ps <- predict(ps_mod, type = "response")

# Define the distance matrix for matching with a caliper on the propensity score
# The caliper restricts the matching distance to 0.2 of the standard deviation of the propensity score
combined_distance <- match_on(ps_mod, data = ess11_necessary_data) +
  exact_constraint +  # Ensure exact matching within the constraints
  caliper(match_on(ps_mod, data = ess11_necessary_data), width = 0.2)  # Example caliper width of 0.2

# Use `fullmatch` to create matched groups
fm <- fullmatch(combined_distance, data = ess11_necessary_data)

#Add the matching results to the dataset
ess11_necessary_data$matched_set <- fm

#Summary of the matching
summary(fm)

#Tabulate treatment and control distribution within matched sets
table(ess11_necessary_data$uemp3m, ess11_necessary_data$matched_set, useNA = "ifany")

#Tabulate the number of treatment (1) and control (0) units in each matched set
table(ess11_necessary_data$uemp3m, ess11_necessary_data$matched_set, useNA = "ifany")

#Inspect the number of units in each matched set
table(ess11_necessary_data$matched_set)

# STEP 5: OMNIBUS TEST
#Hansen and Bowers

library(optmatch)

# Correct usage of xBalance
balance_test <- xBalance(
  uemp3m ~ age_group + gndr + edulvlb_group + imbgeco_group + rlgdnade_group,
  strata = matched_data$matched_set,  # Include matched strata
  data = matched_data
)

# Print the results of the balance test
print(balance_test)

# Load necessary library
library(ggplot2)

# Create a dataframe with your balance test results
balance_results <- data.frame(
  Variable = c("age_groupYoung adults", "age_groupAdults", "age_groupMiddle-aged",
               "age_groupSeniors", "gndr0", "gndr1", "edulvlb_groupHigh",
               "edulvlb_groupLow", "edulvlb_groupMedium", "imbgeco_groupLow-Medium",
               "imbgeco_groupMedium-High", "rlgdnade_groupChristian", 
               "rlgdnade_groupNon-Christian"),
  SMD = c(0.00, 0.01, 0.00, -0.01, 0.00, 0.00, 0.01, -0.03, 0.00, 0.02, -0.02, 0.00, 0.00),
  Z = c(-1.00, 2.03, -0.15, -1.29, 0.00, 0.00, 2.20, -2.14, 0.53, 2.50, -2.50, 0.00, 0.00)
)

# Add significance indicator based on Z-scores
balance_results$Significant <- ifelse(abs(balance_results$Z) > 1.96, "Significant", "Not Significant")

# Plot standardized mean differences
ggplot(balance_results, aes(x = reorder(Variable, SMD), y = SMD, fill = Significant)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +  # Flip the axes for better readability
  scale_fill_manual(values = c("Not Significant" = "lightblue", "Significant" = "red")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Standardized Mean Differences (SMD) After Matching",
    x = "Covariates",
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill = guide_legend(title = "Significance"))


# Assuming `matched_data` is your dataset after matching
if (exists("matched_data")) {
  cat("Number of observations in the matched dataset:", nrow(matched_data), "\n")
} else {
  cat("Matched dataset not found. Please verify the variable name.\n")
}

# Check summary of matched data
summary(matched_data)


# STEP 6: POWER ANALYSIS (POST-MATCHING)

#Main Model:
# Define power analysis parameters
set.seed(123)
n_sim <- 1000         # Number of simulations
n_obs <- 719          # Effective sample size from matching
control_prob <- 0.2   # Baseline probability of AfD support in control group
treatment_effect <- 0.1  # Hypothesized increase in probability due to treatment
alpha <- 0.05         # Significance level

# Placeholder to store p-values
p_values <- numeric(n_sim)

# Run simulations
for (i in 1:n_sim) {
  # Simulate covariates
  age_group <- factor(sample(c("Young", "Adult", "Middle-aged", "Seniors"), n_obs, replace = TRUE))
  gndr <- factor(sample(c("Male", "Female"), n_obs, replace = TRUE))
  edulvlb_group <- factor(sample(c("High", "Medium", "Low"), n_obs, replace = TRUE))
  imbgeco_group <- factor(sample(c("Low-Medium", "Medium-High"), n_obs, replace = TRUE))
  
  # Simulate treatment assignment (uemp3m)
  uemp3m <- rbinom(n_obs, 1, 0.5)  # 50% treatment probability
  
  # Simulate probabilities based on logistic regression
  logits <- log(control_prob / (1 - control_prob)) +
    uemp3m * log((control_prob + treatment_effect) / (1 - (control_prob + treatment_effect)))
  probabilities <- exp(logits) / (1 + exp(logits))
  
  # Generate binary outcomes
  prtvgde2 <- rbinom(n_obs, 1, probabilities)
  
  # Fit logistic regression
  model <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + imbgeco_group, family = binomial)
  
  # Extract p-value for treatment effect
  p_values[i] <- summary(model)$coefficients["uemp3m", 4]
}

# Calculate power: Proportion of p-values below significance threshold
power <- mean(p_values < alpha)
print(paste("Estimated Power:", power))


#Bivariate Model:
# Define parameters for the bivariate model
set.seed(123)
n_sim <- 1000         # Number of simulations
n_obs <- 719          # Effective sample size from matching
control_prob <- 0.2   # Baseline probability of AfD support in control group
treatment_effect <- 0.1  # Hypothesized increase in probability due to treatment
alpha <- 0.05         # Significance level

# Placeholder to store p-values
p_values_simple <- numeric(n_sim)

# Run simulations for simple model
for (i in 1:n_sim) {
  # Simulate treatment assignment (uemp3m)
  uemp3m <- rbinom(n_obs, 1, 0.5)  # 50% treatment probability
  
  # Simulate probabilities based on logistic regression
  logits <- log(control_prob / (1 - control_prob)) +
    uemp3m * log((control_prob + treatment_effect) / (1 - (control_prob + treatment_effect)))
  probabilities <- exp(logits) / (1 + exp(logits))
  
  # Generate binary outcomes
  prtvgde2 <- rbinom(n_obs, 1, probabilities)
  
  # Fit logistic regression (simple model)
  model_simple <- glm(prtvgde2 ~ uemp3m, family = binomial)
  
  # Extract p-value for treatment effect
  p_values_simple[i] <- summary(model_simple)$coefficients["uemp3m", 4]
}

# Calculate power for simple model: Proportion of p-values below significance threshold
power_simple <- mean(p_values_simple < alpha)
print(paste("Estimated Power for Simple Model:", power_simple))


#Missingness Model
# Define parameters for model with missingness
# Define parameters for model with missingness
set.seed(123)
n_sim <- 1000         # Number of simulations
n_obs <- 719          # Effective sample size from matching
control_prob <- 0.2   # Baseline probability of AfD support in control group
treatment_effect <- 0.1  # Hypothesized increase in probability due to treatment
alpha <- 0.05         # Significance level

# Placeholder to store p-values for model with missingness
p_values_missingness <- numeric(n_sim)

# Run simulations for model with missingness
for (i in 1:n_sim) {
  # Simulate treatment assignment (uemp3m)
  uemp3m <- rbinom(n_obs, 1, 0.5)  # 50% treatment probability
  
  # Simulate probabilities based on logistic regression
  logits <- log(control_prob / (1 - control_prob)) +
    uemp3m * log((control_prob + treatment_effect) / (1 - (control_prob + treatment_effect)))
  probabilities <- exp(logits) / (1 + exp(logits))
  
  # Generate binary outcomes
  prtvgde2 <- rbinom(n_obs, 1, probabilities)
  
  # Simulate covariates
  age_group <- factor(sample(c("Young", "Adult", "Middle-aged", "Seniors"), n_obs, replace = TRUE))
  gndr <- factor(sample(c("Male", "Female"), n_obs, replace = TRUE))
  edulvlb_group <- factor(sample(c("High", "Medium", "Low"), n_obs, replace = TRUE))
  imbgeco_group <- factor(sample(c("Low-Medium", "Medium-High"), n_obs, replace = TRUE))
  
  # Simulate religion variable
  rlgdnade_group <- factor(sample(c("Christian", "Non-Christian"), n_obs, replace = TRUE))
  
  # Simulate missingness indicators
  imbgeco_group_missingness <- rbinom(n_obs, 1, 0.1)  # 10% missingness
  edulvlb_group_missingness <- rbinom(n_obs, 1, 0.1)
  uemp3m_missingness <- rbinom(n_obs, 1, 0.05)
  rlgdnade_group_missingness <- rbinom(n_obs, 1, 0.05)
  
  # Fit logistic regression (model with missingness)
  model_missingness <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + 
                             rlgdnade_group + imbgeco_group + imbgeco_group_missingness + 
                             edulvlb_group_missingness + uemp3m_missingness + rlgdnade_group_missingness,
                           family = binomial)
  
  # Extract p-value for treatment effect (uemp3m)
  p_values_missingness[i] <- summary(model_missingness)$coefficients["uemp3m", 4]
}

# Calculate power for model with missingness: Proportion of p-values below significance threshold
power_missingness <- mean(p_values_missingness < alpha)
print(paste("Estimated Power for Model with Missingness:", power_missingness))





# STEP 7: HYPOTHESIS TESTING



#Main Model
# Logistic regression on matched data
model_logit <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + imbgeco_group + rlgdnade_group, 
                   data = matched_data, 
                   family = binomial)

summary(model_logit)

# Extract the coefficients and calculate the confidence intervals
conf_int <- confint(model_logit, level = 0.95)

# Exponentiate the coefficients to get odds ratios and their confidence intervals
odds_ratios <- exp(coef(model_logit))
odds_ratios_conf_int <- exp(conf_int)

# Print the odds ratios and their confidence intervals
print("Odds Ratios:")
print(odds_ratios)

print("Confidence Intervals for Odds Ratios:")
print(odds_ratios_conf_int)


#Bivariate


# Simple logistic regression model with only uemp3m (treatment) as predictor
simple_model <- glm(prtvgde2 ~ uemp3m, family = binomial, data = matched_data)

# Summary of the model
summary(simple_model)

# AIC of the simple model
simple_model_aic <- AIC(simple_model)
print(paste("AIC of the simple model: ", simple_model_aic))

conf_int <- confint(simple_model, level = 0.95)

# Exponentiate the coefficients to get odds ratios and their confidence intervals
odds_ratios <- exp(coef(simple_model))
odds_ratios_conf_int <- exp(conf_int)

# Print the odds ratios and their confidence intervals
print("Odds Ratios:")
print(odds_ratios)

print("Confidence Intervals for Odds Ratios:")
print(odds_ratios_conf_int)

#Missingness

# Logistic regression model including missingness indicators for all covariates
model_with_missingness <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + rlgdnade_group + 
                                imbgeco_group + imbgeco_group_missingness + edulvlb_group_missingness + 
                                uemp3m_missingness + rlgdnade_group_missingness,
                              data = matched_data, 
                              family = binomial)

# Summary of the regression model with missingness indicators
summary(model_with_missingness)

conf_int <- confint(model_with_missingness, level = 0.95)

# Exponentiate the coefficients to get odds ratios and their confidence intervals
odds_ratios <- exp(coef(model_with_missingness))
odds_ratios_conf_int <- exp(conf_int)

# Print the odds ratios and their confidence intervals
print("Odds Ratios:")
print(odds_ratios)

print("Confidence Intervals for Odds Ratios:")
print(odds_ratios_conf_int)


# STEP 8: PERFORMANCE OF TESTS:

#False Positive Rates:

#Main Model:
# Extract p-values from your model
p_values <- summary(model_logit_2)$coefficients[, 4]  # Adjusted p-values (last column)

# Define significance threshold
threshold <- 0.05

# Count how many predictors have p-values below the threshold (significant predictors)
significant_predictors <- sum(p_values < threshold)

# Calculate total number of predictors (excluding the intercept)
total_predictors <- length(p_values) - 1

# Calculate false positive rate (FPR)
FPR <- significant_predictors / total_predictors

# Print the result
print(paste("False Positive Rate (FPR):", FPR))



#Bivariate Model:
# Bivariate model FPR for single predictor
p_values_2 <- summary(simple_model_2)$coefficients[, 4]  # Adjusted p-values (last column)

# Define significance threshold
threshold <- 0.05

# Check if the only predictor (uemp3m) is significant
FPR <- ifelse(p_values_2[2] < threshold, 1, 0)  # Only check uemp3m, since it's the only predictor apart from the intercept

# Print the result
print(paste("False Positive Rate (FPR):", FPR))




#Model with Missingness:
p_values_3 <- summary(model_with_missingness_2)$coefficients[, 4]  # Adjusted p-values (last column)

# Define significance threshold
threshold <- 0.05

# Count how many predictors have p-values below the threshold (significant predictors)
significant_predictors <- sum(p_values_3 < threshold)

# Calculate total number of predictors (excluding the intercept)
total_predictors <- length(p_values_3) - 1

# Calculate false positive rate (FPR)
FPR <- significant_predictors / total_predictors

# Print the result
print(paste("False Positive Rate (FPR):", FPR))



#BH

#Main Model 

#Fit your logistic regression model
model_logit_2 <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + 
               imbgeco_group + rlgdnade_group, family = binomial, data = matched_data)

# Get the summary of the model
summary_model <- summary(model_logit_2)

# Extract the p-values from the model summary
p_values <- summary_model$coefficients[, 4]  # 4th column contains the p-values

# Adjust the p-values using the Benjamini-Hochberg procedure (FDR control)
p_adjusted <- p.adjust(p_values, method = "BH")

# Print the adjusted p-values
print(p_adjusted)



#Bivariate model

simple_model_2 <- glm(prtvgde2 ~ uemp3m, family = binomial, data = matched_data)
# Get the summary of the model
summary_model_2 <- summary(simple_model_2)

# Extract the p-values from the model summary
p_values_2 <- summary_model_2$coefficients[, 4]  # 4th column contains the p-values

# Adjust the p-values using the Benjamini-Hochberg procedure (FDR control)
p_adjusted_2 <- p.adjust(p_values_2, method = "BH")

# Print the adjusted p-values
print(p_adjusted_2)



#Model with Missingness
model_with_missingness_2 <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + rlgdnade_group + 
                                imbgeco_group + imbgeco_group_missingness + edulvlb_group_missingness + 
                                uemp3m_missingness + rlgdnade_group_missingness,
                              data = matched_data, 
                              family = binomial)
summary_model_3 <- summary(model_with_missingness_2)

# Extract the p-values from the model summary
p_values_3 <- summary_model_3$coefficients[, 4]  # 4th column contains the p-values

# Adjust the p-values using the Benjamini-Hochberg procedure (FDR control)
p_adjusted_3 <- p.adjust(p_values_3, method = "BH")

# Print the adjusted p-values
print(p_adjusted_3)



# STEP 9: MSE and Bias

#Main Model:
# Define parameters
n_sim <- 1000
mse_values <- numeric(n_sim)  # Store MSE for each simulation

# True value of the parameter
true_param <- 1.5

# Simulate data and calculate MSE
for (i in 1:n_sim) {
  # Simulate data
  simulated_data <- data.frame(
    uemp3m = rbinom(100, 1, 0.5),
    age_group = sample(c("Young", "Adult", "Middle-aged", "Seniors"), 100, replace = TRUE),
    prtvgde2 = rbinom(100, 1, 0.5)
  )
  
  # Fit logistic regression model
  model <- glm(prtvgde2 ~ uemp3m + age_group, data = simulated_data, family = binomial)
  
  # Estimate parameter for 'uemp3m'
  estimated_param <- coef(model)["uemp3m"]
  
  # Calculate MSE for this simulation
  mse_values[i] <- (estimated_param - true_param)^2  # MSE is just the squared error here
}

# Calculate average MSE
mean_mse <- mean(mse_values)
print(paste("Mean Squared Error (MSE):", mean_mse))


# Simulating true parameters
true_param <- 1.5  # True value of the coefficient you're estimating

# Run logistic regression model on simulated data
model <- glm(prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + imbgeco_group, 
             data = matched_data, family = binomial)

# Estimate parameter
estimated_param <- coef(model)["uemp3m"]  # Extract the estimated coefficient for the treatment variable

# Calculate bias
bias <- estimated_param - true_param
print(paste("Bias: ", bias))




#Bivariate

# Define parameters for bivariate model
n_sim <- 1000
mse_values_bivariate <- numeric(n_sim)  # Store MSE for each simulation

# True value of the parameter for bivariate model
true_param_bivariate <- 1.5

# Simulate data and calculate MSE for bivariate model
for (i in 1:n_sim) {
  # Simulate data
  simulated_data <- data.frame(
    uemp3m = rbinom(100, 1, 0.5),  # Simulate treatment
    prtvgde2 = rbinom(100, 1, 0.5) # Simulate binary outcome
  )
  
  # Fit logistic regression model
  model_bivariate <- glm(prtvgde2 ~ uemp3m, data = simulated_data, family = binomial)
  
  # Estimate parameter for 'uemp3m'
  estimated_param_bivariate <- coef(model_bivariate)["uemp3m"]
  
  # Calculate MSE for this simulation
  mse_values_bivariate[i] <- (estimated_param_bivariate - true_param_bivariate)^2
}

# Calculate average MSE for bivariate model
mean_mse_bivariate <- mean(mse_values_bivariate)
print(paste("Mean Squared Error (MSE) for Bivariate Model:", mean_mse_bivariate))

# Calculate bias for bivariate model
model_bivariate_real <- glm(prtvgde2 ~ uemp3m, data = matched_data, family = binomial)
estimated_param_bivariate_real <- coef(model_bivariate_real)["uemp3m"]
bias_bivariate <- estimated_param_bivariate_real - true_param_bivariate
print(paste("Bias for Bivariate Model:", bias_bivariate))


#Missingness

# Define parameters for model with missingness
n_sim <- 1000
mse_values_missingness <- numeric(n_sim)  # Store MSE for each simulation

# True value of the parameter for model with missingness
true_param_missingness <- 1.5

# Simulate data and calculate MSE for model with missingness
for (i in 1:n_sim) {
  # Simulate data
  simulated_data <- data.frame(
    uemp3m = rbinom(100, 1, 0.5),  # Simulate treatment
    age_group = sample(c("Young", "Adult", "Middle-aged", "Seniors"), 100, replace = TRUE),  # Simulate age
    gndr = sample(c("Male", "Female"), 100, replace = TRUE),  # Simulate gender
    edulvlb_group = sample(c("High", "Medium", "Low"), 100, replace = TRUE),  # Simulate education level
    imbgeco_group = sample(c("Low-Medium", "Medium-High"), 100, replace = TRUE),  # Simulate economic perception
    prtvgde2 = rbinom(100, 1, 0.5) # Simulate binary outcome
  )
  
  # Fit logistic regression model with missingness
  model_missingness <- glm(
    prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + imbgeco_group,
    data = simulated_data,
    family = binomial
  )
  
  # Estimate parameter for 'uemp3m'
  estimated_param_missingness <- coef(model_missingness)["uemp3m"]
  
  # Calculate MSE for this simulation
  mse_values_missingness[i] <- (estimated_param_missingness - true_param_missingness)^2
}

# Calculate average MSE for model with missingness
mean_mse_missingness <- mean(mse_values_missingness)
print(paste("Mean Squared Error (MSE) for Model with Missingness:", mean_mse_missingness))

# Calculate bias for model with missingness
model_missingness_real <- glm(
  prtvgde2 ~ uemp3m + age_group + gndr + edulvlb_group + imbgeco_group + 
    imbgeco_group_missingness + edulvlb_group_missingness + uemp3m_missingness + rlgdnade_group_missingness,
  data = matched_data,
  family = binomial
)
estimated_param_missingness_real <- coef(model_missingness_real)["uemp3m"]
bias_missingness <- estimated_param_missingness_real - true_param_missingness
print(paste("Bias for Model with Missingness:", bias_missingness))



# STEP 10: MOCK ANALYSIS

# Load necessary library
# Mock data: Odds Ratios and Confidence Intervals, including Age Groups
# Load necessary library
library(ggplot2)

# Data for covariates and their confidence intervals (from your results)
mock_data <- data.frame(
  Covariate = c("Intercept", "Unemployment", "Age Group (L)", "Age Group (Q)", 
                "Age Group (C)", "Gender (Male)", "Education (Low vs High)", 
                "Education (Medium vs High)", "Economic Perception (Low-Medium)", 
                "Religion (Non-Christian)"),
  OR = c(NA, 2.00, 0.85, 0.93, 1.20, 1.85, 1.55, 0.55, 0.30, 0.10),  # Odds Ratios from CI midpoints
  Lower_CI = c(0.02, 1.46, 0.51, 0.56, 0.88, 1.29, 1.23, 0.35, 0.20, 0.0035),  # Lower bounds
  Upper_CI = c(0.04, 3.07, 1.17, 1.22, 1.72, 2.69, 2.33, 0.94, 0.39, 0.29)   # Upper bounds
)

# Exclude the intercept for visualization (optional)
mock_data_no_intercept <- mock_data[-1, ]

# Create the visualization
ggplot(mock_data_no_intercept, aes(x = Covariate, y = OR)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Mock Analysis: Confidence Intervals for Odds Ratios",
    x = "Covariate",
    y = "Odds Ratio (OR)"
  )




# STEP 11: SENSITIVITY ANALYSIS 

# Sensitivity analysis using different gamma values
library(sensemakr)
library(sensitivitymv)
library(sensitivitymw)
library(sensitivitymult)
library(sensitivityfull)
library(senstrat)
library(rbounds)

# Gamma values to test
somegammas <- seq(1, 3, 0.05)

# Reshape the data for sensitivity analysis
respmat <- with(matched_data, reshape_sensitivity(
  y = prtvgde2,                # Outcome variable (AfD support)
  z = uemp3m,                  # Treatment variable (Unemployed vs. Employed)
  fm = matched_set             # Matching strata from full matching
))

# Perform sensitivity analysis with t-statistic (method="t")
sensTresults <- sapply(somegammas, function(g) {
  c(gamma = g, senmv(respmat, method = "t", gamma = g)$pval)
})

# Perform sensitivity analysis with mean difference (method="mean")
sensHresults <- sapply(somegammas, function(g) {
  c(gamma = g, senmv(respmat, gamma = g)$pval)
})

print(sensHresults)

# Adjusting plot margins and layout
# Check the structure of sensTresults and sensHresults
# Extract gamma values and p-values for sensTresults
gamma_T <- sensTresults[1, ]  # First row: gamma values
pval_T <- sensTresults[2, ]   # Second row: p-values

# Extract gamma values and p-values for sensHresults
gamma_H <- sensHresults[1, ]  # First row: gamma values
pval_H <- sensHresults[2, ]   # Second row: p-values

# Adjusting plot margins and layout
par(mar = c(3, 3, 2, 1), mfrow = c(1, 1))  

# Plot Sensitivity Analysis Results (T-stat)
plot(
  x = gamma_T, 
  y = pval_T, 
  type = "b",  # type 'b' for both points and lines
  xlab = "Gamma", 
  ylab = "P-Value", 
  main = "Sensitivity Analysis", 
  col = "blue",  # color for the first plot (sensTresults)
  pch = 1,  # point type for the first plot
  lty = 1,  # line type for the first plot
  ylim = c(0, 1)  # y-axis range (p-values range from 0 to 1)
)

# Add second set of points (Influential Point Resistant) to the same plot
points(
  x = gamma_H, 
  y = pval_H, 
  pch = 2,  # different point type for the second set
  col = "red"  # color for the second plot (sensHresults)
)

# Add a horizontal red line at p-value = 0.05 (significance threshold)
abline(h = 0.05, col = "green")  # Dashed red line at 0.05

# I add legends or other customizations to enhance the plot.
legend("topleft", legend = c("T-Stat", "Influential Point Resistant"), 
       col = c("blue", "red"), pch = c(1, 2), lty = 1)














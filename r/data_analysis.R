# Load Packages -----------------------------------------------------------
library(tidyverse)
library(leaps)
library(MASS)
library(corrplot)
library(car)
library(gtsummary)
library(gt)
library(webshot2)

options(scipen = 999)

# Read in the Data --------------------------------------------------------
gi_bill <- read_rds(file = "../data/final/final_gi_bill_data_for_analysis.rds")

# Load Static Values ------------------------------------------------------
n_gi_tool <- nrow(read_rds(file = "../data/original/gi_bill_comparison_tool.rds"))
n_census <- nrow(read_rds(file = "../data/original/vets_acs_survey_by_zip_code.rds"))
split_percentage <- .70
gi_max <- 27120

# Prepare Modeling Data -------------------------------------------
model_data <- gi_bill %>%
  dplyr::select(bah:avg_yellow_ribbon_payment, institution) %>%
  dplyr::select(!c(p911_recipients, p911_yr_recipients, p911_tuition_fees, p911_yellow_ribbon)) %>%
  na.omit()

model_data <- model_data %>%
  filter(n_gibill != 0) %>%
  mutate(avg_yellow_ribbon_payment = avg_yellow_ribbon_payment) %>%
  mutate(avg_yellow_ribbon_payment = if_else(avg_yellow_ribbon_payment == 0, .0000001, avg_yellow_ribbon_payment))



# Model Selection ---------------------------------------------------------
full_model_formula <- log(n_gibill) ~ bah + log(undergrad_enrollment) + log(avg_yellow_ribbon_payment) + log(annual_tuition) +
  yr_member + poe_member + eight_keys_member + dodmou +
  credit_for_mil_training + vet_poc + student_vet_grp_ipeds +
  graduation_rate_all_students + retention_all_students_ba +
  salary_all_students + n_vets_census

full_model <- lm(full_model_formula, data = model_data)

summary(full_model)

## Exploratory Analysis ----------------------------------------------------
plot(full_model)

hist(full_model$residuals)

hist(model_data$bah)

### Cross Validation -- For Exploratory Purposes ----------------------------
set.seed(123)
train_index <- sample(nrow(model_data), split_percentage * nrow(model_data))
training_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Sub-setting model variables using backward selection.
reg_subsets_train <- regsubsets(full_model_formula, data = training_data, nvmax = 20, method = "backward")

## Exploring the plot functions from the leap packages
plot(reg_subsets_train, scale = "adjr2")

### Creating a model matrix to use for testing from the test data.
full_model_matrix <- model.matrix(full_model_formula, data = test_data)

# I am going to compare RMSE for training and test data for a bunch of different models to try and select the best model. So, I will
# Run a for-loop for each variable;
## Extract the coefficients from the regsubsets used on my training data earlier;
## Multiply them into the appropriate columns;
### Compute the test MSE.

n_variables <- reg_subsets_train$last - 1 # remove the intercept as a variable.
rmse_for_models <- rep(NA, n_variables)

for (i in 1:n_variables) {
  coefi <- coef(reg_subsets_train, id = i)
  pred <- full_model_matrix[, names(coefi)] %*% coefi
  rmse_for_models[i] <- sqrt(mean((test_data$n_gibill - pred)^2))
}

# Which model did the best?
which.min(rmse_for_models)
coef(reg_subsets_train, which.min(rmse_for_models)) # this model has the lowest RMSE

tibble(
  rmse_for_models = rmse_for_models,
  n_var = 1:n_variables,
  var_names = coef(reg_subsets_train, n_var)
) %>%
  arrange(rmse_for_models)

### For completeness, I am going to check the RMSE on the training data to see how they change.
full_model_matrix_train <- model.matrix(full_model_formula, data = training_data)
n_variables <- reg_subsets_train$last - 1 # remove the intercept as a variable.
rmse_for_models_train <- rep(NA, n_variables)

for (i in 1:n_variables) {
  coefi <- coef(reg_subsets_train, id = i)
  pred <- full_model_matrix_train[, names(coefi)] %*% coefi
  rmse_for_models_train[i] <- sqrt(mean((test_data$n_gibill - pred)^2))
}

which.min(rmse_for_models_train)
coef(reg_subsets_train, which.min(rmse_for_models_train))


## Stepwise Variable Selection ---------------------------------------------
stepAIC(object = full_model, direction = "both")

selected_model <- lm(formula = log(n_gibill) ~ log(undergrad_enrollment) +
  log(avg_yellow_ribbon_payment) + log(annual_tuition) + poe_member +
  dodmou + credit_for_mil_training + student_vet_grp_ipeds + log(salary_all_students) +
  n_vets_census, data = model_data)

summary(selected_model)

# Model Diagnostics -------------------------------------------------------
plot(selected_model)

tibble(actual = selected_model$residuals, fitted = selected_model$fitted.values) %>%
  ggplot(aes(x = actual, y = fitted)) +
  geom_point()


# Check for multicollinearity ---------------------------------------------
## Correlation matrix
corr_matrix <- model_data %>%
  dplyr::select(-institution) %>%
  cor()

corrplot(corr_matrix)

## Variance Inflation Factor (VIF)
vif(selected_model)

# Identify Outliers -------------------------------------------------------
## Identify residuals that have a standard deviation above three.
potential_outliers <- tibble(
  standardized_residuals = scale(selected_model$residuals),
  observation = 1:nrow(model_data)
) %>%
  filter(abs(standardized_residuals) > 3) %>%
  dplyr::select(observation)

model_data[potential_outliers$observation, ]

## Create a model without those outliers
no_outlier <- lm(formula = log(n_gibill) ~ log(undergrad_enrollment) + log(avg_yellow_ribbon_payment) +
  log(annual_tuition) + poe_member + dodmou + credit_for_mil_training + student_vet_grp_ipeds +
  salary_all_students + n_vets_census, data = model_data[-potential_outliers$observation, ])

summary(no_outlier)
plot(no_outlier)
hist(no_outlier$residuals)

# It appears the colleges that have a really low veteran enrollment ( <3 ) are populating as outliers.
# However, removing these points to not appear to drastically change my analysis. Moreover, they appear to be naturally occurring, so I don't see a real reason to remove them.

# Final Tables and Visualizations -----------------------------------------
regression_sum_tbl <- selected_model %>%
  tbl_regression(
    label = list(
      "log(undergrad_enrollment)" ~ "Log Undergraduate Enrollment",
      "log(avg_yellow_ribbon_payment)" ~ " Log Mean Yellow Ribbon Payment",
      "log(annual_tuition)" ~ "Log Annual Tuition",
      "n_vets_census" ~ "Veterans in Community From Census",
      "log(salary_all_students)" ~ "Log Post-Graduation Salary (All Students)",
      "poe_member" ~ "Principles of Excellence Signatory",
      "dodmou" ~ "Signed MOU with Department of Defense",
      "credit_for_mil_training" ~ "Offers Academic Credit for Military Training",
      "student_vet_grp_ipeds" ~ "Has Recognized Student Veteran Group on Campus"
    ), intercept = TRUE,
    show_single_row = c(poe_member, dodmou, credit_for_mil_training, student_vet_grp_ipeds),
    estimate_fun = function(x) style_number(x, digits = 4)
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%
  add_vif() %>%
  add_significance_stars() %>%
  modify_caption("**College Characteristics**") %>%
  add_glance_source_note(include = c(r.squared, adj.r.squared, p.value)) %>% 
  as_gt()

gtsave(data = regression_sum_tbl, filename = "./tables_graphs/regression_summary.png") 




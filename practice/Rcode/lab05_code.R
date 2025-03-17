# #    logo: imgs_slides/mitgest_logo.png ----
# #    footer: <https://lulliter.github.io/R4biostats/lectures.html> ----
# ## ------------- x salvare come PDF  ----
# # GOAL OF TODAY'S PRACTICE SESSION {.section-slide}   ----
# ## Lab # 5  ----
# ## ACKNOWLEDGEMENTS ----
# # R ENVIRONMENT SET UP & DATA {.section-slide} ----
# ## Needed R Packages ----

# # Prefer non-scientific notation ----
# Prefer non-scientific notation
options(scipen = 999)

# # Load pckgs for this R session ----
# Load pckgs for this R session
# # --- General  ----
# --- General 
library(here)     # tools find your project's files, based on working directory
library(dplyr)    # A Grammar of Data Manipulation
library(skimr)    # Compact and Flexible Summaries of Data
library(tibble)   # Simple Data Frames
library(magrittr) # A Forward-Pipe Operator for R 
library(readr)    # A Forward-Pipe Operator for R 
library(tidyr)    # Tidy Messy Data
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
# # ---Plotting & data visualization ----
# ---Plotting & data visualization
library(ggplot2)      # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggfortify)     # Data Visualization Tools for Statistical Analysis Results
library(scatterplot3d) # 3D Scatter Plot
# # --- Statistics ----
# --- Statistics
library(MASS)       # Support Functions and Datasets for Venables and Ripley's MASS
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining
library(rstatix)    # Pipe-Friendly Framework for Basic Statistical Tests
library(car)        # Companion to Applied Regression
library(ROCR)       # Visualizing the Performance of Scoring Classifiers
library(pROC)      # Display and Analyze ROC Curves
# # --- Tidymodels (meta package) ----
# --- Tidymodels (meta package)
library(rsample)    # General Resampling Infrastructure  
library(broom)      # Convert Statistical Objects into Tidy Tibbles

# # LOGISTIC REGRESSION {.section-slide} ----
# ## Logistic regression: review ----
# ## Logistic regression: `logit` function ----
# # DATASET #1: Heart Disease {.section-slide} ----
# ## [Dataset on Heart Disease (`heart_data`)]{.r-fit-text} ----

# # Use `here` in specifying all the subfolders AFTER the working directory  ----
# Use `here` in specifying all the subfolders AFTER the working directory 
heart_data <- utils::read.csv(file = here::here("practice", "data_input", "05_datasets",
                                      "heart_data.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# ## `heart_data` variables with description ----

#| output: true
#| echo: false

heart_data_desc <- tibble::tribble(
  ~Variable, ~ Type, ~Description,
#"X" ,  "integer" ,         "row counter ", 
"heart_disease",    "int",  "whether an individual has heart disease (1 = yes; 0 = no)", 
"coffee_drinker",   "int",  "whether an individual drinks coffee regularly (1 = yes; 0 = no)", 
"fast_food_spend",  "dbl",  "a numerical field corresponding to the annual spend of each individual on fast food", 
"income",           "dbl",   "a numerical field corresponding to the individual’s annual income"

)                 

kableExtra::kable(heart_data_desc)

# ## `heart_data` dataset splitting ----

set.seed(123)

# # Obtain 2 sub-samples from the dataset: training and testing ----
# Obtain 2 sub-samples from the dataset: training and testing
sample  <-  sample(c(TRUE, FALSE), nrow(heart_data), replace = TRUE , prob = c(0.7, 0.3) )
heart_train  <-  heart_data[sample,]
heart_test  <-  heart_data[!sample,]


# # check the structure of the resulting datasets ----
# check the structure of the resulting datasets
dim(heart_train)
dim(heart_test)

# ## Convert binary variables to factors ----

heart_train <- heart_train %>% 
  # convert to factor with levels "Yes" and "No"
  dplyr::mutate(heart_disease = factor(heart_disease, levels = c(0, 1),
                                labels = c("No_HD", "Yes_HD")),
         coffee_drinker = factor(coffee_drinker, levels = c(0, 1),
                                 labels = c("No_Coffee", "Yes_Coffee")) 
  )

# # show the first 5 rows of the dataset ----
# show the first 5 rows of the dataset
heart_train[1:5,]

# ## Plotting `Y` by `X1` (continuous variable) ----

#| output: true
#| output-location: slide
#| fig.cap: |
#|   + The boxplots indicate that subjects with heart disease (HD =1) seem to spend higher amounts on fast food <br>
#|   + Also, this sample has many more subjects without heart disease (HD = 0) than with heart disease (HD = 1)

# # plot the distribution of heart disease status by fast food spend ----
# plot the distribution of heart disease status by fast food spend
heart_train %>% 
  ggplot2::ggplot(aes(x = heart_disease, y = fast_food_spend, fill = heart_disease)) + 
  geom_jitter(aes(fill = heart_disease), alpha = 0.3, shape = 21, width = 0.25) +  
  scale_color_manual(values = c("#005ca1", "#9b2339")) + 
  scale_fill_manual(values = c("#57b7ff", "#e07689")) + 
  geom_boxplot(fill = NA, color = "black", linewidth = .7) + 
  coord_flip() +
    theme(plot.title = element_text(size = 13,face="bold", color = "#873c4a"),
        axis.text.x = element_text(size=12,face="italic"), 
        axis.text.y = element_text(size=12,face="italic"),
        legend.position = "none") + 
  labs(title = "Fast food expenditure by heart disease status") + 
  xlab("Heart Disease (Y/N)") + 
  ylab("Annual Fast Food Spend") 

# ## Plotting `Y` by `X2` (discrete variable) ----

#| output: true
#| output-location: slide
#| fig.cap: 'Also drinking coffee seems associated to a higher likelihood of heart disease (HD =1)'

heart_train %>% 
  # 1) Count the unique values per each group from 2 categorical variables' combinations
  dplyr::count(heart_disease, coffee_drinker, name = "count_by_group") %>% 
  dplyr::group_by(coffee_drinker) %>% 
  dplyr::mutate(
    total_coffee_class = sum(count_by_group),
    proportion = count_by_group / total_coffee_class) %>% 
  dplyr::ungroup() %>% 
  # 2) Filter only those with heart disease
  dplyr::filter(heart_disease == "Yes_HD") %>% 
  # 3) Plot frequency of heart disease status by coffee drinking 
  ggplot2::ggplot(aes(x = coffee_drinker, y = proportion, fill = coffee_drinker)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("#57b7ff", "#e07689")) + 
  theme_minimal() +
  ylab("Percent with Heart Disease") +
  xlab("Coffee Drinker (Y/N)") +
  ggtitle("Figure 3: Percent of HD incidence by CoffeeDrinking class") +
  labs(fill = "Coffee Drinker") + 
  scale_y_continuous(labels = scales::percent)

# ## `Linear regression` wouldn't work! ----

# # --- 1) Linear regression model ----
# --- 1) Linear regression model
linear_mod <- lm(heart_disease ~ fast_food_spend# + coffee_drinker
                 , data = heart_data)
# # --- 2) Logistic regression model ----
# --- 2) Logistic regression model
logit_mod <- glm(heart_disease ~ fast_food_spend# + coffee_drinker
                 , data = heart_data, family = "binomial")

# ## [Compute alternative models' predictions]{.r-fit-text}  ----

# # --- 1) Extract coefficients from linear regression model ----
# --- 1) Extract coefficients from linear regression model
intercept_lin <- coef(linear_mod)[1]
fast_food_spend_lin <- coef(linear_mod)[2]
coffee_drinker_lin <- coef(linear_mod)[3]

# # --- 2) Extract coefficients from logit regression model ----
# --- 2) Extract coefficients from logit regression model
intercept_logit <- coef(logit_mod)[1]
fast_food_spend_logit <- coef(logit_mod)[2]
coffee_drinker_logit <- coef(logit_mod)[3]

# # --- Estimate predicted data from different models    ----
# --- Estimate predicted data from different models   
heart_data <- heart_data %>%
  dplyr::mutate(
    # Convert outcome variable to factor
    heart_disease_factor = factor(heart_disease, 
                                  labels = c("No Disease (Y=0)", "Disease (Y=1)")),
    # 1) Linear model prediction
    lin_pred = intercept_lin + fast_food_spend_lin * fast_food_spend, 
    # 2) Logit model prediction
    logit_pred = intercept_logit + fast_food_spend_logit * fast_food_spend,  coffee_drinker,
    # 3) Convert logit to probability (logistic model prediction)
    logistic_pred = 1 / (1 + exp(-logit_pred))) %>%
  dplyr::arrange(fast_food_spend)   

# ## Plot alternative models' outcomes ----

#| output: true
#| output-location: slide
#| fig.width: 8
#| fig.height: 4.5
#| fig.cap: |
#|      + (The **actual** data points are shown as the grey dots)<br>
#|      + The **linear** model predicts `values` that are ≠ 0 and 1, which poorly fit the actual data <br>
#|      + The **logit** model predicts `log(odds)` ranging from -Inf to +Inf, which is not interpretable <br>
#|      + The **logistic** model _squeezes_ `probabilities` between 0 and 1, which fits the data better  

# # --- Plot   ----
# --- Plot  
ggplot2::ggplot(heart_data, aes(x = fast_food_spend)) +
  # Actual dataset observations (Y=0, Y=1 ) using `color =`
  geom_jitter(aes(y = heart_disease, color = heart_disease_factor), 
              width = 200, height = 0.03, alpha = 0.75, size = 2, shape = 16) + 
  # Models' predictions (smooth trends)
  geom_smooth(aes(y = lin_pred, color = "Linear Regression"), method = "lm", se = FALSE, 
              linewidth = 1.25, linetype = "dashed") +
  geom_smooth(aes(y = logit_pred, color = "Logit (Log-Odds)"), method = "lm", se = FALSE, 
              linewidth = 1.25, linetype = "dotdash") +
  geom_smooth(aes(y = logistic_pred, color = "Logistic Regression"), method = "glm", 
              method.args = list(family = "binomial"), se = FALSE, 
              linewidth = 1.25, linetype = "solid") +
  # Separate legends: color for dots, color for lines
  scale_color_manual(name = "Actual Y values & Prediction Models", 
                     values = c("No Disease (Y=0)" = "#A6A6A6", "Disease (Y=1)" = "#4c4c4c",
                                "Linear Regression" = "#d02e4c","Logit (Log-Odds)" = "#239b85", 
                                "Logistic Regression" = "#BD8723")) +
  # Define scales for the axes
  scale_x_continuous(breaks = seq(0, 6500, by = 500), limits = c(0, 6500), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(-3, 3, by = .25)) +
  coord_cartesian(ylim = c(-1.25,1.25), xlim = c(0, 6500))  + theme_minimal() +
  labs(title = "Comparing Linear and Logistic Regression Predictions v. actual Y values",
       #subtitle = "(For simplicity, only fast food spend is considered)",
       y = "Y = Heart disease [0,1]", x = "Fast food spend [US$/yr]", color = "Actual Y values and Predictions")

# ## `Linear regression` didn't work! ----

#| label: fig-lin-reg_diag
#| output: true
#| output-location: slide
#| fig.show: "hold"
#| fig.cap: |
#|      "Diagnostic plots for a hypothetical linear regression model 👎🏻" <br>
#|      + a) Residuals should be randomly scattered around 0 <br>
#|      + b) QQ plot should be close to the diagonal line 

# # Set graphical parameters to plot side by side ----
# Set graphical parameters to plot side by side
par(mfrow = c(1, 2))

# # Diagnostic plots ----
# Diagnostic plots
plot(linear_mod, which = 1)
plot(linear_mod, which = 2)

# # Reset graphical parameters (optional, avoids affecting later plots) ----
# Reset graphical parameters (optional, avoids affecting later plots)
par(mfrow = c(1, 1))


# | eval: false # need at least 2 predictors
# | echo: false
# | output: false

# # multicollinearity ----
# multicollinearity
#vif(linear_mod)
# # ideally < 5 ----
# ideally < 5

# ## [Fitting a `logistic regression` model]{.r-fit-text} ----

#| output: true

# # Fit a logistic regression model ----
# Fit a logistic regression model
heart_model <- stats::glm(heart_disease ~ coffee_drinker + fast_food_spend + income,
                   data = heart_train, 
                   family = binomial(link = "logit"))

# ## Model output ----

#| echo: true
#| output: true
#| output-location: slide
#| label: tbl-logit_heart
#| tbl-cap: |
#|  Logistic regression model output <br><br>
#|    + **estimates** of coefficients are in the form of `natural logarithm of the odds` (log (odds)) of the event happening (Heart Disease) <br>
#|        + a `positive estimate` indicates an increase in the odds of having Heart Desease  <br>
#|        + a `negative estimate` indicates a decrease in the odds of having Heart Desease <br>
#|    + **odds ratio** = the exponentiated coefficient estimate <br><br>

# # Convert model's output summary into data frame ----
# Convert model's output summary into data frame
heart_model_coef <- broom::tidy(heart_model) %>% 
  # improve readability of significance levels
  dplyr::mutate('signif. lev.' = case_when(
    `p.value` < 0.001 ~ "***",
    `p.value` < 0.01 ~ "**",
    `p.value` < 0.05 ~ "*",
    TRUE ~ ""))%>%
  # add odds ratio column
  dplyr::mutate(odds_ratio = exp(estimate)) %>%
  dplyr::relocate(odds_ratio, .after = estimate) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  # format as table
  knitr::kable() %>% 
  # reduce font size
  kable_styling(font_size = 20) %>% 
  # add table title
  kableExtra::add_header_above(c("Logistic Regression Analysis of Heart Disease Risk Factors"= 7))

heart_model_coef

# ## [Interpreting the logistic `coefficients`]{.r-fit-text} ----

#| echo: false
#| output: true

broom::tidy(heart_model) %>% 
  # improve readability of significance levels
  dplyr::mutate('signif. lev.' = case_when(
    `p.value` < 0.001 ~ "***",
    `p.value` < 0.01 ~ "**",
    `p.value` < 0.05 ~ "*",
    TRUE ~ ""))%>%
  # add odds ratio column
  dplyr::mutate(odds_ratio = exp(estimate)) %>%
  dplyr::relocate(odds_ratio, .after = estimate) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  knitr::kable() %>% 
  # reduce font size
  kable_styling(font_size = 20) %>% 
  # highlight rows of terms (intercept) and income
  kableExtra::row_spec(1,  background = "#f1e7d3") %>%
  kableExtra::row_spec(4,  background = "#f1e7d3")


# ## [The `coefficient` of fast food $$ 🍔🍟]{.r-fit-text} ----

#| echo: false
#| output: true

broom::tidy(heart_model) %>% 
  # improve readability of significance levels
  dplyr::mutate('signif. lev.' = case_when(
    `p.value` < 0.001 ~ "***",
    `p.value` < 0.01 ~ "**",
    `p.value` < 0.05 ~ "*",
    TRUE ~ ""))%>%
  # add odds ratio column
  dplyr::mutate(odds_ratio = exp(estimate)) %>%
  dplyr::relocate(odds_ratio, .after = estimate) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  # filter for intercept and fast food spend
  dplyr::filter(term %in% c("(Intercept)", "fast_food_spend")) %>% 
  knitr::kable() %>% 
  # # reduce font size
  # kable_styling(font_size = 20) %>% 
  # highlight rows of terms (intercept) and income
  kableExtra::row_spec(2,  background = "#f1e7d3")  

# ## [The `coefficient` of fast food $$ 🍔🍟]{.r-fit-text} ----

#| echo: false
#| output: true

broom::tidy(heart_model) %>% 
  # improve readability of significance levels
  dplyr::mutate('signif. lev.' = case_when(
    `p.value` < 0.001 ~ "***",
    `p.value` < 0.01 ~ "**",
    `p.value` < 0.05 ~ "*",
    TRUE ~ ""))%>%
  # add odds ratio column
  dplyr::mutate(odds_ratio = exp(estimate)) %>%
  dplyr::relocate(odds_ratio, .after = estimate) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  # filter for intercept and fast food spend
  dplyr::filter(term %in% c("(Intercept)", "fast_food_spend")) %>% 
  knitr::kable() %>% 
  # # reduce font size
  # kable_styling(font_size = 20) %>% 
  # highlight rows of terms (intercept) and income
  kableExtra::row_spec(2,  background = "#f1e7d3")  

# ## [The `coefficient` of coffee drinking ☕️]{.r-fit-text} ----

#| echo: false
#| output: true

broom::tidy(heart_model) %>% 
  # improve readability of significance levels
  dplyr::mutate('signif. lev.' = case_when(
    `p.value` < 0.001 ~ "***",
    `p.value` < 0.01 ~ "**",
    `p.value` < 0.05 ~ "*",
    TRUE ~ ""))%>%
  # add odds ratio column
  dplyr::mutate(odds_ratio = exp(estimate)) %>%
  dplyr::relocate(odds_ratio, .after = estimate) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  # filter for intercept and fast food spend
  dplyr::filter(term %in% c("(Intercept)", "coffee_drinkerYes_Coffee")) %>% 
  knitr::kable() %>% 
  # reduce font size
  # kable_styling(font_size = 20) %>% 
  # highlight rows of terms (intercept) and income
  kableExtra::row_spec(2,  background = "#f1e7d3")  

# ## [Wait, is drinking coffee good or bad? 🤔]{.r-fit-text} ----
# # DIGRESSION {.section-slide} ----
# ## Understanding `Odds` {background-color="#d5eae0"} ----
# ## Understanding `Odds Ratio` {background-color="#d5eae0"} ----
# ## [Example: Coffee Drinkers vs. Non-Coffee Drinkers]{.r-fit-text}{background-color="#d5eae0"} ----

#| output: true
#| echo: false

library(knitr)
library(kableExtra)

# # Create the table with properly formatted headers ----
# Create the table with properly formatted headers
odds_table <- data.frame(
  "Odds Ratio (O.R.)" = c("O.R. < 1", "O.R. > 1"),
  "Example O.R." = c("0.48", "2.08"),
  "Formula for % Change in Odds" = c("(1 - O.R.) × 100", "(O.R. - 1) × 100"), 
  "Calculation" = c("(1 - 0.48) × 100 = 52%", "(2.08 - 1) × 100 = 108%"),
  "Interpretation" =  c("52% lower odds of having heart disease compared to non-coffee drinkers", 
                         "108% higher odds of having heart disease compared to coffee drinkers"),
  check.names = FALSE # Prevents R from replacing spaces with dots
)

# # Print as a kable table with better formatting ----
# Print as a kable table with better formatting
kable(odds_table, align = "c") %>% 
  #caption = "Odds Ratio Interpretation: Coffee Drinkers vs. Non-Coffee Drinkers",  %>%
  # kableExtra::kable_styling(full_width = FALSE, font_size = 18) %>%  # Reduced font size slightly for better fit
  # kableExtra::row_spec(0, bold = TRUE, font_size = 22) %>%  # Bolder and slightly larger header
  kableExtra::column_spec(3:5, background = "#f1e7d3")  

# ## [Making predictions from `logistic regression` model ✍🏻]{.r-fit-text} ----
# ## Making predictions from `logistic regression` model ✍🏻 ----

# # Define values for individual case  ----
# Define values for individual case 
x1 <- 1
x2 <- 5000
x3 <- 50000

# # Coefficients from our logistic regression model ----
# Coefficients from our logistic regression model
# # (mind the decimals!) ----
# (mind the decimals!)
beta_0 <- -11.055360943337
b1 <- -0.729562678149
b2 <- 0.002376170127
b3 <- -0.000002304669 

# # Compute the exponent part ----
# Compute the exponent part
exponent <- beta_0 + (b1 * x1) + (b2 * x2) + (b3 * x3)

# # Compute the predicted probability ----
# Compute the predicted probability
probability <- 1 / (1 + exp(-exponent))

# # Print the result ----
# Print the result
print(probability) # 0.4951735


# ## Making predictions from `logistic regression` model 👩🏻‍💻 ----

# # Define values for individual case (as df) ----
# Define values for individual case (as df)
individual <- data.frame(
  coffee_drinker = "Yes_Coffee", # x1 (factor)  
  fast_food_spend = 5000,  # x2  
  income = 50000) # x3 

# # Make a prediction for individual outcome ----
# Make a prediction for individual outcome
predict(heart_model, individual, type = "response") # 0.4951735 

# ## Adding predictions to the training data ----

# # Add calculated predictions to the whole training dataset ----
# Add calculated predictions to the whole training dataset
heart_train$heart_disease_pred <- predict(heart_model, type = "response")


# # Subsetof 3 with heart_disease = Yes_HD ----
# Subsetof 3 with heart_disease = Yes_HD
heart_train[heart_train$heart_disease == "Yes_HD", ][1:3,] 
# # Subsetof 3 with heart_disease = No_HD ----
# Subsetof 3 with heart_disease = No_HD
heart_train[heart_train$heart_disease == "No_HD", ][1:3,]

# ## [Converting predictions into classifications]{.r-fit-text} ----

#| output: true

# # Convert predictions to classifications ----
# Convert predictions to classifications
heart_train <- heart_train %>%
  dplyr::mutate(heart_disease_pred_class = ifelse(heart_disease_pred > 0.5, 
                                           "pred_YES", "pred_NO"))
# # (... same BASE WAY) ----
# (... same BASE WAY)
# # heart_train$heart_disease_pred_class <- ifelse(heart_train$heart_disease_pred > 0.5, 1, 0) ----
# heart_train$heart_disease_pred_class <- ifelse(heart_train$heart_disease_pred > 0.5, 1, 0)

# ## [Evaluating performance with the confusion matrix]{.r-fit-text} ----
# ## Confusion matrix with threshold of 0.5 ----

#| output: true
 
# # Confusion matrix at 0.5 ----
# Confusion matrix at 0.5
confusion_matrix <- table(heart_train$heart_disease, heart_train$heart_disease_pred_class)

confusion_matrix %>% 
  knitr::kable()


# ## Sensitivity, Specificity, Accuracy ----

#| output: true
#| echo: true

sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ]) # 80/228
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ]) # 6790/6820
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) # 6870/7048

# # Print the results ----
# Print the results
sensitivity # 0.3508772
specificity # 0.9956012
accuracy # 0.9751131

# ## Making predictions on `test data` ----

# # Prep test data to match training data ----
# Prep test data to match training data
heart_test <- heart_test %>% 
  # convert to factor with levels "Yes" and "No"
  dplyr::mutate(heart_disease = factor(heart_disease, levels = c(0, 1),
                                labels = c("No_HD", "Yes_HD")),
         coffee_drinker = factor(coffee_drinker, levels = c(0, 1),
                                 labels = c("No_Coffee", "Yes_Coffee")) 
  ) 

heart_test_pred <- heart_test %>%
  # add prediction as column 
  mutate(heart_disease_pred = predict(heart_model, newdata = heart_test, type = "response"))


# ## Confusion matrix with threshold of 0.5 ----

#| output: true

# # Convert predictions to classifications ----
# Convert predictions to classifications
heart_test_pred$heart_disease_pred_class <- ifelse(
  heart_test_pred$heart_disease_pred > 0.5, "pred_YES", "pred_NO")
 
# # Confusion matrix at 0.5 ----
# Confusion matrix at 0.5
confusion_matrix_test <- table(heart_test_pred$heart_disease,
                               heart_test_pred$heart_disease_pred_class)

# # Print the confusion matrix ----
# Print the confusion matrix
confusion_matrix_test %>% 
  knitr::kable()

# ## [Sensitivity, Specificity, Accuracy (on test data)]{.r-fit-text} ----

#| output: true

sensitivity_test <- confusion_matrix_test[2, 2] / sum(confusion_matrix_test[2, ]) # 25/105
specificity_test <- confusion_matrix_test[1, 1] / sum(confusion_matrix_test[1, ]) # 2838/2847
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test) # 2863/2952

# # Print the results ----
# Print the results
sensitivity_test # 0.2380952
specificity_test # 0.9968388
accuracy_test    # 0.9698509

# ## [Performance at different classification thresholds]{.r-fit-text} ----

# # Set different thresholds options ----
# Set different thresholds options
thresh_025 <- 0.25
thresh_075 <- 0.75

# # Classify the predictions based on such alternative threshold ----
# Classify the predictions based on such alternative threshold
heart_test_pred <-  heart_test_pred %>% 
  mutate(heart_disease_pred_class_025 = ifelse(heart_disease_pred > thresh_025,
                                               "pred_YES", "pred_NO"),
         heart_disease_pred_class_075 = ifelse(heart_disease_pred > thresh_075,
                                               "pred_YES", "pred_NO"))
 
# # Recompute confusion matrices for different thresholds ----
# Recompute confusion matrices for different thresholds
confusion_matrix_test_025 <- table(heart_test_pred$heart_disease,
                                   heart_test_pred$heart_disease_pred_class_025)
confusion_matrix_test_075 <- table(heart_test_pred$heart_disease,
                                   heart_test_pred$heart_disease_pred_class_075)

# # Calcluate sensitivity and specificity for each threshold ----
# Calcluate sensitivity and specificity for each threshold
sensitivity_test_025 <- confusion_matrix_test_025[2, 2] / sum(confusion_matrix_test_025[2, ])
sensitivity_test_075 <- confusion_matrix_test_075[2, 2] / sum(confusion_matrix_test_075[2, ])
specificity_test_025 <- confusion_matrix_test_025[1, 1] / sum(confusion_matrix_test_025[1, ])
specificity_test_075 <- confusion_matrix_test_075[1, 1] / sum(confusion_matrix_test_075[1, ])

# ## [Confusion matrices at different classification thresholds]{.r-fit-text} ----

#| output: TRUE
#| echo: false
#| tbl-cap: "Confusion matrix at threshold 0.25"
#| tbl-cap-location: top

# # Apply cell_spec to highlight one specific cell (e.g., row 2, column 2) ----
# Apply cell_spec to highlight one specific cell (e.g., row 2, column 2)
confusion_matrix_test_025[2, 2] <- kableExtra::cell_spec(confusion_matrix_test_025[2, 2], background = "#e9c776")

confusion_matrix_test_025[1,1] <- kableExtra::cell_spec(confusion_matrix_test_025[1,1], background = "#ffc2b0")

# # Print the confusion matrix ----
# Print the confusion matrix
confusion_matrix_test_025 %>% 
  knitr::kable() 
  
 


#| output: TRUE
#| echo: false
#| tbl-cap: "Confusion matrix at threshold = 0.75"
#| tbl-cap-location: top
 
# # Apply cell_spec to highlight one specific cell (e.g., row 2, column 2) ----
# Apply cell_spec to highlight one specific cell (e.g., row 2, column 2)
confusion_matrix_test_075[2, 2] <- kableExtra::cell_spec(confusion_matrix_test_075[2, 2], background = "#e9c776")

confusion_matrix_test_075[1,1] <- kableExtra::cell_spec(confusion_matrix_test_075[1,1], background = "#ffc2b0")

# # Print the confusion matrix ----
# Print the confusion matrix
confusion_matrix_test_075 %>% 
  knitr::kable()

# ## [Performance at different classification thresholds]{.r-fit-text} ----

# # Print the classification results at the threshold 0.25  ----
# Print the classification results at the threshold 0.25 
sensitivity_test_025 # 0.4857143 (a.k.a. TRUE POSITIVE rate)
specificity_test_025 # 0.9803302 (a.k.a. TRUE NEGATIVE rate)


# # Print the classification results at the threshold 0.75 ----
# Print the classification results at the threshold 0.75
sensitivity_test_075 # 0.1047619 (a.k.a. TRUE POSITIVE rate)
specificity_test_075 # 0.9989463 (a.k.a. TRUE NEGATIVE rate)

# ## ROC curve may help deciding ----

#| output: true

# # Create a the needed data for a ROC curve with confidence intervals ----
# Create a the needed data for a ROC curve with confidence intervals
roc_curve_data <- pROC::roc(heart_test_pred$heart_disease,
                       heart_test_pred$heart_disease_pred,
                       ci = TRUE, # Compute confidence intervals
                       thresholds = "best", # Select the best threshold
                       print.thres = "best") 

# # Extract the best threshold value directly from roc_curve_data ----
# Extract the best threshold value directly from roc_curve_data
best_threshold <- pROC::coords(roc_curve_data, 
                         # looks for the best threshold
                         x = "best", 
                         # and returns: 
                         ret=c("threshold","specificity", "sensitivity"))
# # Print the best threshold ----
# Print the best threshold
best_threshold

# ## ROC curve may help deciding ----

#| output: true
#| output-location: slide
#| fig-cap: "ROC Curve for Heart Disease Prediction: Logistic regression model on test data <br>*(Ideally, the curve is as close to the top-left corner as possible.)*"
#| fig-cap-location: top

# # Use function to store the plot as an object ----
# Use function to store the plot as an object
roc_plot <- function() {
  plot(roc_curve_data, col = "#00589b", lwd = 2, 
       # main = "ROC Curve for Heart Disease Prediction",
       # sub = "Logistic regression model on test data",
       xlab = "(Specificity) False Positive Rate", 
       ylab = "(Sensitivity) True Positive Rate")
  # Add a horizontal line at the best threshold
  abline(h = best_threshold$sensitivity, col = "#cd6882", lty = 2)
  # Add the AUC text
  text(0.5, 0.5, paste("AUC =", round(pROC::auc(roc_curve_data), 4)), 
       col = "#887455", cex = 1.5)
}

# # Call the function to plot the ROC curve ----
# Call the function to plot the ROC curve
roc_plot()

# ## Conclusions ----
# # PCA: EXAMPLE of UNSUPERVISED ML ALGORITHM {.section-slide} ----
# # DATASET #2: `biopsy` {.section-slide} ----
# ## Dataset on Breast Cancer Biopsy] ----
# ## Importing Dataset `biopsy`] ----

# # (after loading pckg) ----
# (after loading pckg)
# # library(MASS)   ----
# library(MASS)  

# # I can call  ----
# I can call 
utils::data(biopsy)

# ## `biopsy` variables with description] ----

#| output: true
#| echo: false

biopsy_desc <- tibble::tribble(
  ~Variable, ~ Type, ~Description,
#"X" ,  "integer" ,         "row counter ", 
"id" ,   "character",               "Sample id",
"V1",    "integer 1 - 10",        "clump thickness",       
"V2",    "integer 1 - 10",        "uniformity of cell size",   
"V3",    "integer 1 - 10",        "uniformity of cell shape",  
"V4",    "integer 1 - 10",        "marginal adhesion",              
"V5",    "integer 1 - 10",        "single epithelial cell size",   
"V6",    "integer 1 - 10",        "bare nuclei (16 values are missing)",
"V7",    "integer 1 - 10",        "bland chromatin",             
"V8",    "integer 1 - 10",        "normal nucleoli",       
"V9",    "integer 1 - 10",        "mitoses",               
"class", "factor"     ,        "benign or malignant" )                 

kableExtra::kable(biopsy_desc)

# ## `biopsy` dataset manipulation] ----


# # new (manipulated) dataset  ----
# new (manipulated) dataset 
data_biopsy <- biopsy %>% 
  # drop incomplete & non-integer columns
  dplyr::select(-ID, -class) %>% 
  # drop incomplete observations (rows)
  dplyr::filter(complete.cases(.))

# ## `biopsy` dataset manipulation] ----

# # check reduced dataset  ----
# check reduced dataset 
str(data_biopsy)

# ## Calculate Principal Components ----

# # calculate principal component ----
# calculate principal component
biopsy_pca <- prcomp(data_biopsy, 
                     # standardize variables
                     scale = TRUE)

# ## Analyze Principal Components ----

names(biopsy_pca)

# ## Analyze Principal Components (cont.) ----

summary(biopsy_pca)

# ## Proportion of Variance for components] ----

# # a) Extracting Proportion of Variance from summary ----
# a) Extracting Proportion of Variance from summary
summary(biopsy_pca)$importance[2,]

# # b) (same thing) ----
# b) (same thing)
round(biopsy_pca$sdev^2 / sum(biopsy_pca$sdev^2), digits = 5)

# ## Cumulative Proportion of variance for components] ----

# # Extracting Cumulative Proportion from summary ----
# Extracting Cumulative Proportion from summary
summary(biopsy_pca)$importance[3,]

# # VISUALIZING PCA OUTPUTS {.section-slide} ----
# ## Scree plot ----

#| output-location: slide
#| fig-cap: "The obtained **scree plot** simply visualizes the output of `summary(biopsy_pca)`."

# # Scree plot shows the variance of each principal component  ----
# Scree plot shows the variance of each principal component 
factoextra::fviz_eig(biopsy_pca, 
                     addlabels = TRUE, 
                     ylim = c(0, 70))

# ## Principal Component `Scores`] ----

#| output-location: slide

# # Create new object with PC_scores ----
# Create new object with PC_scores
PC_scores <- as.data.frame(biopsy_pca$x)
head(PC_scores)

# ## Principal Component `Scores` plot (adding label variable)] ----

# # retrieve class variable ----
# retrieve class variable
biopsy_no_na <- na.omit(biopsy)
# # adding class grouping variable to PC_scores ----
# adding class grouping variable to PC_scores
PC_scores$Label <- biopsy_no_na$class

# ## Principal Component `Scores` plot (2D)] ----

#| output-location: slide
#| fig-cap: "Figure 1 shows the observations projected into the new data space made up of principal components"

ggplot2::ggplot(PC_scores, 
       aes(x = PC1, 
           y = PC2, 
           color = Label)) +
  geom_point() +
  scale_color_manual(values=c("#245048", "#CC0066")) +
  ggtitle("Figure 1: Scores Plot") +
  theme_bw()

# ## Principal Component `Scores` (2D Ellipse Plot)] ----

#| output-location: slide
#| fig-cap: "Figure 2 shows the observations projected into the new data space made up of principal components, with 95% confidence regions displayed." 

ggplot2::ggplot(PC_scores, 
       aes(x = PC1, 
           y = PC2, 
           color = Label)) +
  geom_point() +
  scale_color_manual(values=c("#245048", "#CC0066")) +
  stat_ellipse() + 
  ggtitle("Figure 2: Ellipse Plot") +
  theme_bw()

# ## Principal Component `Scores` plot (3D)] ----

#| output-location: slide
#| fig-cap: "Figure 3 shows the observations projected into the new 3D data space made up of principal components." 

# # 3D scatterplot ... ----
# 3D scatterplot ...
plot_3d <- with(PC_scores, 
                scatterplot3d::scatterplot3d(PC_scores$PC1, 
                                             PC_scores$PC2, 
                                             PC_scores$PC3, 
                                             color = as.numeric(Label), 
                                             pch = 19, 
                                             main ="Figure 3: 3D Scatter Plot", 
                                             xlab="PC1",
                                             ylab="PC2",
                                             zlab="PC3"))

# # ... + legend ----
# ... + legend
legend(plot_3d$xyz.convert(0.5, 0.7, 0.5), 
       pch = 19, 
       yjust=-0.6,
       xjust=-0.9,
       legend = levels(PC_scores$Label), 
       col = seq_along(levels(PC_scores$Label)))

# ## Biplot: principal components v. original variables] ----

#| output-location: slide
#| fig-cap: "The axes show the principal component scores, and the vectors are the loading vectors"

factoextra::fviz_pca_biplot(biopsy_pca, 
                repel = TRUE,
                col.var = "black",
                habillage = biopsy_no_na$class,
                title = "Figure 4: Biplot", geom="point")

# ## Interpreting biplot output ----
# ## Interpreting biplot output (cont.) ----

scores <- biopsy_pca$x

loadings <- biopsy_pca$rotation
# # excerpt of first 2 components ----
# excerpt of first 2 components
loadings[ ,1:2] 

# ## Recap of the workshop's content] ----
# ## Conclusions ----

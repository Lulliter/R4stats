# #    logo: imgs_slides/mitgest_logo.png ----
# #    footer: <https://lulliter.github.io/R4biostats/lectures.html> ----
# ## ------------- x salvare come PDF  ----
# # [GOAL OF TODAY'S PRACTICE SESSION]{.r-fit-text} ----
# ## Topics discussed in Lecture # 6 ----
# # R ENVIRONMENT SET UP & DATA ----
# ## Needed R Packages ----

# # Load pckgs for this R session ----
# Load pckgs for this R session

# # --- General  ----
# --- General 
library(here)     # tools find your project's files, based on working directory
library(dplyr)    # A Grammar of Data Manipulation
library(skimr)    # Compact and Flexible Summaries of Data
library(magrittr) # A Forward-Pipe Operator for R 
library(readr)    # A Forward-Pipe Operator for R 

# # Plotting & data visualization ----
# Plotting & data visualization
library(ggplot2)      # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggfortify)     # Data Visualization Tools for Statistical Analysis Results
library(scatterplot3d) # 3D Scatter Plot

# # --- Statistics ----
# --- Statistics
library(MASS)       # Support Functions and Datasets for Venables and Ripley's MASS
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining
library(rstatix)    # Pipe-Friendly Framework for Basic Statistical Tests

# # --- Tidymodels (meta package) ----
# --- Tidymodels (meta package)
library(rsample)    # General Resampling Infrastructure  
library(broom)      # Convert Statistical Objects into Tidy Tibbles

# # POWER ANALYSIS  ----
# # DATASETS for today ----
# ## Sample Size determination in Inferential statistics  ----
# ## [Purpose and challenges of Power Analysis]{.r-fit-text} ----
# ## [Required inputs to define the sample size `n`]{.r-fit-text} ----
# ## [Specifying effect size]{.r-fit-text} ----
# ## [Specifying effect size: general guidelines]{.r-fit-text} ----
# ## The `pwr` package   ----
# ## [One Sample Mean: EXE data]{.r-fit-text} ----

# # Load data on river fish length  ----
# Load data on river fish length 
fishlength_data <- readr::read_csv(here::here("practice", "data_input", "05_datasets", 
                                              "fishlength.csv"),
                              show_col_types = FALSE)

# # Select a portion of the data (i.e. out "pilot" experiment)  ----
# Select a portion of the data (i.e. out "pilot" experiment) 
guanapo_data <- fishlength_data %>% 
  dplyr::filter(river == "Guanapo")

# # Pilot experiment data  ----
# Pilot experiment data 
names(guanapo_data)
mean_H1 <-  mean(guanapo_data$length) # 18.29655
mean_H1
sd_sample <- sd(guanapo_data$length)  # 2.584636
sd_sample

# ## [One Sample Mean t-test: EXAMPLE cont.]{.r-fit-text} ----

# # Hypothetical fish population length mean (H0) ----
# Hypothetical fish population length mean (H0)
mean_H0 <- 20
# # one-sample mean t-test  ----
# one-sample mean t-test 
t_stat <- stats::t.test(x = guanapo_data$length,
                        mu = mean_H0,
                        alternative = "two.sided")
# # one-sample t-test results ----
# one-sample t-test results
t_stat

# ## [One Sample Mean t-test: POWER ANALYSIS (`n`)]{.r-fit-text} ----

# # Cohen's d formula  ----
# Cohen's d formula 
eff_size <- (mean_H1 - mean_H0) / sd_sample # -0.6590669

# # power analysis to actually calculate the minimum sample size required: ----
# power analysis to actually calculate the minimum sample size required:
pwr::pwr.t.test(d = eff_size, 
                sig.level = 0.05, 
                power = 0.8,
                type = "one.sample")

# ## [One Sample Mean t-test: POWER ANALYSIS, stricter conditions]{.r-fit-text} ----

# # power analysis to actually calculate the minimum sample size required: ----
# power analysis to actually calculate the minimum sample size required:
pwr::pwr.t.test(d = eff_size, 
                sig.level = 0.01, 
                power = 0.9,
                type = "one.sample")

# ## [Two Independent Samples: EXE data]{.r-fit-text} ----

# # Explore complete data  ----
# Explore complete data 
fishlength_data %>% 
  dplyr::group_by (river) %>% 
  dplyr::summarise (N = n(), 
                    mean_len = mean(length),
                    sd_len = sd(length)) 


#| output-location: slide
#| fig-cap: "The fish in the 2 samples appear to have different mean length" 

# # visualize the data ----
# visualize the data
fishlength_data %>% 
  ggplot(aes(x = river, y = length)) +
  geom_boxplot()

# ## [Two Independent Samples: t-test]{.r-fit-text} ----

# # Perform two-samples unpaired test ----
# Perform two-samples unpaired test
fishlength_data %>% 
  rstatix::t_test(length ~ river,
                  paired = FALSE
                    )

# ## [Two Independent Samples: POWER ANALYSIS 1/2]{.r-fit-text} ----

# # Estimate cohen's d  ----
# Estimate cohen's d 
fishlength_data %>%
  rstatix::cohens_d(length ~ river, var.equal = TRUE)

# ## [Two Independent Samples: POWER ANALYSIS 2/2  (`n`)]{.r-fit-text} ----

# # run power analysis  ----
# run power analysis 
pwr::pwr.t.test(d = 0.94, power = 0.8, sig.level = 0.05,
           type = "two.sample", alternative = "two.sided")

# ## [Two Paired Samples: EXE data]{.r-fit-text} ----

# # Load data  ----
# Load data 
cortisol_data <- read.csv(file = here::here("practice", "data_input", "05_datasets", 
                                        "cortisol.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# # Explore data  ----
# Explore data 
names(cortisol_data)

cortisol_data %>% 
  dplyr::group_by (time) %>% 
  dplyr::summarise (
    N = n(), 
    mean_cort = mean(cortisol),
    sd_cort = sd(cortisol)) 

# ## [Two Paired Samples t-test: visualization]{.r-fit-text} ----

#| fig-cap: "The cortisol levels in the 2 paired amples appear quite different" 

# # visualize the data ----
# visualize the data
cortisol_data %>% 
  ggplot(aes(x = time, y = cortisol)) +
  geom_boxplot()

# ## [Two Paired Samples: POWER ANALYSIS (`d`)]{.r-fit-text} ----

# # power analysis to actually calculate the effect size at the desired conditions: ----
# power analysis to actually calculate the effect size at the desired conditions:
pwr::pwr.t.test(n = 20, 
                #d =  eff_size, 
                sig.level = 0.05, 
                power = 0.8,
                type = "paired")

# ## [Two Paired Samples t-test: POWER ANALYSIS on given `n`]{.r-fit-text} ----

d <- cortisol_data %>% 
  # estimate cohen's d
  rstatix::cohens_d(cortisol ~ time, paired = TRUE)

d

# ## [Two Paired Samples t-test: POWER ANALYSIS gives sufficient `n`]{.r-fit-text} ----

# # power analysis to calculate minimunm n given the observed effect size in the sample  ----
# power analysis to calculate minimunm n given the observed effect size in the sample 
pwr::pwr.t.test(# n = 20, 
                d =  -1.16, 
                sig.level = 0.05, 
                power = 0.8,
                type = "paired")


# ## [One-way ANOVA test: EXE data]{.r-fit-text} ----

# # Load data  ----
# Load data 
mussels_data <- read.csv(file = here::here("practice", "data_input", "05_datasets", 
                                        "mussels.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# # Explore data  ----
# Explore data 
names(mussels_data)

stats <- mussels_data %>% 
  dplyr::group_by (location) %>% 
  dplyr::summarise (
    N = n(), 
    mean_len = mean(length),
    sd_len = sd(length)) 

stats

# ## [One-way ANOVA test: visualization]{.r-fit-text} ----

#| output-location: slide

# # Visualize the data with a boxplot ----
# Visualize the data with a boxplot
mussels_data %>% 
  ggplot(aes(x = location, y = length)) +
  geom_boxplot()

# ## [One-way ANOVA test: EXAMPLE cont.]{.r-fit-text} ----

# # Summary of test outputs:  ----
# Summary of test outputs: 
summary_ANOVA <- summary(stats::aov(length ~ location,
                   data = mussels_data))

# # From which we extract all the output elements  ----
# From which we extract all the output elements 
# # F value  ----
# F value 
summary_ANOVA[[1]][["F value"]] # 7.121019
# # p value  ----
# p value 
summary_ANOVA[[1]][["Pr(>F)"]]  # 0.0002812242
# # df of numerator and denominator ----
# df of numerator and denominator
summary_ANOVA[[1]][["Df"]]      # 4, 34 
# # Sum of Square BETWEEN groups ----
# Sum of Square BETWEEN groups
SSB <- summary_ANOVA[[1]][["Sum Sq"]][1]  # 0.004519674
# # Sum of Square WITHIN groups ----
# Sum of Square WITHIN groups
SSW <- summary_ANOVA[[1]][["Sum Sq"]][2]  # 0.005394906

# ## [One-way ANOVA test: POWER ANALYSIS (`effect`)]{.r-fit-text} ----

n_loc <- nrow(stats)

means_by_loc <- c(0.0780, 0.0748, 0.103, 0.0802, 0.0957)
overall_mean <-  mean(means_by_loc)
sd_by_loc <- c(0.0129, 0.00860, 0.0162, 0.0120, 0.0130)
overall_sd <-  mean(sd_by_loc)

# ## [One-way ANOVA test: POWER ANALYSIS (`effect`)]{.r-fit-text} ----

# # Effect Size f formula ----
# Effect Size f formula
Cohen_f = sqrt( sum( (1/n_loc) * (means_by_loc - overall_mean)^2) ) /overall_sd
Cohen_f # EXTREMELY BIG 

# # Power analysis with given f  ----
# Power analysis with given f 
pwr::pwr.anova.test(k = n_loc,
                    n = NULL,
                    f = Cohen_f,
                    sig.level = 0.05,
                    power = 0.80)

# ## [Linear Regression with grouped data: EXE data]{.r-fit-text} ----

# # define the linear model ----
# define the linear model
lm_mussels <- lm(length ~ location, 
                 data = mussels_data)


#| output-location: slide

# # summarise the model ----
# summarise the model
summary(lm_mussels)

# ## [Linear Regression with grouped data: POWER ANALYSIS]{.r-fit-text} ----

# # Extract R squared ----
# Extract R squared
R_2 <- summary(lm_mussels)$r.squared
# # compute f squared ----
# compute f squared
f_2 <- R_2 / (1 - R_2)
f_2


# # power analysis for overall linear model  ----
# power analysis for overall linear model 
pwr::pwr.f2.test(u = 4, v = NULL, 
                 f2 = 0.8378974,
                 sig.level = 0.05 , power = 0.8)

# ## [Linear Regression with grouped data: POWER ANALYSIS interpret.]{.r-fit-text} ----

pwr::pwr.f2.test(u = 4, f2 = 0.8378974,
            sig.level = 0.05 , power = 0.8)

# # SAMPLE SPLITTING IN MACHINE LEARNING ----
# ## [2 different approaches with different takes on empirical data]{.r-fit-text} ----
# ## [Data Splitting in ML approaches]{.r-fit-text} ----
# ## [Introducing R (metapackage) `tidymodels` for modeling and ML]{.r-fit-text} ----
# ## [Revisiting NHANES for a quick demonstration of predictive modeling]{.r-fit-text} ----

# # (we already saved this dataset in our project folders) ----
# (we already saved this dataset in our project folders)

# # Use `here` in specifying all the subfolders AFTER the working directory  ----
# Use `here` in specifying all the subfolders AFTER the working directory 
nhanes <- read.csv(file = here::here("practice", "data_input", "03_datasets",
                                      "nhanes.samp.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# ## Splitting the dataset into training and testing samples ----

# # ensure we always get the same result when sampling (for convenience ) ----
# ensure we always get the same result when sampling (for convenience )
set.seed(12345)

nhanes_split <- nhanes %>%
  # define the training proportion as 75%
  rsample::initial_split(prop = 0.75,
  # ensuring both sets are balanced in gender
                         strata = Gender)

# # resulting datasets ----
# resulting datasets
nhanes_train <- rsample::training(nhanes_split)
dim(nhanes_train)
nhanes_test <- rsample::testing(nhanes_split)
dim(nhanes_test)

# ## [Fitting a linear model on the training data]{.r-fit-text} ----

# # fitting  linear regression model specification ----
# fitting  linear regression model specification
lin_mod <- lm(BMI ~ Age + Gender + PhysActive, data = nhanes_train)

summary(lin_mod)

# ## [Predicting BMI estimates for new data set]{.r-fit-text} ----

# # Obtain predictions from the results of a model fitting function ----
# Obtain predictions from the results of a model fitting function
pred_bmi <- stats::predict(lin_mod, 
               newdata = nhanes_test,
               interval = "confidence" )
head(pred_bmi)

# ## [Evaluating the predictive performance in testing data]{.r-fit-text} ----

# # Computing the Root Mean Square Error ----
# Computing the Root Mean Square Error
RMSE_test <- sqrt(mean((nhanes_test$BMI - predict(lin_mod, nhanes_test))^2, na.rm = T))
RMSE_test # 6.170518

# ## [... and what about RMSE in training data?]{.r-fit-text} ----

RMSE_train <- sqrt(mean((nhanes_train$BMI - predict(lin_mod, nhanes_train))^2, na.rm = T))
RMSE_train # 6.866044

# # R squared is also quite low  ----
# R squared is also quite low 
summary(lin_mod)$r.squared     # R^2 0.0341589

# # WRAPPING UP TODAY'S KEY MESSAGE  ----
# ## [Recap of the workshop's content]{.r-fit-text} ----
# ## Final thoughts ----

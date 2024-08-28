# Lab 3: Modeling correlation and regression ------------------------------
# Practice session covering topics discussed in Lecture 3 


# GOAL OF TODAY'S PRACTICE SESSION ----------------------------------------
# + Review the basic questions we can ask about ASSOCIATION between any two variables:
#   + does it exist?
#   + how strong is it?
#   + what is its direction?
# + Introduce a widely used analytical tool: REGRESSION

# The examples and code from this lab session follow very closely the open access book:  
# Vu, J., & Harrington, D. (2021). **Introductory Statistics for the Life and Biomedical Sciences**. [https://www.openintro.org/book/biostat/](https://www.openintro.org/book/biostat)

# _________---------------------------------------------------------------------

# R ENVIRONMENT SET UP & DATA ---------------------------------------------

##  Load pckgs for this R session ---------------------------------------------
# General 
library(fs)      # file/directory interactions
library(here)    # tools find your project's files, based on working directory
library(paint) # paint data.frames summaries in colour
library(janitor) # tools for examining and cleaning data
library(dplyr)   # {tidyverse} tools for manipulating and summarizing tidy data 
library(forcats) # {tidyverse} tool for handling factors
library(openxlsx) # Read, Write and Edit xlsx Files
library(flextable) # Functions for Tabular Reporting

# Statistics
library(rstatix) # Pipe-Friendly Framework for Basic Statistical Tests
library(lmtest) # Testing Linear Regression Models # Testing Linear Regression Models
library(broom) # Convert Statistical Objects into Tidy Tibbles
library(tidymodels) # not installed on this machine
library(performance) # Assessment of Regression Models Performance 

# Plotting
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
 

## Our dataset for today  ---------------------------------------------
## Importing from your project folder (previously downloaded file) ---------------------------------------------
# Make sure to match your own folder structure (argument of function `here`)

##  Importing Dataset 1 (NHANES) ----

# Use `here` in specifying all the subfolders AFTER the working directory 
nhanes_samp <- read.csv(file = here::here("practice", "data_input", "03_datasets","nhanes.samp.csv"), 
                        header = TRUE, # 1st line is the name of the variables
                        sep = ",", # which is the field separator character.
                        na.strings = c("?","NA" ), # specific MISSING values  
                        row.names = NULL) 

## Importing Dataset 2 (PREVEND) ----
# Use `here` in specifying all the subfolders AFTER the working directory 
prevend_samp <- read.csv(file = here::here("practice", "data_input", "03_datasets",
                                           "prevend.samp.csv"), 
                         header = TRUE, # 1st line is the name of the variables
                         sep = ",", # which is the field separator character.
                         na.strings = c("?","NA" ), # specific MISSING values  
                         row.names = NULL) 

## Importing Dataset 3 (FAMuSS) ---- 
# Use `here` in specifying all the subfolders AFTER the working directory 
famuss <- read.csv(file = here::here("practice", "data_input", "03_datasets",
                                     "famuss.csv"), 
                   header = TRUE, # 1st line is the name of the variables
                   sep = ",", # which is the field separator character.
                   na.strings = c("?","NA" ), # specific MISSING values  
                   row.names = NULL) 


# _________---------------------------------------------------------------------


# CORRELATION -------------------------------------------------------------

## Two numerical variables (plot) (NHANES) ---- 
# rename for convenience
nhanes <- nhanes_samp %>% 
  janitor::clean_names()

# basis plot 
plot(nhanes$height, nhanes$weight,
     xlab = "Height (cm)", ylab = "Weight (kg)", cex = 0.8)  

##  Two numerical variables: correlation (with `stats::cor`) ---- 

# Check them first 
is.numeric(nhanes$height) 
is.numeric(nhanes$weight)

# using `stats` package
stats::cor(x = nhanes$height, y =  nhanes$weight, 
           # argument for dealing with missing values
           use = "pairwise.complete.obs",
           method = "pearson")

##  Two numerical variables: correlation (with `stats::cor.test`) ---- 

# using `stats` package 
cor_test_result <- cor.test(x = nhanes$height, y =  nhanes$weight, 
                            method = "pearson")

# looking at the cor estimate
cor_test_result[["estimate"]][["cor"]]

# OR The function `ggpubr::ggscatter` gives us all in one (scatter plot + $r$ ("R"))! 🤯 
library("ggpubr") # 'ggplot2' Based Publication Ready Plots
ggpubr::ggscatter(nhanes, x = "height", y = "weight", 
                  cor.coef = TRUE, cor.method = "pearson", #cor.coef.coord = 2,
                  xlab = "Height (in)", ylab = "Weight (lb)")

##  Spearman rank-order correlation ---- 

# The **Spearman's rank-order correlation is the nonparametric version** of the `Pearson` correlation. 

tabyl(nhanes$education)
tabyl(nhanes$health_gen)

# Let's say we want to get Spearman's correlation with ordinal factors `Education` and `HealthGen` in the `NHANES` sample. 
# + We have to convert them to their underlying numeric code, to compare rankings. 

nhanes <- nhanes %>% 
  # reorder education
  mutate (edu_ord = factor (education, 
                            levels = c("8th Grade", "9 - 11th Grade",
                                       "High School", "Some College",
                                       "College Grad" , NA))) %>%  
  # create edu_rank 
  mutate (edu_rank = as.numeric(edu_ord)) %>% 
  # reorder health education
  mutate (health_ord = factor (health_gen, 
                               levels = c( NA, "Poor", "Fair",
                                           "Good", "Vgood",
                                           "Excellent"))) %>%
  # create health_rank 
  mutate (health_rank = as.numeric(health_ord))



## Spearman rank-order correlation (example), cont.  ---- 

# + Let's check out the `..._rank` version of the 2 categorical variables of interest: 
#   + **education** from `edu_ord` to `edu_rank`

table(nhanes$edu_ord, useNA = "ifany" )
table(nhanes$edu_rank, useNA = "ifany" )

#  + **general health** from `health_ord` to `health_rank`
table(nhanes$health_ord, useNA = "ifany" )
table(nhanes$health_rank,  useNA = "ifany" )


##  Spearman rank-order correlation (example cont.) ---- 
# After setting up the variables in the correct (numerical rank) format, now we can actually compute it: 
# -- using `stats` package 
cor_test_result_sp <- cor.test(x = nhanes$edu_rank,
                               y = nhanes$health_rank, 
                               method = "spearman", 
                               exact = FALSE) # removes the Ties message warning 
# looking at the cor estimate
cor_test_result_sp

# -- only print Spearman rho 
cor_test_result_sp[["estimate"]][["rho"]]


# TWO CATEGORICAL VARIABLES (FAMuSS) -------------------------------------------


##  [Two categorical variables (plot)  ---- 
# In the `famuss` dataset, the variables `race`, and `actn3.r577x` are categorical variables.
# + we can use the generic base R function `graphics::barplot`  

mycolors_contrast <- c("#9b2339", "#E7B800","#239b85", "#85239b", "#9b8523","#23399b", "#d8e600", "#0084e6","#399B23",  "#e60066" , "#00d8e6",  "#005ca1", "#e68000")

## genotypes as columns
genotype.race = matrix(table(famuss$actn3.r577x, famuss$race), ncol=3, byrow=T)
colnames(genotype.race) = c("CC", "CT", "TT")
rownames(genotype.race) = c("African Am", "Asian", "Caucasian", "Hispanic", "Other")

# using generic base::barplot
graphics::barplot(genotype.race, col = mycolors_contrast[1:5], ylim=c(0,300), width=2)
legend("topright", inset=c(.05, 0), fill=mycolors_contrast[1:5], 
       legend=rownames(genotype.race))

## Two categorical variables (contingency table)  ---- 
# A **contingency table** summarizes data for two categorical variables. 

# levels of actn3.r577x
table(famuss$actn3.r577x)

# contingency table to summarize race and actn3.r577x
addmargins(table(famuss$race, famuss$actn3.r577x))

## Two categorical variables (contingency table prop)  ---- 
# Contingency tables can also be converted to show *proportions*. Since there are 2 variables, it is necessary to specify whether the proportions are calculated according to the row variable or the column variable.

# adding row proportions
addmargins(prop.table(table(famuss$race, famuss$actn3.r577x), margin =  1))

# adding column proportions
addmargins(prop.table(table(famuss$race, famuss$actn3.r577x),margin =  2))

## Chi Squared test of `independence`  ---- 
# Chi-squared test
# (Test of association to see if 
# H0: the 2 cat var (race  & actn3.r577x ) are independent
# H1: the 2 cat var are correlated in __some way__

tab <- table(famuss$race, famuss$actn3.r577x)
test_chi <- chisq.test(tab)

# the obtained result (`test_chi`) is a list of objects...
View(test_chi)

# + `Observed frequencies` = how often a combination occurs in our sample
# Observed frequencies
test_chi$observed

# + `Expected frequencies` = what would it be if the 2 vars were PERFECTLY INDEPENDENT  
# Expected frequencies
round(test_chi$expected  , digits = 1 )


## Chi Squared test of `independence` (results) ---- 

# + The result of Chi-Square test represents a comparison of the above two tables (*observed* v. *expected*): 
# + p-value = 0.01286 smaller than α = 0.05 so **we REJECT the null hypothesis** (i.e. there’s likely an association between race and ACTN3 gene)

test_chi

##  Computing Cramer's V after test of independence ---- 
# 1/2 Compute Creamer's V by hand

# inputs 
chi_calc <- test_chi$statistic
n <- nrow(famuss) # N of obd 
n_r <- nrow(test_chi$observed) # number of rows in the contingency table
n_c <- ncol(test_chi$observed) # number of columns in the contingency table

# Cramer’s V
sqrt(chi_calc / (n*min(n_r -1, n_c -1)) )

# 2/2 Using an R function `rstatix::cramer_v`
# Cramer’s V with rstatix
rstatix::cramer_v(test_chi$observed)


# GOODNESS OF FIT TEST  ---------------------------------------------------



## Chi Squared test of `goodness of fit`  ---- 
# Since the participants of the **FAMuSS study** where *volunteers* at a university, they did not come from a "representative" sample of the US population, we can use the Chi Squared goodness of fit test to test against:

# retrieve from input data folder (replace to match your own file path)
famuss_race <- openxlsx::read.xlsx(
  here::here("practice","data_input","03_datasets","famuss_race.xlsx") )

# print with nice format 
famuss_race %>% flextable()

# Manually write the vectors of frequencies from the 2 rows  
observed <- c(27,  55,  467, 46)
expected <- c(76.2,  5.95, 478.38,  34.51)

# Calculate Chi-Square statistic manually 
chi_sq_statistic <- sum((observed - expected)^2 / expected) 
df <- length(observed) - 1 
p_value <- 1 - pchisq(chi_sq_statistic, df) 

# Print results 
chi_sq_statistic
df
p_value 

# The calculated Chi Squared statistic is very large, and the `p_value` is close to 0. Hence, there is more than sufficient evidence to **reject the null hypothesis** that the sample is representative of the general population.  
# Comparing the observed and expected values (or the residuals), we find the **largest discrepancy with the over-representation of Asian study participants**.


# SIMPLE LINEAR REGRESSION  -----------------------------------------------



##  Visualize the data: BMI and age (NHANES) ---- 

# We are mainly looking for a "vaguely" linear shape here 
ggplot(nhanes, aes (x = age, 
                    y = bmi)) + 
  geom_point() + 
  geom_smooth(method = lm,  
              #se = FALSE
  )

## Linear regression models syntax ---- 
# fitting linear model
lm(nhanes$bmi ~ nhanes$age)

# or equivalently...
lm(bmi ~ age, data = nhanes)

# We can save the model and then extract individual output elements from it using the `$` syntax 

# name the model object
lr_model <- lm(bmi ~ age, data = nhanes)
# extract model output elements
lr_model$coefficients
lr_model$residuals
lr_model$fitted.values

# The command `summary` returns these elements 
  # + `Call`: reminds the equation used for this regression model
  # + `Residuals`: a 5 number summary of the distribution of residuals from the regression  model
  # + `Coefficients`:displays the estimated coefficients of the regression model and relative  hypothesis testing, given for:  
  #   + intercept 
  # + explanatory variable(s) slope
summary(lr_model)


## Linear regression models interpretation: coefficients  ---- 
summary(lr_model)$coefficients 

## Linear regression models outputs: fitted values  ---- 
lr_model$fitted.values

##  Linear regression models outputs: residuals ---- 
lr_model$residuals



# Evaluating a simple regression model   ---- 



##  Linear regression model's fit: Residual standard error ---- 

# Calculate 'by hand' the Residual Standard error (Like Standard Deviation)

# ---  inputs 
# sample size
n =length(lr_model$residuals)
# n of parameters in the model
k = length(lr_model$coefficients)-1 #Subtract one to ignore intercept
# degrees of freedom of the the residuals 
df_resid = n-k-1
# Squared Sum of Errors
SSE =sum(lr_model$residuals^2) # 22991.19

# --- Residual Standard Error
ResStdErr <- sqrt(SSE/df_resid)  # 6.815192
ResStdErr



## Linear regression model's fit: : $R^2$ and $Adj. R^2$  ---- 
# --- R^2
summary(lr_model)$r.squared
# --- Adj. R^2
summary(lr_model)$adj.r.squared



## Linear regression model's fit: : F statistic  ---- 
# extract only F statistic 
summary(lr_model)$fstatistic 

# this isn't readily available, but we can define function to extract overall p-value of model
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# extract overall p-value of model
overall_p(lr_model)

# Given the **p-value is > 0.05**, this indicate that *the predictor variable is not useful for predicting the value of the response variable*.


# DIAGNOSTIC PLOTS  -------------------------------------------------------


##  Linear regression diagnostic plots: residuals 1/4 ---- 
# residual plot
plot(lr_model, which = 1 )

## Linear regression diagnostic plots: normality of residuals 2/4  ---- 
# quantile-quantile plot
plot(lr_model, which = 2 )

##  Linear regression diagnostic plots: Homoscedasticity 3/4 ---- 
# Spread-location plot
plot(lr_model, which = 3 )


## (Adding) test for Homoscedasticity   ---- 
# Breusch-Pagan test against heteroskedasticity 
lmtest::bptest(lr_model)
# Because the test statistic (BP) is small and the p-value is not significant  (p-value > 0.05): **WE DO NOT REJECT THE NULL HYPOTHESIS** (i.e. we can assume equal variance)

##  Linear regression diagnostic plots: leverage 4/4 ---- 
plot(lr_model, which = 5 )
# In this particular case, there is no influential case, or cases


# (Digression on the `broom` package) -------------------------------------

# Just keep this in mind as it can be quite helpful (especially when doing further analysis on model outputs)

# The function `tidy` will turn an object into a tidy tibble
# render model as a dataframe 
broom::tidy(lr_model)

# The function `glance` will construct a single row summary "glance" of a model, fit, or other object
# see overal performance 
broom::glance(lr_model)

# The function `augment` will show a lot of results for the model attached to each observation 
# this is very useful for further use of such objects, like `ggplot2` etc. 
# save an object with all the model output elements 
model_aug <- broom::augment(lr_model)
View(model_aug)


# MULTIPLE LINEAR REGRESSION ----------------------------------------------




## Visualize the data: Statin use and cognitive function [PREVEND]  ---- 
# rename for convenience
prevend <- prevend_samp %>% janitor::clean_names() %>% 
  #create statin.use logical + factor
  mutate(statin_use = as.logical(statin)) %>% 
  mutate(statin_use_f = factor(statin, levels = c(0,1), labels = c("NonUser", "User")))   

# box plot 
ggplot(prevend, 
       aes (x = statin_use_f, y = rfft, fill = statin_use_f)) + 
  geom_boxplot(alpha=0.5) +
  scale_fill_manual(values=c("#005ca1","#9b2339" )) +
  # drop legend and Y-axis title
  theme(legend.position = "none") 

##  Consider Simple Linear regression: Statin use and cognitive function ---- 
t_test_w <- t.test(prevend$rfft[prevend$statin == 1], 
                   prevend$rfft[prevend$statin == 0],
                   # here we specify the situation
                   var.equal = TRUE,
                   paired = FALSE, alternative = "two.sided") 

t_test_w
# (statistically significant difference in means is confirmed)...


## Consider Simple Linear regression: Statin use and cognitive function  ---- 
#fit the linear model
model_1 <- lm(rfft ~ statin, data=prevend)
summary(model_1)

#  This preliminary model shows that, on average, statin users score approximately 10 points lower on the RFFT cognitive test
# However, following the literature, this prelimary model might be misleading (`biased`) because it # does not account for the underlying relationship between age and statin  


##  Visualize the data: Statin use and cognitive function + age ---- 
ggplot(prevend, 
       aes (x = age, y = rfft, group = statin_use)) + 
  geom_point (aes(color = statin_use , size=.01, alpha = 0.75),
              show.legend = c(size = F, alpha = F) )+
  scale_color_manual(values=c("#005ca1","#9b2339" )) + 
  # decades line separators 
  geom_vline(xintercept = 40, color = "#A6A6A6")+
  geom_vline(xintercept = 50, color = "#A6A6A6")+
  geom_vline(xintercept = 60, color = "#A6A6A6")+
  geom_vline(xintercept = 70, color = "#A6A6A6")+
  geom_vline(xintercept = 80, color = "#A6A6A6") 


## Multiple linear regression model  ---- 
# fit the (multiple) linear model
model_2 <- lm(rfft ~ statin + age , data=prevend)

## examine the model results 
summary(model_2)

# Evaluating a multiple regression model   ---- 

# [THIS IS MEANINGFUL !]
# assess linearity
plot(residuals(model_2) ~ prevend$age,
     main = "Residuals vs Age in PREVEND (n = 500)",
     xlab = "Age (years)", ylab = "Residual",
     pch = 21, col = "cornflowerblue", bg = "slategray2",
     cex = 0.60)
abline(h = 0, col = "red", lty = 2)

# [THIS IS NOT MEANINGFUL !]
#assess linearity
plot(residuals(model_2) ~ prevend$statin,
     main = "Residuals vs Age in PREVEND (n = 500)",
     xlab = "Age (years)", ylab = "Residual",
     pch = 21, col = "cornflowerblue", bg = "slategray2",
     cex = 0.60)
abline(h = 0, col = "red", lty = 2)

##  Using residual plots to assess CONSTANT VARIABILITY ---- 
#assess constant variance of residuals
plot(residuals(model_2) ~ fitted(model_2),
     main = "Resid. vs Predicted RFFT in PREVEND (n = 500)",
     xlab = "Predicted RFFT Score", ylab = "Residual",
     pch = 21, col = "cornflowerblue", bg = "slategray2",
     cex = 0.60)
abline(h = 0, col = "red", lty = 2)

## Using residual plots to assess NORMALITY of residuals  ---- 
#assess normality of residuals
qqnorm(resid(model_2),
       pch = 21, col = "cornflowerblue", bg = "slategray2", cex = 0.75,
       main = "Q-Q Plot of Residuals")
qqline(resid(model_2), col = "red", lwd = 2)

## Assumption of NO MULTICOLLINEARITY (with `performance`) ---- 

# return and store a list of single plots
diagnostic_plots <- plot(performance::check_model(model_2, panel = FALSE))

# see multicollinearity plot 
diagnostic_plots[[5]]

# in fact the package `performance` provides a great alternative to make diagnostic plots!!! 

# Diagnostic plot of linearity
diagnostic_plots[[2]]
# Diagnostic plot of influential observations - outliers
diagnostic_plots[[4]]
# Diagnostic plot of normally distributed residuals
diagnostic_plots[[6]]


## R^2 with multiple regression  ---- 
#extract R^2 of a model
summary(model_2)$r.squared


## Adj R^2 with multiple regression  ---- 
#extract adjusted R^2 of a model
summary(model_2)$adj.r.squared


# INTRODUCING SPECIAL KINDS OF PREDICTORS -------------------------------



## Categorical predictor in regression - (example | prevend)  ---- 
# convert Education to a factor
prevend <- prevend %>% 
  mutate(educ_f = factor(education,
                         levels = c(0, 1, 2, 3),
                         labels = c("Primary", "LowerSecond",
                                    "HigherSecond", "Univ")))
 
# load 4 color palette
pacificharbour_shades <- c( "#d4e6f3",  "#9cc6e3", "#5b8bac", "#39576c",  "#16222b")

#create plot
plot(rfft ~ educ_f, data = prevend,
     xlab = "Educational Level", ylab = "RFFT Score",
     main = "RFFT by Education in PREVEND (n = 500)",
     names = c("Primary", "LowSec", "HighSec", "Univ"),
     col = pacificharbour_shades[1:4])

##  Categorical predictor in regression - model ---- 
# calculate group means
prevend %>% 
  group_by(educ_f) %>% 
  summarise(avg_RFFT_score = mean(rfft))

# Fitting a model with `education` as a predictor
# fit a model
model_cat <- lm(rfft ~ educ_f, data = prevend) 

# model results
summary(model_cat)
# + Notice how `Primary` level of `educ_f` does NOT appear as a coefficient 

##  Interaction in regression - preliminary model (example | nhanes) ---- 
# Going back to the NHANES dataset 

# Let's consider a preliminary model WITHOUT interaction terms
# fit a model
model_NOinterac <- lm(tot_chol ~ age + diabetes, data = nhanes) 
summary(model_NOinterac)

##  Interaction in regression - visual comparison ---- 
# 1)  Users in two categories are represented points; linear relationship is represented by ONE golden line for ALL SAMPLE
ggplot(nhanes, 
       aes (x = age, y = tot_chol)) + 
  # For POINTS I split by category (category)
  geom_point (aes(color = diabetes, 
                  alpha = 0.75),
              show.legend = c(size = F, alpha = F))+
  scale_color_manual(values=c("#005ca1","#9b2339" )) + 
  # For SMOOTHED LINES I take ALL data
  geom_smooth(colour="#BD8723",  method = lm, se = FALSE) 

# 2) Users in two categories are represented points; linear relationship is represented by 2 respective line according to diabetes status... the association has DIFFERENT DIRECTION!
ggplot(nhanes, 
       # For *both POINTS & LINES* I split by category (category)
       aes (x = age, y = tot_chol, color = diabetes)) + 
  geom_point (aes(alpha = 0.75),
              show.legend = c(size = F, alpha = F) )+
  geom_smooth(method = lm, se = FALSE)+ 
  scale_color_manual(values=c("#005ca1","#9b2339" ))  


## Interaction in regression - adding term in model ---- 
# Let's rethink the model and consider this new *specification* including an `interaction term` between `diabetes` status and `age`

# possible Diabetes status
table(nhanes$diabetes)

# fit a model (* indicates allowing 2 terms to interact)
model_interac2 <- lm(tot_chol ~ age*diabetes, data = nhanes)
summary(model_interac2)

# Now we have also "age:diabetesYes" among the coefficients where "diabetesYes" takes on 2 values (No, Yes) 
# Diabetes = "No" is taken as the reference level

# EXTRA CHECK 
# Let's re-do the plot but using the prediction model coefficients 
ggplot(nhanes, 
       # For *both POINTS & LINES* I split by category (category)
       aes (x = age, y = tot_chol, col = diabetes)) + 
  geom_point (aes(alpha = 0.75),
              show.legend = c(size = F, alpha = F) )+
  scale_color_manual(values=c("#2D337C","#e68000"))  + 
  # using a manual line instead of the geom_smooth [REFERENCE level: NO ]
  geom_abline(aes(
    intercept = 4.695703,
    slope = 0.009638,
    col = "No"
  )) + 
  # using a manual line instead of the geom_smooth [REFERENCE level: YES]
  geom_abline(aes(
    intercept = 4.695703 + 1.718704, # adding/subtracting *diabetesYes* coeff 
    slope = 0.009638 - 0.033452, # adding/subtracting *age:diabetesYes* coeff 
    col = "Yes"
  ))

# This is fairly close to what `ggplot2::geom_smooth` gave us before 
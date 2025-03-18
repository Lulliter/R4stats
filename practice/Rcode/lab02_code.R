# Lab 2 STATISTICAL INFERENCE & HYPOTHESIS TESTING------------------------------
# Practice session covering topics discussed in Lecture 2 


# GOAL OF TODAY'S PRACTICE SESSION ----------------------------------------
# Consolidate understanding of inferential statistic, through R coding examples conducted on real biostatistics research data. 


# _________---------------------------------------------------------------------

# R ENVIRONMENT SET UP & DATA ---------------------------------------------

##  Load pckgs for this R session ---------------------------------------------
# General 
library(fs)           # file/directory interactions
library(here)         # tools find your project's files, based on working directory
library(janitor)      # tools for examining and cleaning data
library(dplyr)        # {tidyverse} tools for manipulating and summarising tidy data 
library(forcats)      # {tidyverse} tool for handling factors

# Statistics
library(BSDA)   
library(rstatix)      # Pipe-Friendly Framework for Basic Statistical Tests
library(car)          # Companion to Applied Regression
library(multcomp)     # Simultaneous Inference in General Parametric Models 

# Plotting
library(ggplot2)      # {tidyverse} tools for plotting
library(ggstatsplot)  # 'ggplot2' Based Plots with Statistical Details 
library(ggpubr)       # 'ggplot2' Based Publication Ready Plots 
library(patchwork)    # Functions for ""Grid" Graphics"composing" plots 
library(viridis)      # Colorblind-Friendly Color Maps for R 
library(ggthemes)    # Extra Themes, Scales and Geoms for 'ggplot2'
 

## Our dataset for today  ---------------------------------------------
## Importing from your project folder (previously downloaded file) ---------------------------------------------
# Make sure to match your own folder structure (argument of function `here`)

# Use `here` in specifying all the subfolders AFTER the working directory 
heart_failure <- read.csv(file = here::here("practice", "data_input", "02_datasets",
                                            "heart_failure_clinical_records_dataset.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 


# _________---------------------------------------------------------------------

# INSPECTING THE "HEART FAILURE" DATASET  ---------------------------------------------

##  What are the variables and their levels of measurement? ---------------------------------------------

# What variables are included in this dataset?
colnames(heart_failure)
# How many observations & variables?
nrow(heart_failure)
# How many rows & columns?
dim(heart_failure)
 

## Inspect the dataframe structure (`base R`)  ---------------------------------------------

# What does the dataframe look like?
str(heart_failure)

## Inspect the dataframe structure (`skimr`) `)  ---------------------------------------------

# some variables 
heart_failure %>% skimr::skim( age, DEATH_EVENT ) 

# the whole dataframe
heart_failure %>% skimr::skim() 

## Recode some variables for later ease of analysis   ---------------------------------------------

# + using tidyverse packages `dplyr` and `forcats`
# + adding new (recoded) variables called *"`oldname_f`"* 
 
heart_failure <-heart_failure %>% 
  dplyr::mutate(DEATH_EVENT_f = as.factor(DEATH_EVENT) %>%
                  forcats::fct_recode("died" = "1", "survived" = "0")) %>% 
  dplyr::mutate(sex_f = as.factor(sex) %>%
                  forcats::fct_recode("male" = "1", "female" = "0"))

# check 
table(heart_failure$DEATH_EVENT_f)
table(heart_failure$sex_f)



## [Some more dummy variables recoded as factor   ---------------------------------------------
#It's worth learning the useful function `dplyr::across`^[This is a bit more [advanced](https://dplyr.tidyverse.org/articles/colwise.html), but it will save a lot of typing in some situations...], which allows to iteratively transform several columns at once!
 
# Recode as factor with levels "yes" (= 1), "no" (= 0)
fct_cols = c("anaemia", "diabetes", "high_blood_pressure", "smoking" )

heart_failure <- heart_failure  %>% 
  ## ---- 1st create new cols as "factor versions" of old cols
  dplyr::mutate(
    # let's introduce `across` function 
dplyr::across(
  # Columns to transform
  .cols = all_of(fct_cols), 
  # Functions to apply to each col  
  .fns =  ~as.factor (.x),
  # new name to apply where "{.col}" stands for the selected column
  .names = "{.col}_f")) %>% 
  ## ---- 2nd create new cols as "factor versions" of old cols
  dplyr::mutate(
    dplyr::across(
      # Columns to transform 2 conditions 
      .cols = ends_with("_f") & !matches(c( "DEATH_EVENT_f", "sex_f" )) , 
      # Functions to apply to each col(different syntax)
      .fns = ~forcats::fct_recode(.x,  yes = "1", no = "0" )))
 
 

# _________---------------------------------------------------------------------

# [VISUAL DATA EXPLORATION FOR THE "HEART FAILURE"] ---------------------------------------------
 
  
## Age    ---------------------------------------------
# Introducing the handy R package `patchwork` which lets us compose different plots in a very simple and intuitive way

age <-ggplot(heart_failure,aes(x = age ))+
  geom_histogram(binwidth = 5, color = "white", fill = "grey",alpha = 0.5)+
  geom_vline(aes(xintercept = mean(age)), color = "#4c4c4c")+
  theme_fivethirtyeight()+
  labs(title = "Age Distribution" )+
  scale_x_continuous(breaks = seq(40,100,5))  

age2 <-ggplot(heart_failure, aes(x = age, fill = DEATH_EVENT_f))+
  geom_histogram(binwidth = 5, position = "identity",alpha = 0.5,color = "white")+
  geom_vline(aes(xintercept = mean(age[DEATH_EVENT == 0])), color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(age[DEATH_EVENT==1])), color = "#d8717b")+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  labs(title =  "Age Distribution by group (Death Event)")+
  scale_x_continuous(breaks = seq(40,100,5))

# patchwork
library(patchwork)
age + age2 + patchwork::plot_layout	(ncol = 1)




## Creatinine Phosphokinase (CPK)    ---------------------------------------------

#| output-location: slide
#| fig-cap: "This definitely doesn't look like a normal distribution!" 
#| fig-cap-location: bottom

cpk <- ggplot(heart_failure,aes(x = creatinine_phosphokinase))+
  geom_density(fill = "gray", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,8000, 500))+
  geom_vline(aes(xintercept = mean(creatinine_phosphokinase)), color = "#4c4c4c")+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle=50, vjust=0.75))+
  labs(title = "Creatinine phosphokinase (density distribution)" )+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

cpk2 <- ggplot(heart_failure,aes(x = creatinine_phosphokinase,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  scale_x_continuous(breaks = seq(0,8000, 500))+
  geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT==1])), 
             color = "#d8717b")+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle=50, vjust=0.75))+
  labs(title =  "Creatinine phosphokinase (density distribution) by group (Death Event)")

cpk + cpk2 + plot_layout(ncol = 1)




## Ejection Fraction     ---------------------------------------------


#| output-location: slide
#| fig-cap: "This also doesn’t look like a normal distribution... and there is a remarkable change in the *probability density function* (PDF) shape when we introduce the grouping variable" 
#| fig-cap-location: bottom

ejf <- ggplot(heart_failure,aes(x = ejection_fraction))+
  geom_density(fill = "gray", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,100, 5))+
  geom_vline(aes(xintercept = mean(ejection_fraction)), color = "#4c4c4c")+
  theme_fivethirtyeight()+
  labs(title = "Ejection Fraction (density distribution)" )+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

ejf2 <- ggplot(heart_failure,aes(x = ejection_fraction,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+theme_fivethirtyeight()+
  scale_x_continuous(breaks = seq(0,100, 5))+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  geom_vline(aes(xintercept = mean(ejection_fraction[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(ejection_fraction[DEATH_EVENT==1])), 
             color = "#d8717b")+
  labs(title =  "Ejection Fraction (density distribution) by group (Death Event)")+
  theme_fivethirtyeight()

ejf + ejf2 + plot_layout(ncol = 1)





## Platelets  ---------------------------------------------


#| output-location: slide
#| fig-cap: "Here the probability distributions resemble a Normal one and we observe more uniformity in the mean/variance across the 2 groups" 
#| fig-cap-location: bottom

# normalize the var for readability 
heart_failure  <-  heart_failure %>%  dplyr::mutate(plat_norm = platelets/1000) 

plat <- ggplot(heart_failure,aes(x = plat_norm))+
  geom_density(fill = "gray", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,800, 100))+
  geom_vline(aes(xintercept = mean(plat_norm)), color = "#4c4c4c")+
  theme_fivethirtyeight()   + 
  labs(title =  "Platelets (density distribution)",
       y = "Density", x = "Sample platelet count (in 10^3 µL)") 

plat2 <- ggplot(heart_failure,aes(x = plat_norm,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+theme_fivethirtyeight()+
  scale_x_continuous(breaks = seq(0,800, 100))+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  geom_vline(aes(xintercept = mean(plat_norm[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(plat_norm[DEATH_EVENT==1])), 
             color = "#d8717b")+
  theme_fivethirtyeight()   + 
  labs(title =  "Platelets (density distribution) by group (Death Event)",
       caption = "(Sample platelet count in 10^3 µL)") 

plat + plat2 + plot_layout(ncol = 1)



## Serum Creatinine  ---------------------------------------------


#| output-location: slide
#| fig-cap: "Another continuous random variable with a non-normal distribution (long right tails) and a seemingly important difference in variance between the groups. " 
#| fig-cap-location: bottom

ser_cr <- ggplot(heart_failure,aes(x = serum_creatinine))+
  geom_density(fill = "gray", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,10, 1))+
  geom_vline(aes(xintercept = mean(serum_creatinine)), color = "#4c4c4c")+
  theme_fivethirtyeight()+
  labs(title = "Serum Creatinine (density distribution)" )+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

ser_cr2 <- ggplot(heart_failure,aes(x = serum_creatinine,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+theme_fivethirtyeight()+
  scale_x_continuous(breaks = seq(0,10, 1))+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  geom_vline(aes(xintercept = mean(serum_creatinine[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(serum_creatinine[DEATH_EVENT==1])), 
             color = "#d8717b")+
  labs(title =  "Serum Creatinine (density distribution) by group (Death Event)")+
  theme_fivethirtyeight()

ser_cr + ser_cr2 + plot_layout(ncol = 1)



## Serum Sodium  ---------------------------------------------


#| output-location: slide
#| fig-cap: "Same as above, except for the long left tails..." 
#| fig-cap-location: bottom

ser_sod <- ggplot(heart_failure,aes(x = serum_sodium))+
  geom_density(fill = "gray", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,150, 5))+
  geom_vline(aes(xintercept = mean(serum_sodium)), color = "#4c4c4c")+
  theme_fivethirtyeight()+
  labs(title = "Serum Sodium (density distribution)" )

ser_sod2 <- ggplot(heart_failure,aes(x = serum_sodium,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,150, 5))+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  geom_vline(aes(xintercept = mean(serum_sodium[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(serum_sodium[DEATH_EVENT==1])), 
             color = "#d8717b")+
  theme_fivethirtyeight()+
  labs(title =  "Serum Sodium (density distribution) by group (Death Event)")+
  theme_fivethirtyeight()

ser_sod + ser_sod2 + plot_layout(ncol = 1)



  

# _________---------------------------------------------------------------------

#  VISUAL DATA EXPLORATION FOR THE "HEART FAILURE" [**DISCRETE VARIABLES**]  -------------------------------
 
## Anaemia **DISCRETE VARIABLES** ---------------------------------------------
  
#| output-location: slide
#| fig-cap: "There seems to be a greater incidence of anaemia in group 'died'" 

anem <- ggplot(heart_failure, aes(x = forcats::fct_infreq(DEATH_EVENT_f ), 
                                  fill = anaemia_f ))+
  geom_bar(position = "dodge")+
  ## add count labels
  geom_text(stat = "count", aes(label = ..count..),
            ## make labels suit the dodged bars 
            position=position_dodge(width = 1 ), 
            hjust=0.5, vjust=2,color = "white") +
  theme_fivethirtyeight() +
  #scale_x_discrete(labels  = c("Death Event:No","Death Event:Yes"))+
  scale_fill_manual(values = c("#af854f", "#af4f78"),
                    name = "Has Anaemia",
                    labels = c("No","Yes"))+
  labs(title = "Number of Patients with Anemia") + 
  theme(#axis.text.x = element_text(angle=50, vjust=0.75), 
    axis.text.x = element_text(size=12,face="bold"))     

anem


## Diabetes ---------------------------------------------

#| output-location: slide

diab <- ggplot(heart_failure, 
               aes(x = forcats::fct_infreq(DEATH_EVENT_f ), fill = diabetes_f ))+
  geom_bar(position = "dodge")+
  ## add count labels
  geom_text(stat = "count", aes(label = ..count..),
            ## make labels suit the dodged bars 
            position=position_dodge(width = 1 ), 
            hjust=0.5, vjust=2,color = "white", size =4) +
  theme_fivethirtyeight() +
  #scale_x_discrete(labels  = c("Death Event:No","Death Event:Yes"))+
  scale_fill_manual(values = c("#af854f", "#af4f78"),
                    name = "Has Diabetes",
                    labels = c("No","Yes"))+
  labs(title = "Number of Patients with Diabetes") + 
  theme(#axis.text.x = element_text(angle=50, vjust=0.75), 
    axis.text.x = element_text(size=12,face="bold"))     

diab


## Smoking ---------------------------------------------

#| output-location: slide

smok <- ggplot(heart_failure, aes(x = forcats::fct_infreq(DEATH_EVENT_f ), 
                                  fill = smoking_f ))+
  geom_bar(position = "dodge")+
  ## add count labels
  geom_text(stat = "count", aes(label = ..count..),
            ## make labels suit the dodged bars 
            position=position_dodge(width = 1 ), 
            hjust=0.5, vjust=2,color = "white", size =4) +
  theme_fivethirtyeight() +
  #scale_x_discrete(labels  = c("Death Event:No","Death Event:Yes"))+
  scale_fill_manual(values = c("#af854f", "#af4f78"),
                    name = "Patient smokes",
                    labels = c("No","Yes"))+
  labs(title = "Number of Patients who smoke") + 
  theme(#axis.text.x = element_text(angle=50, vjust=0.75), 
    axis.text.x = element_text(size=12,face="bold"))     

smok


## High blood pressure ---------------------------------------------

#| output-location: slide
#| fig-cap: "There is also a greater incidence of high blood pressure in group 'died'" 

hbp <- ggplot(heart_failure, aes(x = forcats::fct_infreq(DEATH_EVENT_f ), 
                                 fill = high_blood_pressure_f ))+
  geom_bar(position = "dodge")+
  ## add count labels
  geom_text(stat = "count", aes(label = ..count..),
            ## make labels suit the dodged bars 
            position=position_dodge(width = 1 ), 
            hjust=0.5, vjust=2,color = "white", size =4) +
  theme_fivethirtyeight() +
  #scale_x_discrete(labels  = c("Death Event:No","Death Event:Yes"))+
  scale_fill_manual(values = c("#af854f", "#af4f78"),
                    name = "Has high blood pressure",
                    labels = c("No","Yes"))+
  labs(title = "Number of Patients with High blood pressure") + 
  theme(#axis.text.x = element_text(angle=50, vjust=0.75), 
    axis.text.x = element_text(size=12,face="bold"))     

hbp



# _________---------------------------------------------------------------------

# HYPOTHESIS TESTNG - [EXAMPLE A ] - 1 sample | n > 30 |  Z test---------------------------------------------
## [Comparing sample mean to a hypothesized population mean (with Z test)]---------------------------------------------

## [1. Question: How does the mean platelets count in the patients’ sample compare against a reference population?]#

# compute mean & sd for plot
mean_plat_p <- round(mean(heart_failure$plat_norm), digits = 1)
sd_plat_p <- round(sd(heart_failure$plat_norm), digits = 1)

heart_failure %>% 
  ggplot(aes(x = plat_norm))+
  geom_histogram(aes(y = ..density..), bins=30, alpha=0.25, colour = "#4c4c4c") + 
  geom_density(colour ="#9b2339", alpha=0.25, fill = "#9b2339") +
  # add mean vertical line
  geom_vline(xintercept = mean_plat_p, na.rm = FALSE,size = 1,color= "#9b6723") +
  # add also +/- 1sd  
  geom_vline(aes(xintercept = mean_plat_p + sd_plat_p), 
             color = "#23749b", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean_plat_p - sd_plat_p), 
             color = "#23749b", size = 1, linetype = "dashed") +
  # add annotations with the mean value
  geom_label(aes(x=mean_plat_p,  y=0.0085, label=paste0("Sample mean\n",mean_plat_p)),
             color = "#9b6723") + 
  geom_label(aes(x=361,  y=0.0085, label=paste0("Sample sd\n",sd_plat_p)),
             color = "#23749b") +
  theme_bw() +  labs(y = "Density", x = "Sample platelet count (x 1000/µL)") 
   
## 2.a Computation of the test statistic  ---------------------------------------------

# General Population of reference 
mu <- 236 
sigma  <- 59
# Sample of HF patients
n <- 299
x_HF <- mean(heart_failure$plat_norm)         #    263.358
s_HF <- sd(heart_failure$plat_norm)           #    97.80424
# IF large sample & KNOWN pop variance 
std_err_HF <- sigma /sqrt(n)                  # 3.412058
z_calc_HF <-  (x_HF - mu) / std_err_HF        # 8.018043
 
## 2.b Computation of the p-value associated to the test statistic  ---------------------------------------------
# To find the **p-value** associated with a z-score in R, we can use the `pnorm()` function, which uses the following syntax:

# Left-tailed test
p_value_l <- stats::pnorm(z_calc_HF, mean = 0, sd = 1, lower.tail = TRUE) 
# Right-tailed test
p_value_r <- stats::pnorm(z_calc_HF, mean = 0, sd = 1,lower.tail = FALSE) 
# Two-tailed test  (our case)
p_value_two <- 2*stats::pnorm(z_calc_HF, mean = 0, sd = 1, lower.tail = FALSE)  


## 2.c Computation of the p-value associated to the test statistic  ---------------------------------------------
z_test_summary <- BSDA::z.test(x = heart_failure$plat_norm,   
                               alternative='two.sided', 
                               mu=236, 
                               sigma.x=59, 
                               conf.level=.95)
z_test_summary
#Same results! 
  
## 3. Results and interpretation ---------------------------------------------
# given 
z_critical  <- c(-1.96, +1.96) # (Z score corresponding to 𝛼  = 0.05)
# Check 
z_calc_HF > z_critical 
# Check
p_value_two <  0.05


# HYPOTHESIS TESTNG - [EXAMPLE B ] - 1 sample | n < 30 | t test ---------------------------------------------
## Comparing sample mean to a hypothesized population mean (with t test)  ---------------------------------------------

# normalize the var for readability 
heart_21d  <-  heart_failure %>%  dplyr::mutate(plat_norm = platelets/1000) %>% 
  filter(time <= 21)                                # 23 obs 
# compute mean & sd for plot
mean_plat_p <- round(mean(heart_21d$plat_norm), digits = 1)
sd_plat_p <- round(sd(heart_21d$plat_norm), digits = 1)

heart_21d %>% 
  ggplot(aes(x = plat_norm))+
  geom_histogram(aes(y = ..density..), bins=30, alpha=0.25, colour = "#4c4c4c") + 
  geom_density(colour ="#9b2339", alpha=0.25, fill = "#9b2339") +
  # add mean vertical line
  geom_vline(xintercept = mean_plat_p, na.rm = FALSE,size = 1,color= "#9b6723") +
  # add also +/- 1sd  
  geom_vline(aes(xintercept = mean_plat_p + sd_plat_p), 
             color = "#23749b", size = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean_plat_p - sd_plat_p), 
             color = "#23749b", size = 1, linetype = "dashed") +
  # add annotations with the mean value
  geom_label(aes(x=mean_plat_p,  y=0.014, label=paste0("Sample mean\n",mean_plat_p)),
             color = "#9b6723") + 
  geom_label(aes(x=361,  y=0.014, label=paste0("Sample sd\n",sd_plat_p)),
             color = "#23749b") +
  theme_bw() +  labs(y = "Density", x = "Sample platelet count (x 1000/µL)") 



## 2.a Picking the suitable test    ---------------------------------------------

## 2.b Computation of the test statistic  ---------------------------------------------

# Option 1: Let's compute the t test "by hand"  
# General Population of reference 
mu_pop <- 236 

# SAMPLE HF patients follow up less 21 days 
heart_21d <- heart_failure %>% filter(time <= 21) 

n_21d <- nrow(heart_21d)                            # 23
x_HF_21d <- mean(heart_21d$plat_norm)               # 251.5094
s_HF_21d <- sd(heart_21d$plat_norm)                 # 102.7341
df_HF_21d <- n_21d-1                                # 22   

# IF SMALL sample UNKNOWN sigma
std_err_HF_21d <- s_HF_21d /sqrt(n_21d -1)        # 21.90298
t_calc <-  (x_HF_21d - mu_pop) / std_err_HF_21d   # 0.7080951


# Option 2:  Let's compute the t test  with `stats::t.test`  

t_stat_HF_21d_v2 <- stats::t.test(x = heart_21d$plat_norm,
                                  mu = mu_pop,
                                  alternative = "two.sided")
# extract t_calc from results df
t_calc_v2  <- t_stat_HF_21d_v2[["statistic"]][["t"]] # 0.7240093

## [2.c Computation of the p-value associated to the test statistic] ----------------------------------

 
# + Option 1:  "by hand"  
# -- Left-tailed test
#pt(t_stat_HF_21d, df_HF_21d, lower.tail = TRUE)

# -- Right-tailed test
#pt(t_stat_HF_21d, df_HF_21d, lower.tail = FALSE) 

# -- Two-tailed test  (our case)
p_value_t_test <- 2*pt(t_calc, df_HF_21d, lower.tail = FALSE) # 0.4863214


# + Option 2: from results of `stats::t.test`  
 # extract  p_value from results df
p_value_v2  <- t_stat_HF_21d_v2[["p.value"]] # 0.4766892

## 3. Results and interpretation  ---------------------------------------------

#  Based on the critical region, `t_calc ≃ 0.71` is smaller than the t critical value, i.e. it falls within the region of acceptance, so he null hypothesis is not rejected

#find two-tailed t critical values
t_crit_two <- qt(p=.05/2, df=22, lower.tail=FALSE)    # 2.073873
# Compare t score against t critical    
t_calc > t_crit_two  # FALSE 

# Based on the p-value, `p_value ≃ 0.48` is larger than $\alpha$, i.e. the probability of observing a test statistic (assuming  $H_0$ is true) is quite large
# Check 
p_value_t_test <  0.05  # FALSE 



# _________---------------------------------------------------------------------

# HYPOTHESIS TESTNG - [EXAMPLE C  ] - 2 samples | t test---------------------------------------------
## [Comparing two independent sample means (t test)]  ---------------------------------------------
  
# boxplot by group
heart_failure %>% 
  ggplot(mapping = aes(y = plat_norm, x = DEATH_EVENT_f, fill = DEATH_EVENT_f)) +
  geom_boxplot(alpha=0.5)+ 
  #geom_violin(alpha=0.5) +
  geom_point(position = position_jitter(width = 0.1), size = 0.5)+ 
  scale_fill_manual(values = c("#999999", "#d8717b"))  +
  # drop legend and Y-axis title
  theme(plot.title = element_text(size = 14,face="bold", color = "#873c4a"),
        legend.position = "none",
        axis.text.x = element_text(size=12,face="bold"), 
        axis.text.y = element_text(size=12,face="bold")) + 
  labs(title = "Boxplot of Total Platelet Count (TPL), grouping by DEATH_EVENT [0,1]",
       x = "", y  = "Platelet count (1000 /µL)")


## 2. Verify the assumptions for independent t-test  ---------------------------------------------

## [Preliminary Fisher's F test to check for variance equality]  ---------------------------------------------

# We can compute the Fisher test "by hand"  

## -- data by group
n_died <- nrow(heart_failure[heart_failure$DEATH_EVENT == 1 ,])
mean_died <- mean(heart_failure [ heart_failure$DEATH_EVENT == 1,  "plat_norm"])
sd_died <- sd(heart_failure [heart_failure$DEATH_EVENT == 1 ,  "plat_norm"])
var_died <- var(heart_failure [heart_failure$DEATH_EVENT == 1 ,  "plat_norm"])

n_survived <- nrow(heart_failure[heart_failure$DEATH_EVENT == 0, ])
mean_survived <- mean(heart_failure [ heart_failure$DEATH_EVENT == 0,  "plat_norm"])
sd_survived <- sd(heart_failure [heart_failure$DEATH_EVENT == 0 ,  "plat_norm"])
var_survived <- var(heart_failure [heart_failure$DEATH_EVENT == 0 ,  "plat_norm"])

## -- F TEST
F_ratio <- var_died / var_survived
F_ratio  # 1.020497 


## [Preliminary Fisher's F test to check for variance equality (.cont)]   ---------------------------------------------


## -- Define the critical value of F distribution for a risk of alpha = 0.05
# qf(p=.05, df1 = n_died-1, df2 = n_survived-1, lower.tail = FALSE) # RIGHT-Tailed
# qf(0.95, df1 = n_died-1, df2 = n_survived-1, lower.tail = FALSE) # LEFT- Tailed 
qf(c(0.025, 0.975), df1 = n_died-1, df2 = n_survived-1) # TWO-Tailed 

## --Compute the exact p-value (two-tailed )
p_value_f <- 2 * (1 - pf(F_ratio, df1 = (n_died-1), df2 = (n_survived-1))) 
p_value_f

# The p-value is 0.89, greater than the p-value threshold of 0.05. This suggests **we can not reject the null hypothesis of equal variances**. 
 
# -> we can run a t-test.

## 3.a Computation of t test statistic    ---------------------------------------------
 
# Step 1 - compute difference of sample means
mean_diff <- (mean_died - mean_survived) # -10.27645 

# Step 2 - Compute associated t-statistics
# pooled std error 
pooled_stderror <- sqrt(sd_died^2/(n_died ) + sd_survived^2/(n_survived )) 
# pooled std error corrected
pooled_stderror_corr <- sqrt(sd_died^2/(n_died-1) + sd_survived^2/(n_survived-1)) 

###  t statistic  
t_calc <- (mean_died - mean_survived) / pooled_stderror_corr 


## 3.b Computation of the p-value associated to the t statistic    ---------------------------------------------

# Step 3 - degrees of freedom
# n1 + n2 - number of estimated parameters (2 means)
d_f <- n_died + n_survived - 1 - 1 # 297

# Step 4 - Deduced p-value
p_value <- 2 * pt(t_calc, df = d_f) # 0.4009635
p_value


## 4. Results and interpretation  ---------------------------------------------
mean_diff
# CI of the means difference 
CI_lower <- mean_diff + qt(.025, sum(n_died + n_survived) - 2) * pooled_stderror_corr  
CI_lower
CI_upper <- mean_diff + qt(.975, sum(n_died + n_survived) - 2) * pooled_stderror_corr  
CI_upper

# As for the p-value, `p_value = 0.40` is  bigger than threshold probability $\alpha$  
p_value
p_value <  0.05  # FALSE 


# So, we  fail to reject the null hypothesis of equal populations means of TPC. So the test indicates that we do not have sufficient evidence to say that the mean counts of platelets in between these two populations is different.
 



# _________---------------------------------------------------------------------

# HYPOTHESIS TESTNG - [EXAMPLE D ] - (3+ samples |  ANOVA test) --------------------------------------------
## Comparing sample means from 3 or more groups (ANOVA) ---------------------------------------

# For this particular case, we use another realistic dataset showing the survival times of 33 laboratory mice  with thymic leukemia who were randomly divided into 3 groups: 

#  + 1st group received Treatment 1
#  + 2nd group received Treatment 2
#  + 3rd group as Control

# load new dataset
mice <- readxl::read_excel(here::here("practice","data_input",
                                      "02_datasets","mice_exe_ANOVA.xlsx"))

# boxplot by group
mice %>% 
ggplot(., aes(x = group, y = surv_days, fill = group)) +
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  # theme_minimal() +
  # drop legend and Y-axis title
  theme(plot.title = element_text(size = 14,face="bold", color = "#873c4a"),
        axis.text.x = element_text(size=12,face="bold"), 
        axis.text.y = element_text(size=12,face="bold"),
        legend.position = "none",
        ) + 
  labs(title = "Visually check mean and variance in populations' samples" ) + 
  ylab(label = "Survival (# days") + xlab(label = "")
    
    
## 2. Verify the assumptions for one-way ANOVA ---------------------------------------
 
mice %>% 
  ggplot(., aes( x = surv_days )) +
  geom_density(fill = "gray", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(surv_days)), color = "#4c4c4c") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle=50, vjust=0.75)) +
  labs(title = "Survival days (density distribution)") +
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))



## [Preliminary check for normality (test) with `stats::shapiro.test`]  ---------------------------------------
# Shapiro-Wilk Normality Test to verify normality  
# option 1 
stats::shapiro.test(mice[mice$group == "Control", "surv_days", drop=TRUE])
stats::shapiro.test(mice[mice$group == "Treatment 1", "surv_days", drop=TRUE])
stats::shapiro.test(mice[mice$group == "Treatment 2", "surv_days", drop=TRUE])


## [Preliminary check for normality (test) with `rstatix::shapiro_test`]{ ---------------------------------------
# (same thing, but using a different R function)

# 2. **Normally-distributed response variable**  confirmed by Shapiro-Wilk normality test
# Shapiro-Wilk Normality Test to verify normality  
# option 2 (all 3 groups at once)
mice %>%
      dplyr::group_by(group) %>%
      rstatix::shapiro_test(surv_days)
    
    
## [Preliminary check variance equality ]{.r-stretch}
#  (Besides visual inspection, confirmed  by Levene test for variance equality)

# [The null hypothesis **$H_0$ = several groups have the same variance** (possible variance differences occur only by chance, since there are small differences in each sampling)]


# Levene test for variance equality
levene <- mice %>%                               # name of the data
  car::leveneTest(surv_days ~ as.factor(group),   # continuous DV ~  group IV
                  data = .,            # pipe the data from above
                  center = mean)       # default is median 
levene
## [3 Computation of ANOVA F-ratio]  ---------------------------------------
# ANOVA in R can be done in several ways.

## Option 1: Let's compute the ANOVA test "by hand"  ---------------------------------------

# Summary statistics
mice_calc <- mice %>% 
  dplyr::mutate(mean_all = mean(surv_days),
         sd_all = sd (surv_days),
         dfw = 33-3, # df1 = n-k
         dfb = 3-1, # df2 = K−1 
         group_f = as.factor(group)
         ) %>% 
  dplyr::group_by(group) %>% 
  dplyr::mutate(n_group = n(),
         mean_group = mean(surv_days),
         sd_group = sd (surv_days)) %>% 
  ungroup() %>% 
  mutate (ST = (surv_days - mean_all)^2,
          SW = (surv_days - mean_group)^2,
          SB = (mean_group - mean_all)^2)

# Sum of Squares 
SST <- sum(mice_calc$ST)
SSB <- sum(mice_calc$SB)
SSW <- sum(mice_calc$SW)
dfw <- 33-3  # df2
dfb <- 3-1 # df1

# calculated F statistic 
F_calc <- (SSB/dfb)/(SSW/dfw) # 5.65
# F critical value
F_crit <- qf(p = 0.01, df1 = 2, df2 = 30, lower.tail = FALSE) # 5.390346
 
## [3.b Computation of ANOVA F-ratio (with R functions)]  ---------------------------------------
## Option 2: With the `stats::aov` followed by the command `summary`  ---------------------------------------

aov_1 <- stats::aov(surv_days ~ group_f,
                    data = mice_calc)
summary(aov_1) 


##  Option 3: With the `stats::oneway.test()` function  ---------------------------------------
aov_2 <- stats::oneway.test(surv_days ~ group_f,
                            data = mice_calc,
                            # assuming equal variances
                            var.equal = TRUE)
aov_2


## 4. Results and interpretation  ---------------------------------------
# All 3 options have given the same results, i.e., `F-ratio = 5.652` and a  `p-value = 0.00826`
# **DECISION**: Given that the p-value is smaller than 0.05, we reject the null hypothesis, so we reject the hypothesis that all means are equal. Therefore, we can conclude that *at least one* group is different than the others in mean number of survival days.
 

# _________---------------------------------------------------------------------

# A CLOSER LOOK AT TESTING ASSUMPTIONS   ---------------------------------------
      
# --- EXAMPLE E  -[Testing two groups that are *not* independent]  ----------------------------------------- 
## [Testing two groups that are *not* independent]{.r-fit-text}

# Let's introduce another toy dataset just for demonstration purposes: imagine a statistics test is administered to the *same* group of 12 students **before and after** attending a workshop 😉. 

# toy dataset for paired groups
grades <- data.frame(
  before = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14),
  after = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18)
)

# [For plotting] we need to reshape the dataframe into the long form using `tidyr::pivot_longer`
# reshape into long form
grades_long <- grades %>% 
  dplyr::mutate(id = row_number()) %>%
  tidyr::pivot_longer(cols = before:after, 
                      names_to = "time", 
                      values_to = "grade") %>% 
  dplyr::group_by(id) %>% 
  # recode time as factor 
  dplyr::mutate(time_f = as_factor(time ))  %>% 
  # reorder time_ levels  
  dplyr::mutate(time_f =  fct_relevel(time_f, "after", after =  1))


## [1. Question: Is the difference between two PAIRED samples statistically significant?] -----------------------------
# boxplot by group
grades_long %>% 
  
  ggplot(mapping = aes(y = grade, x = time_f, fill = time_f)) +
  geom_boxplot(alpha=0.5) + 
  #geom_violin(alpha=0.5) +
  geom_point(position = position_jitter(width = 0.1), size = 0.5)+ 
  scale_fill_manual(values = c( "#d8717b", "#239b85"))  +
  # drop legend and Y-axis title
  theme(plot.title = element_text(size = 14,face="bold", color = "#873c4a"),
        legend.position = "none",
        axis.text.x = element_text(size=12,face="bold"), 
        axis.text.y = element_text(size=12,face="bold")) + 
  labs(title = "Boxplot of test grades grouped as before and after",
       x = "", y  = "")


## [2 Computation of the Wilcoxon signed-rank test for dependent samples]  --------------------------------------- 
# In this example, it is clear that the two samples are not independent since the same 12 students # took the test before and after the workshop.
# 
# Given that the normality assumption is NOT violated (and given the small sample size), we use the # **paired t-test**, with the following hypotheses:
  
t_stat_paired <- stats::t.test(x = grades$before,
                               y = grades$after, 
                               mu = 0, 
                               alternative = "two.sided",
                               paired = TRUE
)
t_stat_paired

# extract t_calc from results df
t_calc_pair   <- t_stat_paired[["statistic"]][["t"]] # -1.877683
p_value_pair   <- t_stat_paired[["p.value"]] # 0.08717703


## 3. Results and interpretation ----------------------------------------
# We obtain the test statistic, the p-value and a reminder of the hypothesis tested.
# The calculated **t value** is -1.877683 The **p-value** is `0.08717703. Therefore, at the 5% significance level, **we do not reject the null hypothesis** that the statistics' grades are # similar before and after the workshop (😭).

## Bonus function! 
# load package
library(ggstatsplot)

# plot with statistical results
grades_long %>% 
  # must ungroup the dataframe or it will give an error
  ungroup () %>% 
  ggstatsplot::ggwithinstats(.,
                             x = time_f ,
                             y = grade ,
                             type = "parametric", # for t test
                             centrality.plotting = FALSE # remove median
  )


# --- EXAMPLE F  **(2 samples no normal | Wilcoxon Rank Sum Test)**  ---------------------------------------
## Testing samples *without* normality assumption
  
# Let’s go back to the HEART FAILURE dataset but looking at the levels of **Creatinine Phosphokinase (CPK)** in the blood, an enzyme that might indicate a heart failure or injury


## [1. Question: Is there a statistically significant difference between CPK levels in the blood of the survivors v. those who died after heart failure?] ---------------------------------------

ggplot(heart_failure,aes(x = creatinine_phosphokinase,fill = DEATH_EVENT_f))+
  geom_density(alpha = 0.5)+theme_fivethirtyeight()+
  scale_fill_manual(values = c("#999999", "#d8717b"))+
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(0,8000, 500))+
  geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT == 0])),
             color = "#4c4c4c")+
  geom_vline(aes(xintercept = mean(creatinine_phosphokinase[DEATH_EVENT==1])), 
             color = "#d8717b")+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle=50, vjust=0.75))+
  labs(title =  "Creatinine phosphokinase (density distribution) by group (Death Event)") + 
  theme(plot.title = element_text(size = 14,face="bold", color = "#873c4a"))


## [Preliminary check for normality (visual)] ---------------------------------------
#  **Normally-distributed response variable** - ❌

# **QQ plot** (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted. In a QQ plot, each observation is plotted as a single dot. 
# If the data are normal, the dots should form a straight line.

 
# visual verification with QQ plot 
ggpubr::ggqqplot( 
  heart_failure$creatinine_phosphokinase, 
  title = "QQ plot for CPK levels in blood",
  xlab ="Theoretical", ylab = "Sample (CPK)")

## [Preliminary check for normality (test) with `rstatix::shapiro_test`] ---------------------------------------

#(same thing, but using a different R function)
# Given the p-value we reject the null hypothesis

# Shapiro-Wilk Normality Test to verify normality  
heart_failure %>%
  dplyr::group_by(DEATH_EVENT_f) %>%
  rstatix::shapiro_test(creatinine_phosphokinase)


## [3. Computation of the Wilcoxon Rank Sum test statistic]  ---------------------------------------
# The **Wilcoxon Rank Sum test** is considered to be the nonparametric equivalent to the **two-sample independent t-test**

wrs_res <- wilcox.test(creatinine_phosphokinase ~ DEATH_EVENT, # immagino 0, 1
                       data = heart_failure ,
                       exact = FALSE, 
                       alternative = "two.sided" )
wrs_res

## [4. Results and interpretation]  ---------------------------------------
#   RESULTS: since the test statistic is `W = 9460` and the corresponding `p-value is 0.684 > 0.05`, we fail to reject the null hypothesis.
 
# --- EXAMPLE G  --- **(2 samples no HOV | t test with the Welch correction  )**   ------------------------------
## Testing samples *without* homogeneous variance of observations assumption   ------------------------------
  
## [1. Question: Is there a statistically significant difference between serum sodium levels in the blood of the survivors v. those who died after heart failure?]   ------------------------------

## [Preliminary check “HOV” assumption (visual)]  ------------------------------
  
#Compute means and 95% confidence intervals
swstats <- heart_failure %>%
  group_by(DEATH_EVENT_f) %>%
  summarise(count = n(),
            mean = mean(serum_sodium,na.rm=TRUE),
            stddev = sd(serum_sodium, na.rm=TRUE),
            meansd_l = mean - stddev,
            meansd_u = mean + stddev)

#The complete script with some styling added
ggplot(swstats, aes(x=DEATH_EVENT_f, y=mean)) + 
  geom_point(colour = "black" , size = 2) +
  #Now plotting the individual data points before the mean values
  geom_point(data=heart_failure, aes(x=DEATH_EVENT_f, y=serum_sodium, colour = DEATH_EVENT_f), 
             position = position_jitter() ) +
  scale_colour_manual(values = c("#999999","#d8717b") ) +
  #Add the error bars
  geom_errorbar(aes(ymin = meansd_l, ymax = meansd_u), width=0.2, color = "black") +
  labs(title = "Mean (-/+SD) serum sodium (mEq/L) by group", x = "", y = "Serum Sodium") +
  guides(fill = "none")  +
  coord_flip() +
  labs(title =  "Serum Sodium means and 95% confidence intervals by group (Death Event)") + 
  theme(legend.position="none",plot.title = element_text(size = 14,face="bold", color = "#873c4a"))

## [Preliminary check “HOV” assumption (test)]   ------------------------------
    
#  It is always best to use an actual test, so we use also the **Fisher's F test** to verify equal variances of Serum Sodium concentration in the two groups. [In this test **$H_0$ = “the ratio of variances is equal to 1”**] 


f_test_res <- stats::var.test(heart_failure$serum_sodium[heart_failure$DEATH_EVENT == 1] ,
                              heart_failure$serum_sodium[heart_failure$DEATH_EVENT == 0])
f_test_res


# Given the `p-value = 0.007646` (smaller than $\alpha$) we reject the null hypothesis, hence the HOV assumption for the t test does not hold. 


## [2 Computation of the t test with the Welch correction]  ---------------------------------------
# We can still run the **t test but with Welch correction**, i.e. the unequal variance condition is compensated by lowering the df. In fact the documentation (`?t.test`), reads:

# With Welch correction (on by default) Unequal variance is compensated by lowering df
t_test_w <- t.test(heart_failure$serum_sodium[heart_failure$DEATH_EVENT == 1], 
                   heart_failure$serum_sodium[heart_failure$DEATH_EVENT == 0],
                   # here we specify the situation
                   var.equal = FALSE,
                   paired = FALSE, alternative = "two.sided") 

t_test_w

## [3. Results and interpretation]  ------------------------------


# RESULTS: since the test statistic is `t = -3.1645 (with df = 154.01)` and the corresponding `p-value is 0.001872 < 0.05`, we reject the null hypothesis.
# 
# INTERPRETATION: We therefore have sufficient evidence to say that the level of serum sodium levels for dead patients is significantly different than that of survived patients $𝝁_{sersod-died} ≠ 𝝁_{sersod-surv}$  


 
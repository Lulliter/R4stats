# Lab 4: Modeling correlation and regression ------------------------------
# Practice session covering topics discussed in Lecture 4 


# GOAL OF TODAY'S PRACTICE SESSION ----------------------------------------
 
  # + Revisit PCA algorithm explored via MetaboAnalyst, to learn how we can compute it with R 
  # + Understand some key elements of statistical Power Analysis
  # + Introduce how ML approaches deal with available data  


# The examples and datasets in this Lab session follow very closely two sources:
    # 1. The tutorial on "Principal Component Analysis (PCA) in R" by: [Statistics Globe](https://statisticsglobe.com    /principal-component-analysis-r)
    # 2. The materials in support of the "Core Statistics using R" course by: [Martin van Rongen](https://github.com    /mvanrongen/corestats-in-r_tidyverse)


# _________---------------------------------------------------------------------

# R ENVIRONMENT SET UP & DATA ---------------------------------------------

##  Load pckgs for this R session ---------------------------------------------
# --- General 
library(here)     # tools find your project's files, based on working directory
library(dplyr)    # A Grammar of Data Manipulation
library(skimr)    # Compact and Flexible Summaries of Data
library(magrittr) # A Forward-Pipe Operator for R 
library(readr)    # A Forward-Pipe Operator for R 

# Plotting & data visualization 
library(ggplot2)      # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggfortify)     # Data Visualization Tools for Statistical Analysis Results
library(scatterplot3d) # 3D Scatter Plot

# --- Statistics
library(MASS)       # Support Functions and Datasets for Venables and Ripley's MASS
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining
library(rstatix)    # Pipe-Friendly Framework for Basic Statistical Tests

# --- Tidymodels (meta package)
library(rsample)    # General Resampling Infrastructure  
library(broom)      # Convert Statistical Objects into Tidy Tibbles
 

## Our dataset for today  ---------------------------------------------
  # In this tutorial, we will use: 
  #   
  # 1. the biopsy dataset attached to the [`MASS` package](https://cran.r-project.org/web/packages/MASS/MASS.pdf). 
  # 2. a few clean datasets used in the "Core Statistics using R" course by: [Martin van Rongen](https://github.com  /mvanrongen# /corestats-in-r_tidyverse)
  

# _________---------------------------------------------------------------------
## Loading `biopsy` dataset  with description -------------

# (after loading pckg MASS) I can call 
utils::data(biopsy) # actual dataset 

biopsy_desc <- tibble::tribble(
  ~Variable, ~ Type, ~Description,
  #"X" ,  "integer" ,         "row counter ", 
  "ID" ,   "character",             "Sample ID",
  "V1",    "integer 1 - 10",        "clump thickness",       
  "V2",    "integer 1 - 10",        "uniformity of cell size",   
  "V3",    "integer 1 - 10",        "uniformity of cell shape",  
  "V4",    "integer 1 - 10",        "marginal adhesion",              
  "V5",    "integer 1 - 10",        "single epithelial cell size",   
  "V6",    "integer 1 - 10",        "bare nuclei (16 values are missing)",
  "V7",    "integer 1 - 10",        "bland chromatin",             
  "V8",    "integer 1 - 10",        "normal nucleoli",       
  "V9",    "integer 1 - 10",        "mitoses",               
  "class",  "factor"     ,          "benign or malignant" )                 

# check them out 
biopsy_desc
 
## [`biopsy` variables exploration 1/2] -------------
  # The `biopsy` data contains **699 observations of 11 variables**. 
  # The dataset also contains a character variable: `ID`, and a factor variable: `class`, with two levels ("benign" and "malignant").

 
# check variable types
str(biopsy)
   
## [`biopsy` variables exploration 2/2] -------------
# There is also one incomplete variable `V6` 
# + remember the package `skimr` for exploring a dataframe?

# check if vars have missing values
biopsy %>% 
  # select only variables starting with "V"
  skimr::skim(starts_with("V")) %>%
  dplyr::select(skim_variable, 
                n_missing)

## [`biopsy` dataset manipulation] -------------------
  
# We will: 
#   
# + exclude the non-numerical variables (`ID` and `class`) before conducting the PCA.   
# + exclude the individuals with missing values using the `na.omit()` or `filter(complete.cases()` functions.


# We can do both in 2 equivalent ways:
 
# 1/2 new (manipulated) dataset 
data_biopsy <- na.omit(biopsy[,-c(1,11)])
# 2/2 with `dplyr` (more explicit)
# new (manipulated) dataset 
data_biopsy <- biopsy %>% 
  # drop incomplete & non-integer columns
  dplyr::select(-ID, -class) %>% 
  # drop incomplete observations (rows)
  dplyr::filter(complete.cases(.))

## [`biopsy` dataset manipulation]  ---------------------------------------------
# We obtained a new dataset with 9 variables and 683 observations (instead of the original 699).  


# check reduced dataset 
str(data_biopsy)


# PCA: EXAMPLE of UNSUPERVISED ML ALGORITHM  ---------------------------------------------

# Reducing high-dimensional data to a lower number of variables

## Calculate Principal Components  ---------------------------------------------
  # The first step of PCA is to calculate the principal components. To accomplish this, we use the `prcomp()` function from the `stats` package.  
  # 
  # + With argument `“scale = TRUE”` each variable in the biopsy data is scaled to have a mean of `0` and a standard deviation of `1` before calculating the principal components (just like option `Autoscaling` in MetaboAnalyst)



# calculate principal component
biopsy_pca <- prcomp(data_biopsy, 
                     # standardize variables
                     scale = TRUE)
## Analyze Principal Components  ---------------------------------------------

# Let’s check out the elements of our obtained `biopsy_pca` object 
# + (All accessible via the  `$` operator)

names(biopsy_pca)

# **"sdev"** = the standard deviation of the principal components
# **"sdev"\^2** = the variance of the principal components (**eigenvalues** of the covariance/correlation matrix)
# **"rotation"** = the matrix of variable **loadings** (i.e., a matrix whose columns contain the **eigenvectors**).
# **"center"** and **"scale"** = the means and standard deviations of the original variables before the transformation;
# **"x"** = the principal component scores (after PCA the observations are expressed in principal component scores)

## Analyze Principal Components (cont.)  ---------------------------------------------

# We can see the summary of the analysis using the `summary()` function
# 1. The first row gives the **Standard deviation** of each component, which can also be retrieved via `biopsy_pca$sdev`. 
# 2. The second row shows the **Proportion of Variance**, i.e. the percentage of explained variance.

summary(biopsy_pca)

  
## [Proportion of Variance for components]{.r-fit-text}  ---------------------------------------------
  
#  2. The row with **Proportion of Variance** can be either accessed from summary or calculated as follows:
# a) Extracting Proportion of Variance from summary
summary(biopsy_pca)$importance[2,]

# b) (same thing)
round(biopsy_pca$sdev^2 / sum(biopsy_pca$sdev^2), digits = 5)
 
## > The output suggests the **1st principal component** explains around 65% of the total variance, the **2nd principal component** explains about 9% of the variance, and this goes on with diminishing proportion for each component. 


## [Cumulative Proportion of variance for components]{.r-fit-text} --------------------------------------------

# 3. The last row from the `summary(biopsy_pca)`, shows the **Cumulative Proportion** of variance, which calculates the cumulative sum of the Proportion of Variance. 

# Extracting Cumulative Proportion from summary 
summary(biopsy_pca)$importance[3,]
 
## > Once you computed the PCA in R you must decide the number of components to retain based on the obtained results.


# VISUALIZING PCA OUTPUTS  ---------------------------------------------

## Scree plot  ---------------------------------------------

# There are several ways to decide on the number of components to retain. 
# 
# + One helpful option is visualizing the percentage of explained variance per principal component via a **scree plot**. 
# + Plotting with the `fviz_eig()` function from the `factoextra` package

# Scree plot shows the variance of each principal component 
factoextra::fviz_eig(biopsy_pca, 
                     addlabels = TRUE, 
                     ylim = c(0, 70))

## > Visualization is essential in the interpretation of PCA results. Based on the number of retained principal components, which is usually the first few, the observations expressed in component scores can be plotted in several ways.

## [Principal Component `Scores`]  ---------------------------------------------
# After a PCA, the observations are expressed as **principal component scores**.   
# 
# 1. We can retrieve the principal component scores for each Variable by calling `biopsy_pca$x`, and  store them in a new dataframe `PC_scores`.
# 2. Next we draw a `scatterplot` of the observations -- expressed in terms of principal components 

# Create new object with PC_scores
PC_scores <- as.data.frame(biopsy_pca$x)
head(PC_scores)
 
#It is also important to visualize the observations along the new axes (principal components) to interpret the relations in the dataset:
  
## [Principal Component `Scores` plot (adding label variable)] ------------------------------------
  
#  3. When data includes a factor variable, like in our case, it may be interesting to show the grouping on the plot as well.

# + In such cases, the label variable `class` can be added to the PC set as follows.

# retrieve class variable
biopsy_no_na <- na.omit(biopsy)
# adding class grouping variable to PC_scores
PC_scores$Label <- biopsy_no_na$class
 

## [Principal Component `Scores` plot (2D)]  ---------------------------------------------
#The visualization of the observation points (point cloud) could be in 2D or 3D.

# The Scores Plot can be visualized via the `ggplot2` package. 
# 
# + grouping is indicated by argument the `color = Label`; 
# + `geom_point()` is used for the point cloud.


# "Figure 1 shows the observations projected into the new data space made up of principal components"
ggplot(PC_scores, 
       aes(x = PC1, 
           y = PC2, 
           color = Label)) +
  geom_point() +
  scale_color_manual(values=c("#245048", "#CC0066")) +
  ggtitle("Figure 1: Scores Plot") +
  theme_bw()
 
## [Principal Component `Scores` (2D Ellipse Plot)] ---------------------------------------------

# Confidence ellipses can also be added to a grouped scatter plot visualized after a PCA. We use the `ggplot2` package. 
# 
# + grouping is indicated by argument the `color = Label`; 
# + `geom_point()` is used for the point cloud; 
# + the `stat_ellipse()` function is called to add the ellipses per biopsy group.


# igure 2 shows the observations projected into the new data space made up of principal components, with 95% confidence regions displayed." 

ggplot(PC_scores, 
       aes(x = PC1, 
           y = PC2, 
           color = Label)) +
  geom_point() +
  scale_color_manual(values=c("#245048", "#CC0066")) +
  stat_ellipse() + 
  ggtitle("Figure 2: Ellipse Plot") +
  theme_bw()
 
## [Principal Component `Scores` plot (3D)] ---------------------------------------------

# A 3D scatterplot of observations shows the first **3 principal components’ scores**. 
# 
# + For this one, we need the `scatterplot3d()` function of the `scatterplot3d` package;
# + The color argument assigned to the Label variable;
# + To add a legend, we use the `legend()` function and specify its coordinates via the `xyz.convert()` function.

# "Figure 3 shows the observations projected into the new 3D data space made up of principal components." 

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

# ... + legend
legend(plot_3d$xyz.convert(0.5, 0.7, 0.5), 
       pch = 19, 
       yjust=-0.6,
       xjust=-0.9,
       legend = levels(PC_scores$Label), 
       col = seq_along(levels(PC_scores$Label)))
 
## [Biplot: principal components v. original variables]  ---------------------------------------------
  
#   Next, we create another special type of scatterplot (a **biplot**) to understand the relationship between the principal components and the original variables.  
# In the `biplot` each of the observations is projected onto a scatterplot that uses the ***first and second principal components as the axes***.
# 
# + For this plot, we use the `fviz_pca_biplot()` function from the `factoextra` package 
# + We will specify the color for the variables, or rather, for the "loading vectors"
# + The `habillage` argument allows to highlight with color the grouping by `class`

# Figure 4: "The axes show the principal component scores, and the vectors are the loading vectors"

factoextra::fviz_pca_biplot(biopsy_pca, 
                            repel = TRUE,
                            col.var = "black",
                            habillage = biopsy_no_na$class,
                            title = "Figure 4: Biplot", geom="point")
 
## Interpreting biplot output  ---------------------------------------------
# Biplots have two key elements: **scores** (the 2 axes) and **loadings** (the vectors). 
# As in the scores plot, each point represents an observation projected in the space of principal components where:
#   
#   + Biopsies of the same class are located closer to each other, which indicates that they have similar **scores**  referred to the 2 main principal components; 
#  + The **loading vectors** show strength and direction of association of original variables with new PC variables.

 ## > As expected from PCA, the single `PC1` accounts for variance in almost all original variables, while `V9` has the major projection along `PC2`.

   
## Interpreting biplot output (cont.)  ---------------------------------------------
scores <- biopsy_pca$x

loadings <- biopsy_pca$rotation
# excerpt of first 2 components
loadings[ ,1:2] 
 
 
    
# POWER ANALYSIS   ---------------------------------------------
 
  # In this section, we will use: 
  #   
  #   + the *NHANES* clinical data, we already analysed in Lab # 3 
  # + a few, tidy *"fish-related"* datasets 🍥🦑 🐠 🍤 🎏  that we will load on the go
  # + Source: the materials of the "Core Statistics using R" by: [Martin van Rongen](https://github.com/mvanrongen/corestats-in-r_tidyverse)
     
# Sample Size determination in Inferential statistics   ---------------------------------------------

## [Purpose and challenges of Power Analysis]  ---------------------------------------------
#     + In this case, given $n$, $\alpha$, and a specified effect size, the analysis will return the power ($1- \beta$) of # the test, or $\beta$ (i.e. the probability of Type II error = incorrectly retaining $H_o$).

## [Specifying effect size] ---------------------------------------------

# So (since $\alpha$ and $1-\beta$ are normally set) the key piece of information we need is the **effect size**, which is essentially a function of the difference between the means of the null and alternative hypotheses over the variation (standard deviation) in the data. 
# 
# > The tricky part is that effect size is related to biological/practical significance rather than statistical significance
# 
# How should you estimate a meaningful `Effect Size`?
# 
# + Use preliminary information in the form of pilot study
# + Use background information in the form of similar studies in the literature
# + (With no prior information), make an estimated guess on the effect size expected (see guidelines next)
# 
# > Most R functions for sample size only allow you to enter effect size as input

## [Specifying effect size: general guidelines] ---------------------------------------------

# As a general indicative reference, below are the **"Cohen's Standard Effect Sizes"** (from statistician Jacob Cohen who came up with a rough set of numerical measures for `“small”`, `“medium”` and `“large”` effect sizes that are still in use today)  

 
## The `pwr` package ---------------------------------------------
 
# The `pwr` package (develped by Stéphane Champely), implements power analysis as outlined by Cohen (1988). 
# The key arguments of the function `pwr.t.test` are 4 quantities, plus 2 for the test description:
# 
# + The core idea behind its functions is that **you enter 3 of the 4 quantities** (effect size, sample size, significance level, power) **and the 4th is calculated**.

  
## [One Sample Mean: EXE data]  ---------------------------------------------
# GOAL: Imagine this is a *pilot study*, in which we tested fish is (on average) different form 20 cm in length. 
# 
# The `guanapo_data` dataset contains information on fish lengths from the Guanapo river pilot

 
# Load data on river fish length 
fishlength_data <- readr::read_csv(here::here("practice", "data_input", "04_datasets", 
                                              "fishlength.csv"),
                                   show_col_types = FALSE)

# Select a portion of the data (i.e. out "pilot" experiment)  
guanapo_data <- fishlength_data %>% 
  dplyr::filter(river == "Guanapo")

# Pilot experiment data 
names(guanapo_data)
mean_H1 <-  mean(guanapo_data$length) # 18.29655
mean_H1
sd_sample <- sd(guanapo_data$length)  # 2.584636
sd_sample
 
## [One Sample Mean t-test: EXAMPLE cont.] ---------------------------------------------
 
# Let's compute the one sample t-test with `stats::t.test` against a hypothetical average fish length ($mean\_H_o = 20$ )

# Hypothetical fish population length mean (H0)
mean_H0 <- 20
# one-sample mean t-test 
t_stat <- stats::t.test(x = guanapo_data$length,
                        mu = mean_H0,
                        alternative = "two.sided")
# one-sample t-test results
t_stat
 
## > There appear to be a statistically significant result here: the mean length of the fish appears to be different from 20 cm.

# QUESTION: In a new study of the same fish, what sample size `n` would you need to get a comparable result? 
 
## [One Sample Mean t-test: POWER ANALYSIS (`n`)] ---------------------------------------------
# Cohen's d formula 
eff_size <- (mean_H1 - mean_H0) / sd_sample # -0.6590669

# power analysis to actually calculate the minimum sample size required:
pwr::pwr.t.test(d = eff_size, 
                sig.level = 0.05, 
                power = 0.8,
                type = "one.sample")
 

## > We would need `n = 21` (rounding up) observations for an experiment (e.g. in different river) to detect an effect size as the pilot study at a 5% significance level and 80% power.  

## [One Sample Mean t-test: POWER ANALYSIS, stricter conditions] ---------------------------------------------
  # What if we wanted the results to be even more stringent? 
  # + e.g. require higher significance level (0.01) and power (0.90) with the same effect?
  
# power analysis to actually calculate the minimum sample size required:
pwr::pwr.t.test(d = eff_size, 
                sig.level = 0.01, 
                power = 0.9,
                type = "one.sample")
## > This time, we would need `n = 38` observations for an experiment to detect the same effect size at the stricter level of significance and power.


## [Two Independent Samples: EXE data] ---------------------------------------------
# Let’s look at the entire `fishlength_data` with the lengths of fish from 2 separate rivers.

# Explore complete data 
fishlength_data %>% 
  dplyr::group_by (river) %>% 
  dplyr::summarise (N = n(), 
                    mean_len = mean(length),
                    sd_len = sd(length)) 
# Visualize quickly the 2 samples (rivers) with a boxplot

# "The fish in the 2 samples appear to have different mean length" 
# visualize the data
fishlength_data %>% 
  ggplot(aes(x = river, y = length)) +
  geom_boxplot()
 
## [Two Independent Samples: t-test] -----------------------------------------
#   Let's confirm it with a two sample t-test against $𝑯_𝟎$: *The two population means are equal*

# Perform two-samples unpaired test
fishlength_data %>% 
  rstatix::t_test(length ~ river,
                  paired = FALSE
                    )
## > The t-test analysis confirms that the difference is significant.

## QUESTION: Can we use this information to design a more `efficient` experiment? I.e. run an experiment powerful enough to pick up the same observed difference in means but with **fewer observations**?

## [Two Independent Samples: POWER ANALYSIS 1/2] -------------------

#1. Let's work out exactly the **effect size** of this study by estimating Cohen’s d using this data.
#+ (We use a function from the package `rstatix::cohens_d` to estimate Cohen's d)

# Estimate cohen's d 
fishlength_data %>%
  rstatix::cohens_d(length ~ river, var.equal = TRUE)

## > The `effsize` column contains the information that we want, in this case **0.94**
  
## [Two Independent Samples: POWER ANALYSIS 2/2  (`n`)] -------------------
# 2. Actually answer the question about **how many fish** we really need to catch in the future

# run power analysis 
pwr::pwr.t.test(d = 0.94, power = 0.8, sig.level = 0.05,
                type = "two.sample", alternative = "two.sided")
## > The `n` output ( = **19 observations per group**) -as opposed to 39 + 29- would be sufficient if we wanted to confidently detect the difference observed in the previous study  

## [Two Paired Samples: EXE data] -------------------
# The `cortisol_data` dataset contains information about cortisol levels measured on 20 participants in the morning and evening

# Load data 
cortisol_data <- read.csv(file = here::here("practice", "data_input", "04_datasets", 
                                            "cortisol.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# Explore data 
names(cortisol_data)

cortisol_data %>% 
  dplyr::group_by (time) %>% 
  dplyr::summarise (
    N = n(), 
    mean_cort = mean(cortisol),
    sd_cort = sd(cortisol)) 

## > Notice the difference in the paired sample means is quite large

## [Two Paired Samples t-test: visualization] -------------------
#   Visualize quickly the 2 paired samples (morning and evening) with a boxplot

# "The cortisol levels in the 2 paired amples appear quite different" 
# visualize the data
cortisol_data %>% 
  ggplot(aes(x = time, y = cortisol)) +
  geom_boxplot()

## [Two Paired Samples: POWER ANALYSIS (`d`)] -------------------
# GOAL: Flipping the question, if we know the given `n` (20 patients observed twice): How big should the `effect size` be to be detected at `power` of 0.8 and `significance level` 0.05? 
  
  #+ We use `pwr::pwr.t.test`, with the argument specification `type = "paired"`, but this time to estimate the **effect size**

# power analysis to actually calculate the effect size at the desired conditions:
pwr::pwr.t.test(n = 20, 
                #d =  eff_size, 
                sig.level = 0.05, 
                power = 0.8,
                type = "paired")

## > The functions returns the effect size (Cohen’s metric): `d = 0.6604413`. So, with this experimental design we would be able to detect a **medium-large effect size**.

## [Two Paired Samples t-test: POWER ANALYSIS on given `n`] -------------------

# Looking instead at the **actual sample data**, what would be the observed effect size?
# + To compute "observed `d`" we can use the function `rstatix::cohens_d` 

d <- cortisol_data %>% 
  # estimate cohen's d
rstatix::cohens_d(cortisol ~ time, paired = TRUE)

d

## > The obtained `d` (-1.16) is extremely large, so ***we likely have more participants in this study than actually needed*** given such a large effect. 

## [Two Paired Samples t-test: POWER ANALYSIS gives sufficient `n`] -------------------

# Let's re-compute the power analysis, but leave `n` as the unknown quantity, given the effect size (`d`) we have observed  
# power analysis to calculate minimunm n given the observed effect size in the sample 
pwr::pwr.t.test(# n = 20, 
                d =  -1.16, 
                sig.level = 0.05, 
                power = 0.8,
                type = "paired")
## > As a matter of fact, ` n = 8` pairs of observations would have sufficed in this study, given the size of effect we were trying to detect.


## [One-way ANOVA test: EXE data] -------------------
# The `mussels_data` dataset contains information about the length of the *anterior adductor muscle scar* in the mussel # `Mytilus trossulus` across five locations around the world!  

# Load data 
mussels_data <- read.csv(file = here::here("practice", "data_input", "04_datasets", 
                                        "mussels.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 

# Explore data 
names(mussels_data)

stats <- mussels_data %>% 
  dplyr::group_by (location) %>% 
  dplyr::summarise (
    N = n(), 
    mean_len = mean(length),
    sd_len = sd(length)) 

stats

## [One-way ANOVA test: visualization] -------------------
# > There appears to be a noticeable difference in lenght at average measurements *at least* between some of the locations

#| output-location: slide

# Visualize the data with a boxplot
mussels_data %>% 
  ggplot(aes(x = location, y = length)) +
  geom_boxplot()

## [One-way ANOVA test: EXAMPLE cont.] -------------------
# Assuming we verified the required assumptions, let's run the ANOVA test to confirm the visual intuition 
# 
# + With the `stats::aov` followed by the command `summary`  

# Summary of test outputs: 
summary_ANOVA <- summary(stats::aov(length ~ location,
                                    data = mussels_data))

# From which we extract all the output elements 
# F value 
summary_ANOVA[[1]][["F value"]] # 7.121019
# p value 
summary_ANOVA[[1]][["Pr(>F)"]]  # 0.0002812242
# df of numerator and denominator
summary_ANOVA[[1]][["Df"]]      # 4, 34 
# Sum of Square BETWEEN groups
SSB <- summary_ANOVA[[1]][["Sum Sq"]][1]  # 0.004519674
# Sum of Square WITHIN groups
SSW <- summary_ANOVA[[1]][["Sum Sq"]][2]  # 0.005394906

## > + A one-way ANOVA test confirms that **the mean lengths of muscle scar differed significantly between locations** ( F = 7.121, with df = [4, 34], and p = 0.000281).
 
## [One-way ANOVA test: POWER ANALYSIS (`effect`)] -------------------

# In ANOVA it may be tricky to decide what kind of effect size we are looking for: 
#   
#   + if we care about an overall significance test, the sample size needed is a function of the standard deviation of the group means
# + if we're interested in the comparisons of means, there are other ways of expressing the effect size (e.g. a difference between the smallest and largest means)
# 
# Here let's consider an overall test in which we could reasonably collect the same n. of observations in each group 

n_loc <- nrow(stats)

means_by_loc <- c(0.0780, 0.0748, 0.103, 0.0802, 0.0957)
overall_mean <-  mean(means_by_loc)
sd_by_loc <- c(0.0129, 0.00860, 0.0162, 0.0120, 0.0130)
overall_sd <-  mean(sd_by_loc)

## [One-way ANOVA test: POWER ANALYSIS (`effect`)] -------------------

# Effect Size f formula
Cohen_f = sqrt( sum( (1/n_loc) * (means_by_loc - overall_mean)^2) ) /overall_sd
Cohen_f # EXTREMELY BIG 

# Power analysis with given f 
pwr::pwr.anova.test(k = n_loc,
                    n = NULL,
                    f = Cohen_f,
                    sig.level = 0.05,
                    power = 0.80)
## > The `n` output ( = **5 observations per group**) -as opposed to >6 per group- would be sufficient if we wanted to confidently detect the difference observed in the previous study  

## [Linear Regression with grouped data: EXE data] -------------------
 
# The ideas covered before apply also to **linear models**, although here:
# 
# + we use `pwr.f2.test()` to do the power calculation
# + the `effect sizes` ($f^2$) is based on $R^2$

# define the linear model
lm_mussels <- lm(length ~ location, 
                 data = mussels_data)
# summarise the model
summary(lm_mussels)
 
## [Linear Regression with grouped data: POWER ANALYSIS] -------------------
# From the linear model we get that the $R^2$ value is 0.4559 and we can use this to calculate Cohen’s $f^2$ value using the formula 

# Extract R squared
R_2 <- summary(lm_mussels)$r.squared
# compute f squared
f_2 <- R_2 / (1 - R_2)
f_2

#Our model has 5 parameters (because we have 5 groups) and so the numerator degrees of freedom $u$ will be 4 (5−1=4). 
#Hence, we carry out the power analysis with the function `pwr.f2.test`:

# power analysis for overall linear model 
pwr::pwr.f2.test(u = 4, v = NULL, 
                 f2 = 0.8378974,
                 sig.level = 0.05 , power = 0.8)

## [Linear Regression with grouped data: POWER ANALYSIS interpret.] -------------------
# Recall that, in the F statistic evaluating the model, 

# + **u** the df for the numerator: $df_{between} =k−1 = 5-1 = 4$ 
# + **v** the df for the denominator: $df_{within} = n-k = ?$ 
#   + so $n = v+5$ 

pwr::pwr.f2.test(u = 4, f2 = 0.8378974,
            sig.level = 0.05 , power = 0.8)
## > This tells us that the denominator degrees of freedom **v** should be 15 (14.62 rounded up), and this means that we would only need 20 observations **n = v+5** in total across all 5 groups to detect this effect size 

# SAMPLE SPLITTING IN MACHINE LEARNING -------------------
 
# > Embracing a different philosophical approach... 
 
## [2 different approaches with different takes on empirical data] -------------------
# [*(Simplifying a little)*] 
#### Inferential statistics
# + `APPROACH`: Strong emphasis on defining assumptions (about variables distributions) and/or hypotheses on the relationship between them 
 
#### Machine Learning
 # + `APPROACH`: Focus on labeling observations or uncovering ("learn") a pattern, without worrying about explaining them
## [Data Splitting in ML approaches] -------------------

## [Introducing R (metapackage) `tidymodels` for modeling and ML] -------------------
# The package `tidymodels` (much like the `tidyverse`) is an ecosystem of packages meant to enable a wide variety of approaches for modeling and statistical analysis.
# 
# + One package in this system is `rsample` is one of its building blocks for resampling data 


## [Revisiting NHANES for a quick demonstration of predictive modeling] -------------------

# Let's re-load a dataset from Lab # 3 (the NHANES dataset) for a quick demonstration of data splitting in an ML predictive modeling scenario 

# + We can try predicting `BMI` from `age` (in years), `PhysActive`, and `gender`, using linear regression model (which is a `Supervised ML algorithm`) 

#+ (we already saved this dataset)
# Use `here` in specifying all the subfolders AFTER the working directory 
nhanes <- read.csv(file = here::here("practice", "data_input", "03_datasets",
                                      "nhanes.samp.csv"), 
                          header = TRUE, # 1st line is the name of the variables
                          sep = ",", # which is the field separator character.
                          na.strings = c("?","NA" ), # specific MISSING values  
                          row.names = NULL) 


## Splitting the dataset into training and testing samples
# + With this approach, it is best practice to **"hold back" some data for testing** to get a better estimate of how models will perform on new data
 
# + We can easily specify training and testing sets using `rsample`'s function `initial_split`
  
# ensure we always get the same result when sampling (for convenience )
set.seed(12345)
  
nhanes_split <- nhanes %>%
  # define the training proportion as 75%
  rsample::initial_split(prop = 0.75,
                         # ensuring both sets are balanced in gender
                         strata = Gender)

# resulting datasets
nhanes_train <- rsample::training(nhanes_split)
dim(nhanes_train)
nhanes_test <- rsample::testing(nhanes_split)
dim(nhanes_test)

#  In this case the **regression models** serves for predicting numeric, continuous quantities 
  # fitting  linear regression model specification
  lin_mod <- lm(BMI ~ Age + Gender + PhysActive, data = nhanes_train)
  
  summary(lin_mod)
 
## [Predicting BMI estimates for new data set] -------------------
# Using the above model, we can predict the BMI for different individuals (those left in the testing data)
#  + with the function `predict`, where we specify the argument `newdata = nhanes_test`)
#  + adding the prediction `interval` (the 95% CI), which gives uncertainty around a single value of the prediction

# Obtain predictions from the results of a model fitting function
pred_bmi <- stats::predict(lin_mod, 
                           newdata = nhanes_test,
                           interval = "confidence" )
head(pred_bmi)

## [Evaluating the predictive performance in testing data] -------------------
# The ultimate goal of holding data back from the model training process was to **evaluate its predictive performance on new data**. 
#   A common measure used is the `RMSE (Root Mean Square Error)` = a measure of the distance between observed values and predicted values **in the testing dataset**  
  
# Computing the Root Mean Square Error
RMSE_test <- sqrt(mean((nhanes_test$BMI - predict(lin_mod, nhanes_test))^2, na.rm = T))
RMSE_test # 6.170518


## > The RMSE (= 6.170518) tells us, (roughly speaking) by how much, on average, the new observed BMI values differ from those predicted by our model

## [... and what about RMSE in training data?] -------------------
#  Let's see the RMSE in the training dataset (for comparison)

RMSE_train <- sqrt(mean((nhanes_train$BMI - predict(lin_mod, nhanes_train))^2, na.rm = T))
RMSE_train # 6.866044

# R squared is also quite low 
summary(lin_mod)$r.squared     # R^2 0.0341589

## > This is not what expected 🤔, since RMSE on the training data is sliglthly bigger that in the testing data! 
# A possible explanation is that out model is `underfitting` in the first place (model's ${R}^2$ was quite low too), so we should definitely try different models...  


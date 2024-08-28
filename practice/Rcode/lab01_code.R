# Lab 1 INTRO TO R AND DATA ANALYSIS -------------------------------------------
# Practice session covering topics discussed in Lecture 1 


# _________---------------------------------------------------------------------
# INTRO TO R AND RSTUDIO ------------------------------------------------- 

# R & RStudio installation ---------------------------------------------------- 
    ## --- (see instructions in website https://r4biostats.com/install.html)

# check your R version ---------------------------------------------------- 
R.Version()
# or just
R.version.string


# Creating an R Project [in Rstudio] ------------------------------------------
    ## --- (see instructions in lab slides https://r4biostats.com/practice/lab01_data_with_R.html)


# (Install) & Load R Packages --------------------------------------------------
    ## --- (see instructions in lab slides https://r4biostats.com/install.html)

# to install a package use `utils` function `install.packages("package_name)`

# We are mostly using {base} & {utils} (pre-installed and pre-loaded)
# We will also use the packages below (specifying `package::function` for clarity).

# Load them for this R session
library(here)      # tools find your project's files, based on working directory
library(janitor)   # tools for examining and cleaning data
library(skimr)     # tools for summary statistics 
library(dplyr)     # {tidyverse} tools for manipulating and summarising tidy data 
library(ggplot2)   # {tidyverse} tools for plotting
library(forcats)   # {tidyverse} tool for handling factors
library(ggridges)  # alternative to plot density functions 
library(fs)        # file/directory interactions


# Defining (reproducible) file paths with `here` -------------------------------

# Where is my Working Directory?
here::here()

# Create a Sub-Directory (e.g. for saving input data and ouptut data)
## with here I simply add subfolder names relative to my WD 

# (check the function documentation)
?fs::dir_create
# here we will save input files
fs::dir_create(here::here("practice", "data", "data_input"))
# here we will save output files at the end
fs::dir_create(here::here("practice", "data", "data_output")) 

## --- [Then I remove, because I have them already]
fs::dir_delete(here::here("practice", "data" ))

# _________---------------------------------------------------------------------
# R OBJECTS, FUNCTIONS, PACKAGES -----------------------------------------------

# Importing data into R workspace  ---------------------------------------------
## --- (We are using real data provided by Thabtah,Fadi. (2017). Autism Screening Adult. 
      # UCI Machine Learning Repository. https://doi.org/10.24432/C5F019 )

# We use `utils::read.csv` to load a csv file 
?read.csv # to learn about function and arguments 

#### Option 1/2 from a url ----------------------------------------------------- 
autism_data_url <- read.csv(file = "https://raw.githubusercontent.com/Sydney-Informatics-Hub/lessonbmc/gh-pages/_episodes_rmd/data/autism_data.csv", 
                               header = TRUE, # 1st line is the name of the variables
                               sep = ",", # which is the field separator character.
                               na.strings = c("?")  # specific values I want R to interpret as missing, i.s. NA
                            ) 

#### Option 2/2 from my folder (upon downloading)------------------------------- 
# Check my working directory location
here::here()

# Use `here` in specifying all the subfolders AFTER the working directory 
autism_data_file <- read.csv(file = here("practice", "data_input", "01_datasets", "autism_data.csv"), 
                             header = TRUE, # 1st line is the name of the variables
                             sep = ",", # which is the field separator character.
                             na.strings = c("?"),  # specific values I want R to interpret as missing, i.e. NA
                              row.names = NULL) 


# _________-------------------------------------------------------------------

# DATA OBSERVATION & MANIPULATION   -----------------------------------------------

# Viewing the dataset -----------------------------------------------------
View(autism_data_file)  
# (or click on the object in Environment tab)

# What data type is this data?
class(autism_data_file)

# What variables are included in this dataset?
base::colnames(autism_data_file)
    ## Notice the var name formatting inconsistency: Class.ASD

# Manipulate / clean the dataframe ---------------------------------------------------

# I want a more consistent naming (no ".", only "_")
# I use very handy function `clean_names` from the {janitor} package
autism_data <- janitor::clean_names(autism_data_file, 
                                     case = "none") ## (also OK with no arguments) 
    ## case = "none" leaves the case as is, but only uses "_" separator
  
# check change
colnames(autism_data)
dim(autism_data)


# With the `$` sign I extract a variable (column name)
autism_data$id
autism_data$A1_Score

# Add a new column (renaming the dataframe -> autism_pids) ---------------------
# rename dataframe 
autism_pids <- autism_data
# create a new column 
autism_pids$pids <- paste("PatientID_" , autism_data$id, sep = "")
base::colnames(autism_pids)

# check change in df structure
dim(autism_data)
dim(autism_pids)


##### (optional) Clean up my workspace -----------------------------------------
# what do I have in the environment? 
ls() 
# remove all EXCEPT for "autism_pids" which I am going to use 
rm("autism_data", "autism_data_file", "autism_data_url" ) 

# Different ways to select variables--------------------------------------------

###### using the `$` sign {base} -----------------------------------------------
# With the `$` sign I extract a variable (column name)
autism_pids$id 
autism_pids$pids
autism_pids$A1_Score
autism_pids$ethnicity

###### using indexing `[,]` {base} -----------------------------------------------
# Indexing to pick `[ , #col]`  
autism_pids[ ,1] # empty rows means all 
autism_pids[ ,23]
autism_pids[ ,2]
autism_pids[ ,14]

# Indexing to pick `[#row, ]`  
autism_pids[1 , ] # empty cols means all 
autism_pids[50,]
autism_pids[25:27 ,]

# Indexing to pick `[#row, #col]`  
autism_pids[1:3,1]
autism_pids[1:3,23]
autism_pids[1:3,2]
autism_pids[1:3,14]

# What are the data types of the variables? ---------------------------------
str(autism_pids) # integer and character

# What values can the variables take? ---------------------------------
summary(autism_pids$pids)
length(unique(autism_pids$pids)) # N unique values
sum(is.na(autism_pids$pids)) # N missing values

summary(autism_pids$ethnicity)
length(unique(autism_pids$ethnicity)) # N unique values
sum(is.na(autism_pids$ethnicity)) # N missing values

# Same but with in one line (!) with function `skim` 
skimr::skim(autism_pids$pids)
skimr::skim(autism_pids$ethnicity)

# I can use it for the WHOLE dataframe!
skimr::skim(autism_pids)

(autism_pids$id)

# _________-------------------------------------------------------------------
# Recoding variables (using {base} -------------------------------------- 

###### using {base}  ----------------------------------------------------------

#### char 2 factor -------------------------------------------------------------
# Say I want to treat some variables as factors
autism_pids$gender <- as.factor(autism_pids$gender)
autism_pids$ethnicity <- as.factor(autism_pids$ethnicity)
autism_pids$contry_of_res <- as.factor(autism_pids$contry_of_res)
autism_pids$relation <- as.factor(autism_pids$relation)
# check 
skimr::skim(autism_pids) # now I have variable type: factor

#### char 2 factor (n cols)-----------------------------------------------------
autism_pids_temp <- autism_pids # copy df for test 

to_factor <- c("gender", "ethnicity", "contry_of_res", "relation") # vector of col names 
autism_pids_temp[ ,to_factor] <-  lapply(X =  autism_pids[ ,to_factor], FUN = as.factor)

# check 
skimr::skim(autism_pids_temp) # now I have Variable type: factor

#### Inspect factors levels (3 different ways) ---------------------------------

# Option 1/3 using base::levels function
levels(autism_pids$ethnicity)

# Option 2/3 using base::table function
table(autism_pids$ethnicity,useNA = "ifany")

# Option 3/3 using janitor::tabyl function 
  ## * {janitor} uses the "pipe operator %>% which takes the output of a function 
  ## as input of the next one 
janitor::tabyl(autism_pids$ethnicity) %>% 
  adorn_totals() %>% 
  adorn_pct_formatting()

# Use `is.na` to check if the 95 missing obs are the same missing for `ethnicity` and `relation`
which(is.na(autism_pids$ethnicity)) # indices of TRUE elements in vector
which(is.na(autism_pids$relation))  # indices of TRUE elements in vector
  ## indeed they are the same IDs!

#### char 2 logical  --------------------------------------------------------
# observe a subset of some columns 
autism_subset <- autism_pids [1:5, c("gender","jaundice", "autism", "age_desc", "Class_ASD","pids")]
View(autism_subset)

# recode "age_desc" as LOGICAL new var "age_desc_log"
autism_pids$age_desc_log <- ifelse(autism_pids$age_desc == "18 and more", TRUE, FALSE )
class(autism_pids$age_desc)
class(autism_pids$age_desc_log)
 
# check 
skimr::skim(autism_pids) # now I have Variable type: logical


#### char 2 dummy [0,1]  ---------------------------------------------------
# I also may need my binary variables expressed as 01 (e.g. to incorporate 
  # nominal variables into regression analysis)
autism_pids$autism_dummy <- ifelse(autism_pids$autism == 'yes', 1, 0)
class(autism_pids$autism)
class(autism_pids$autism_dummy)

# _________-------------------------------------------------------------------

# Subsetting the data for further investigation ------------------------------
colnames(autism_pids)

###### using {utils} `head` or `tail` -------------------------------------------
head(autism_pids)   #return fist 6 obs
tail(autism_pids)   #return last 6 obs

head(autism_pids, n = 2) #return fist 2 obs
tail(autism_pids, n = 2) #return last 2 obs


# Investigating a subset of observations ---------------------------------------
# E.g. I learned that some patients have missing age… how many are they?
sum(is.na(autism_pids$age)) # or 
skimr::n_missing(autism_pids$age)

# ... I need the ID of the patients with missing `age`

# Create a new df (only the patients with missing `age`) as SUBSET of the given df 
## I choose the obs of interest and a few useful vars

###### Option 1/3 using {base} `[]`  ------------------------------------------- 
missing_age_subset <- autism_pids[is.na(autism_pids$age), c("pids", "age", "autism_dummy") ]
missing_age_subset

###### Option 2/3 using {base} `which` -----------------------------------------------------
missing_age_subset2 <- autism_pids[which(is.na(autism_pids$age)), c("pids", "age", "autism_dummy")] 
missing_age_subset2

###### Option 3/3 using {base} `subset` ---------------------------------------- 
# arguments allow me to specify rows and cols 
missing_age_subset3 <- subset(x = autism_pids, 
                              subset = is.na(autism_pids$age), # 1 logical condition
                              select = c("pids", "age", "autism_dummy") # which cols
                              ) 
missing_age_subset3


# Creates a SUBSET based on MORE conditions (`age` and `ethnicity`)------------- 
###### Option 1/2 using `base::subset` -----
twocond_base_subset <- subset(x = autism_pids, 
                              # 2 logical conditions      
                              subset = age < 25 & contry_of_res == "Brazil", 
                              # pick a few cols 
                              select = c("pids", "age", "contry_of_res",
                                         "autism_dummy")) 

twocond_base_subset

###### Option 2/2 using `dplyr` (`filter` + `select`) -----
## here the filtering (rows) and selecting (columns) is done in sequence
twocond_dplyr_subset <- autism_pids %>% 
  dplyr::filter(age < 25 & contry_of_res == "Brazil") %>%  # which rows
  dplyr::select (pids, age, contry_of_res, autism_dummy)   # which cols

twocond_dplyr_subset

# Dealing with missing data ----------------------------------------------

# Explore how to input values where missing ----------------------------------------------

# 1/2 create a new variable 
autism_pids$age_inputed <- autism_pids$age
# 2/2 replace value (presumably taken from other source) of `aged_inputed` 
  # CONDITIONAL on `pids`
autism_pids$age_inputed[autism_pids$pids == "PatientID_63"] <-  65
autism_pids$age_inputed[autism_pids$pids == "PatientID_92"] <-  45

# check
skimr::n_missing(autism_pids$age_inputed)  


# _________-------------------------------------------------------------------
# DESCRIPTIVE STATISTICS  ------------------------------------------------------

#### Summarizing all variables {base} `summary` --------------------------------
skimr::skim(autism_pids)
# OR 
summary(autism_pids)

# Notice the different treatment of integer (A1_Score) / factor (ethnicity) / logical (age_desc_log)
# the function results depend on the class of the object 
summary(autism_pids$A1_Score)     # min, max quartiles, mean, median
summary(autism_pids$ethnicity)    # counts of levels' frequency (included NA!)
summary(autism_pids$age_desc_log) # counts of TRUE 

# Frequency distributions with table (1 var) -----------------------------------
## Frequency distributions can be used for nominal, ordinal, or interval/ration variables 
table(autism_pids$gender)
table(autism_pids$age) # automatically drops missing...
table(autism_pids$age, useNA = "ifany") #...unless specified

# Cross tabulation with table (2 vara) -----------------------------------
table(autism_pids$gender, autism_pids$age, useNA = "ifany")
table(autism_pids$gender, autism_pids$age_inputed)
table(autism_pids$ethnicity, autism_pids$autism_dummy)

#### Grouping and summarizing ------------------------------------------------
# E.g. I want to know the avg age of men and women 

###### using {base} `by` -----------------------------------------------------
# by(data$column, data$grouping_column, mean)
by(data = autism_pids$age_inputed, INDICES = autism_pids$gender, FUN = mean)

###### using {base} `tapply` ----------------------------------------------------
# i.e. apply a function to subsets of a vector or array, split by one or more factors.
tapply(X = autism_pids$age_inputed, INDEX = autism_pids$gender, FUN = mean)

###### using {base} `split` and `lapply` ----------------------------------------
# sapply(split(data$column, data$grouping_column), mean)
sapply(X = split(autism_pids$age_inputed, autism_pids$gender),FUN = mean) # returns a vector

###### using {dplyr} -------------------------------------------------------
# {dplyr} is a key package in the {tidyverse} collection 
# it uses the "pipe" %>% and doesn't require to specify the `dataframe$col_name` 
# but simply `col_name`
autism_pids %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(mean(age_inputed))  # returns a dataframe!

# I could add more statistics to the grouped summary... 
autism_pids %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarise(mean_age = mean(age_inputed),  
                   N_obs = n(), 
                   N_with_autism = sum(autism_dummy == 1)
  ) 

# _________-------------------------------------------------------------------
# Measures of central tendency --------------------------------------------
## Let's use `age` and `age_inputed` to see what inputed missing values did 

###### using {base} `mean`, `median` -----------------------------------------------------
mean(autism_pids$age)
median(autism_pids$age)

# Important to specify the argument `na.rm = TRUE` or the function won't work
mean(autism_pids$age, na.rm = TRUE)
median(autism_pids$age, na.rm = TRUE)

mean(autism_pids$age_inputed)
median(autism_pids$age_inputed)

###### using a custom function for the "mode" ----------------------------------

# Create custom function to calculate statistical mode

f_calc_mode  <- function(x) { 
  # `unique` returns a vector of unique values 
  uni_x <- unique(x)  
  # `match` returns the index positions of 1st vector against 2nd vector
  match_x <- match(x, uni_x)
  # `tabulate` count the occurrences of integer values in a vector.
  tab_x  <-  tabulate(match_x) 
  # returns element of uni_x that corresponds to max occurrences
  uni_x[tab_x == max(tab_x)]
}

# check
f_calc_mode(autism_pids$age)
f_calc_mode(autism_pids$age_inputed)
f_calc_mode(autism_pids$ethnicity)

# u <- unique(autism_pids$age_inputed)
# m <- match(autism_pids$age_inputed, u)
# t <- tabulate(m)
# u[t == max(t)]
 
# Measures of variability (or spread) -------------------------------------

## Variance and Standard deviation

var(autism_pids$age)
var(autism_pids$age_inputed)

sd(autism_pids$age)
sd(autism_pids$age_inputed)


# _________-------------------------------------------------------------------
# VISUAL DATA EXPLORATION -------------------------------------------------

# Plotting with ggplot2 ----------------------------------------------------------------

# Save some colors (for customizing plots) ---------------------------------

# Colors are defined in the form of **Hexadecimal color values** 
two_col_palette <-  c("#9b2339", "#005ca1")
contrast_cols_palette <- c("#E7B800","#239b85", "#85239b", "#9b8523","#23399b",
                           "#d8e600", "#0084e6", "#399B23", "#e60066",
                           "#00d8e6", "#e68000")

# Distribution of continuous var  ---------------------------------------------- 

### Histograms   ----------------------------------------------------------------
# Histograms (and density plots) are often used to show the distribution of a continuous variable. 

#### ... Option 1) data inside the ggplot() function --------------------------

ggplot(data = autism_pids, mapping = aes(x=age_inputed)) + 
  geom_histogram() + 
  theme_bw()

#### ... Option 2) data before the pipe %>% --------- --------------------------
# notice the  `%>%`  before using ggplot2...
autism_pids %>% 
  # then `+` when using ggplot2
  ggplot(aes(x = age_inputed )) +
  geom_histogram() + 
  theme_bw()


#### ... define bin width ------------------------------------------------------
# Histograms split the data into ranges (bins) and show the number of observations in each. Hence the shape of the histogram will change depending on how wide or narrow these ranges (or bins, or buckets) are. Try to pick widths that represents the data well.

autism_pids %>% 
    ggplot(aes(x = age_inputed )) + 
    # specify to avoid warning if we fail to specify the number of bins 
    geom_histogram(bins=40) + 
    theme_bw()

#### ... add mean and std dev vertical lines ----------------------------------
# using geom_vline() to add a vertical line for the mean, and the range between -1 and +1 sd from the mean.
# using annotate() for adding small annotations (such as text labels)
  
autism_pids %>% 
    ggplot(aes(x = age_inputed )) + 
    geom_histogram(bins=40) + 
    # add mean vertical line
    geom_vline(xintercept = mean(autism_pids$age_inputed),
               na.rm = FALSE,
               lwd=1,
               color="#9b2339") +
    # add annotations with the mean value
    annotate("text",                        
             x = mean(autism_pids$age_inputed) * 1.2, # coordinates for positioning
             y = mean(autism_pids$age_inputed) * 2.5,
             label = paste("Mean =", round(mean(autism_pids$age_inputed), digits = 2)),
             col = "#9b2339",
             size = 4)+
    # add also sd +1 and -1 
    geom_vline(aes(xintercept = mean(autism_pids$age_inputed) + sd(autism_pids$age_inputed)), 
               color = "#000000", size = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = mean(autism_pids$age_inputed) - sd(autism_pids$age_inputed)), 
               color = "#000000", size = 1, linetype = "dashed") +
    theme_bw() 

### Density plot  ----------------------------------------------------------------
autism_pids %>% 
    ggplot(aes(x = age_inputed)) +
    geom_density()+
    theme_bw() 
  
  
### Density plot + color + x axis ticks ----------------------------------------
autism_pids %>% 
  ggplot(aes( x=age_inputed)) +
  # add arguments   
  geom_density(fill="#85239b", color="#e9ecef", alpha=0.5)+
  theme_bw() + 
  # increase number of x axis ticks 
  scale_x_continuous(breaks = seq(10, 100,10 ), limits = c(16, 86))


# ___-------------------------------------------------------------------
# Distribution of continuous var split by categorical var ------------

### Histograms  ----------------------------------------------------------------
#### ... `fill =  category`------------------------------------------------------------------

# indicate the categorical group as fill = in the aesthetic mapping
# specify custom colors for each group:
autism_pids %>% 
 # specifying `fill` = gender
 ggplot(mapping = aes(x = age_inputed, fill = gender )) + 
 geom_histogram(bins=40) + 
 scale_fill_manual(values = c("#e07689","#57b7ff")) +
 scale_color_manual(values = c("#9b2339","#005ca1")) +
 theme_bw() 

#### ... shifting bars by group----------------------------------------------------
# trying to improve readability 
autism_pids %>% 
  ggplot(mapping = aes(x = age_inputed, fill = gender )) + 
  # bars next to each other with `position = 'dodge'`
    geom_histogram(bins=40, position = 'dodge')  + 
    scale_fill_manual(values = c("#e07689","#57b7ff")) +
    scale_color_manual(values = c("#9b2339","#005ca1")) +
    theme_bw()  

#### ...facet by gender  ------------------------------------------------------- 
# That’s not very easy to digest
# Instead of only filling, you can separate the data into multiple plots to improve readability  
autism_pids %>% 
  ggplot(aes(x = age_inputed, fill = gender )) + 
  geom_histogram(color="#e9ecef", alpha=0.8, position = 'dodge') + 
  theme_bw() + 
  # splitting the gender groups, specifying `ncol` to see one above the other
  facet_wrap(~gender, ncol = 1)  + 
  scale_fill_cyclical(values = c("#9b2339","#005ca1"))

#### ...extra step for 2 mean vert lines by gender  ---------------------------- 
#I want to see the mean vertical line for each of the subgroups, but in this case,
#I need to create a small dataframe of summary statistics (group_stats).
  
# 1/3 using `dplyr` add a column `mean_age` with the group mean
group_stats <- autism_pids %>% 
  dplyr::group_by(gender) %>% 
  dplyr::summarize(mean_age = mean(age_inputed),
                   median_age = median (age_inputed)) 

group_stats

#### ...(Introducing tidyr::pivot_longer) -----------------------------------------
# 2/3 using `tidyr` I reshape this small df to LONG form  
group_stats_long <- group_stats %>% 
  tidyr::pivot_longer(cols = mean_age:median_age, 
                      names_to = "Stat", 
                      values_to = "Value") %>% 
  dplyr::mutate(label = as.character(glue::glue("{gender}_{Stat}")))

group_stats_long


#### ...facet by gender + vert lines by group ---------------------------------- 
# 3/3 re do the plot 
autism_pids %>% 
  ggplot(aes(x = age_inputed, fill = gender)) + 
  # geom_histogram from dataframe 1
  geom_histogram(bins=30,color="#e9ecef", alpha=0.8, position = 'dodge') + 
  facet_wrap(~gender, ncol = 1) + 
  scale_fill_manual(values = c("#9b2339","#005ca1"))  +
  # geom_vline from dataframe 2
  geom_vline(data = group_stats_long, 
             mapping = aes(xintercept = Value, color = Stat),
             lwd=1.5,
             linetype=6) + 
  scale_color_manual(values = c( "#e68000", "#d8cf71")) +
  theme_bw() 

#### ...finishing touches ---------------------------------- 
# using labs() and theme() layers
# saving plot as object 

hist_plot <- autism_pids %>% 
  ggplot(aes(x = age_inputed, fill = gender)) + 
  # geom_histogram from dataframe 1
  geom_histogram(bins=30,color="#e9ecef", alpha=0.8, position = 'dodge') + 
  facet_wrap(~gender, ncol = 1) + 
  scale_fill_manual(values = c("#9b2339","#005ca1"))  +
  # geom_vline from dataframe 2
  geom_vline(data = group_stats_long, 
             mapping = aes(xintercept = Value, color = Stat),
             lwd=1.5,
             linetype=6) + 
  scale_color_manual(values = c( "#e68000", "#d8cf71")) +
  # increase number of x axis ticks 
  scale_x_continuous(breaks = seq(10, 100,10 ), 
                     limits = c(10, 70)) +
  # Additional theme details 
  labs(x = "age brackets", y = "n of individuals",
       color = "Stats",
       title = "Distribution of observations by gender",
       subtitle = "",
       caption = "Source: Thabtah,Fadi. (2017). Autism Screening Adult. UCI Machine Learning Repository. https://doi.org/10.24432/C5F019.") +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold")) 

hist_plot


### Density `ggridges` package ----------------------------------------------------------------
# As an alternative, you can use the {ggridges} package to make ridge plots
autism_pids %>% 
  # this takes also `y` = group
  ggplot(aes( x=age_inputed, y = gender, fill = gender)) +
  ggridges::geom_density_ridges() +
  # I can add quantile lines (2 is the median)
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+  
  # increase number of x axis ticks 
  scale_x_continuous(breaks = seq(10, 100,10 ), limits = c(16, 86)) + 
  scale_fill_cyclical(values = c("#9b2339","#005ca1")) + 
  theme_bw() # or theme_ridges()


### Barchart ----------------------------------------------------------------
# Bar charts provide a visual presentation of categorical data. `geom_bar()` makes the height of the bar proportional to the number of cases in each group

# let's take a variable that we recoded as `factor`
class(autism_pids$ethnicity)

#### ...bare minimum ---------------------------------------------------------- 
autism_pids %>% 
  ggplot(aes(x = ethnicity )) + 
  geom_bar() +  
  theme_bw() 

#### ...improve theme  --------------------------------------------------------- 
autism_pids %>% 
  ggplot(aes(x = ethnicity )) + 
  geom_bar(fill = "steelblue") +
  # reference line  
  geom_hline(yintercept=100, color = "#9b2339", size=0.5, ) +
  # labels, title, etc 
  labs(x = "ethnicity", y = "n of individuals",
       color = "Stats",
       title = "Distribution of observations by ethnicity",
       subtitle = "",
       caption = "Autism study")  +
  theme_bw() +
  # specification son axis labels
  theme(axis.text.x = element_text(angle=50, vjust=0.75), 
        axis.text.y = element_text(size=10,face="bold"))
  
#### ...improve readability (reorder)  ---------------------------------- 
# reordering the bars by count using the package `forcats`   
# (this we can do because ethnicity is coded as factor)

autism_pids %>% 
    # we modify our x like so 
    ggplot(aes(x = forcats::fct_infreq(ethnicity ))) + 
    geom_bar(fill = "steelblue") +
    geom_hline(yintercept=100, color = "#9b2339", size=0.5, ) +
    labs(x = "ethnicity", y = "n of individuals",
         color = "Stats",
         title = "Distribution of observations by ethnicity",
         subtitle = "",
         caption = "Autism study")  +
    theme_bw() +
    theme(axis.text.x = element_text(angle=50, vjust=0.75), 
          axis.text.y = element_text(size=10,face="bold"))  
  
#### ...improve readability (highlight NA)  ---------------------------------- 

autism_pids %>%
  ## --- prep the dataframe 
  dplyr::mutate(# Add a factor variable with two levels
    highlight = forcats::fct_other(ethnicity, 
                                   keep = "NA", 
                                   other_level = "All Groups")) %>% 
  ## --- now plot 
  # In `aes mapping` we map color to a variable (`fill = highlight`)
  ggplot(aes(x = forcats::fct_infreq(ethnicity), fill = highlight)) + 
  geom_bar()+
  # Use custom color palettes
  scale_fill_manual(values=c("#0084e6")) +
  # Add a line at a significant level 
  geom_hline(yintercept=100, color = "#9b2339", size=0.5, ) +
  theme_bw() +
  # make some more theme specifications  
  labs(x = "ethnicity", y = "n of individuals",
       color = "Stats",
       title = "Distribution of observations by ethnicity",
       subtitle = "",
       caption = "Autism study")  +
  theme(axis.text.x = element_text(angle=50, vjust=0.75), 
        axis.text.y = element_text(size=10,face="bold"))  +
  theme(legend.position = "none")   
 
    
### Boxplot   ----------------------------------------------------------------
# The boxplot is one of the simplest ways of representing a distribution of a continuous variable and it is packed with information. 


# Boxplot Example 1  ------------------------------------------------------
# explore how the continuous variable `result` is distributed in the autism dataset.
autism_pids %>% 
  ggplot(aes(x = result )) +
  geom_boxplot(alpha=0.5)+
  # switch to vertical orientation
  coord_flip() +
  theme_bw()   


# Boxplot Example 2  ------------------------------------------------------
# Explore how the continuous variable `result` is distributed by the categorical variable (factor) `ethnicity`
autism_pids %>% 
  ggplot(aes(x = ethnicity, y = result, fill = ethnicity)) +
  geom_boxplot(alpha=0.5)+
  # using previously saved color palette 
  scale_fill_manual(values =  contrast_cols_palette)   +
  theme_bw()+
  # make x axis labes readable
  theme(axis.text.x = element_text(angle=50, vjust=0.75)) +
  # drop legend and Y-axis title
  theme(legend.position = "none") 

### Violin   ----------------------------------------------------------------
# the violin plot is an interesting alternative to show the distribution of a continuous variable along one or more categorical variables.
# it shows not only the summary statistics but also the shape and variability of the data
autism_pids %>% 
  ggplot(mapping = aes(y = age_inputed, x = ethnicity, fill = ethnicity)) +
  geom_violin(alpha=0.5, drop = FALSE) +
  # it can be enriched by adding with other geoms (e.g. points)
  geom_point(position = position_jitter(width = 0.1), size = 0.5)+ 
  # Stat_summary() function adds mean marker on plot
  scale_fill_manual(values =  contrast_cols_palette)  +
  # make x axis labes readable
  theme(axis.text.x = element_text(angle=50, vjust=0.75)) +
  # drop legend and Y-axis title
  theme(legend.position = "none")

# _________---------------------------------------------------------------------
# SAVING & EXPORTING OUTPUT ARTIFACTS ------------------------------------------
# Now I want to save the most polished plot I made into my output folder 

#### ... save a plot with `ggplot2::ggsave`  ---------------------------------- 
ggsave (hist_plot, filename = here::here("practice",  "data_output", "hist_plot.png"))

#### ... save a dataframe with `base::saveRDS`  ---------------------------------- 
saveRDS (object = autism_pids, file =  here::here("practice",  "data_output", "autism_pids_v2.Rds"))  

# notice I renamed while saving: next time I load it it will be called "autism_pids_v2"
autism_pids_v2 <- readRDS(here::here("practice",  "data_output", "autism_pids_v2.Rds"))


# _________-------------------------------------------------------------------
# FOUNDATIONS OF INFERENCE -------------------------------------------------
# (lo lascerei x lab # 2) -------------------------------------------------



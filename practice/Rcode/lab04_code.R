# "Lab 4:Mapping causal & predictive approaches"-----

# # [GOAL OF TODAY'S PRACTICE SESSION]{.r-fit-text} ----
# # R ENVIRONMENT SET UP & DATA ----
# ## Needed R Packages ----

# # Load pckgs for this R session ----
# --- General 
library(here) # A Simpler Way to Find Your Files   
library(skimr)    # Compact and Flexible Summaries of Data
library(paint)   # Fancy structure for data frames 
library(janitor)  # Simple Tools for Examining and Cleaning Dirty Data
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(gt) # Easily Create Presentation-Ready Display Tables
library(marginaleffects) # Marginal Effects for Model Objects
library(WeightIt) # Weighting for Covariate Balance in Observational Studies
# # --- Plotting & data visualization ----
# --- Plotting & data visualization
library(ggfortify)     # Data Visualization Tools for Statistical Analysis Results
library(ggpubr)        # 'ggplot2' Based Publication Ready Plots
library(patchwork) # The Composer of Plots
library(ggdag) # Analyze and Create Elegant Directed Acyclic Graphs #

# # --- Descriptive statistics and regressions ----
# --- Descriptive statistics and regressions
library(broom) # Convert Statistical Objects into Tidy Tibbles
library(Hmisc) # Harrell Miscellaneous
library(estimatr) # Fast Estimators for Design-Based Inference
library(modelsummary) # Summary Tables and Plots for Statistical Models and Data:
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(sandwich) #for robust variance estimation
library(survey) # Analysis of Complex Survey Samples
library(haven) # Import and Export 'SPSS', 'Stata' and 'SAS' Files
library(simstudy) # Simulation of Study Data
library(NHANES) # Data from the US National Health and Nutrition Examination Study  
library(mediation) # Causal Mediation Analysis

# Define a nice color palette from {MoMAColors}
# https://github.com/BlakeRMills/MoMAColors
clrs <- c("#9b2339", "#E7B800","#239b85", "#85239b", "#9b8523","#23399b", "#d8e600", "#0084e6","#399B23",  "#e60066" , "#00d8e6",  "#005ca1", "#e68000")

# # DATASETS for today ----
# ## [Importing Dataset 1 (NHANES)]{.r-fit-text} ----

set.seed(123) # Set seed for reproducibility
data(NHANES)
# # Select relevant columns and drop rows with missing values ----
# Select relevant columns and drop rows with missing values
nhanes_data <- NHANES %>%
  dplyr::select(ID, Gender, Age, Race1, Height, Weight, BMI, SmokeNow, PhysActive, PhysActiveDays,
         BPSysAve, BPDiaAve, TotChol, DirectChol, Diabetes, HealthGen,
         Education, HHIncomeMid, Poverty) %>%
  drop_na() %>% 
  # make it smaller 
  slice_sample(n = 1000) # Randomly select 1000 rows using

# # Take a quick look at the data ----
# Take a quick look at the data
#paint(nhanes_data)


# # VISUALLY EXPLORE CAUSAL PATHWAYS WITH DAGS ----
# ## DAG with `collider` ----

# # df DAG  ----
# df DAG 
dag_collid <- ggdag::dagify( Y ~ X + e, Z ~ X + Y, 
                      exposure = "X",outcome = "Y", #latent = "e",
                      # labels
                      labels = c("Z" = "Collider",  "X" = "Exposure",
                                 "Y" = "Outcome", "e" = "Unobserved \nerror"),
                      # positions
                      coords = list(
                        x = c(X = 0, Y= 4, Z = 2 , e = 5),
                        y = c(X = 0, Y = 0, Z = 2, e = 1) 
                      )) %>% 
  ggdag::tidy_dagitty() 

# # Plot DAG  ----
# Plot DAG 
dag_col_p <-  dag_collid %>% 
  ggdag::ggdag(., layout = "circle") +  # decided in dagify
  theme_dag_blank(plot.caption = element_text(hjust = 1)) +
  # Nodes 
   ggdag::geom_dag_node(aes(color = name)) +  
  # Labels dodged to avoid overlap
   ggdag::geom_dag_label(aes(label = label), color = "#4c4c4c", nudge_x = 0.7, nudge_y = 0.2) +  
  
   ggdag::geom_dag_text(color="white") +
   ggplot2::labs(title = "Causal map with COLLIDER (Z)" , 
       #subtitle = "X = exposure, Y = outcome, Z = collider, e = random error", 
       caption = ) +
  # Map colors to specific nodes
   ggplot2::scale_color_manual(values = c("X" = "#9b6723", "Y" = "#72aed8", "Z" = "#d02e4c",
                                "e" = "#A6A6A6"), 
                     guide = "none")    # Remove legend

dag_col_p

# ## DAG with `confounder`   ----
# # df DAG  ----
# df DAG 
dag_confounder <- ggdag::dagify(Y ~ X + Z + e, X ~  Z, 
                                exposure = "X", outcome = "Y", #latent = "e",
                                # labels
                                labels = c(
                                  "Z" = "Confounder",
                                  "X" = "Exposure",
                                  "Y" = "Outcome",
                                  "e" = "Unobserved \nerror"),
                                # positions
                                coords = list(
                                  x = c(X = 0, Y= 4, Z = 2 , e = 5),
                                  y = c(X = 0, Y = 0, Z = 2, e = 1) 
                                )) %>% 
  ggdag::tidy_dagitty()     # Add edge_type within pipe

# # Plot DAG  ----
# Plot DAG 
dag_conf_p <- dag_confounder %>% 
  ggdag::ggdag(., layout = "circle") + 
  ggdag::theme_dag_blank(plot.caption = element_text(hjust = 1)) +
  # Nodes 
  ggdag::geom_dag_node(aes(color = name)) +  
  # Labels dodged to avoid overlap
  ggdag::geom_dag_label(aes(label = label), color = "#4c4c4c", nudge_x = 0.7, nudge_y = 0.2) +  
  ggdag::geom_dag_text(color="white") +
  ggplot2::labs(title = "Causal map with CONFOUNDER (Z)" , #subtitle = " ",
        caption = ) +
  # Map colors to specific nodes
  ggplot2::scale_color_manual(values = c("X" = "#9b6723", "Y" = "#72aed8", "Z" = "#d02e4c",
                                "e" = "#A6A6A6"), 
                     guide = "none")   
dag_conf_p


# ## DAG with `mediator` ----

# # df DAG  ----
# df DAG 
dag_med <- ggdag::dagify( Y ~ Z + e,  Z ~ X, exposure = "X", outcome = "Y",  #latent = "e",
                   # labels
                   labels = c(
                     "Z" = "Mediator",
                     "X" = "Exposure",
                     "Y" = "Outcome",
                     "e" = "Unobserved \nerror"),
                   # positions
                   coords = list(
                     x = c(X = 0, Y= 4, Z = 2 , e = 5),
                     y = c(X = 0, Y = 0, Z = 2, e = 1) 
                   )) %>% 
  ggdag::tidy_dagitty()     # Add edge_type within pipe

# # Plot DAG  ----
# Plot DAG 
dag_med_p <-  dag_med %>% 
  ggdag::ggdag(., layout = "circle") +  # decided in dagify
  ggdag::theme_dag_blank(plot.caption = element_text(hjust = 1)) +
  # Nodes 
  ggdag::geom_dag_node(aes(color = name)) +  
  # Labels dodged to avoid overlap
  ggdag::geom_dag_label(aes(label = label), color = "#4c4c4c", nudge_x = 0.7, nudge_y = 0.2) +  
  
  ggdag::geom_dag_text(color="white") +
  ggplot2::labs(title = "Causal map with MEDIATOR (Z)" , 
       #subtitle = "X = exposure, Y = outcome, Z = collider, e = random error", 
       caption = ) +
  # Map colors to specific nodes
  ggplot2::scale_color_manual(values = c("X" = "#9b6723", "Y" = "#72aed8", "Z" = "#d02e4c",
                                "e" = "#A6A6A6"), guide = "none")    # Remove legend
dag_med_p


# # CAUSAL MODELING WITH `CONFOUNDER` ----
# ## DAG with `confounder`   ----

# # df DAG  ----
# df DAG 
dag_conf_p

# ## Example of CONFOUNDER ----

data(NHANES)
# # Select relevant columns and drop rows with missing values ----
# Select relevant columns and drop rows with missing values
nhanes_conf <- NHANES %>%
  dplyr::select(ID, Age, SmokeNow, BPSysAve) %>%
  tidyr::drop_na()
  
# # Take a quick look at the data ----
# Take a quick look at the data
paint::paint(nhanes_conf)

# ## Visualisation of CONFOUNDER ----

#| output-location: slide 

# # Scatterplot of Age and Blood Pressure by Smoking Status ----
# Scatterplot of Age and Blood Pressure by Smoking Status
ggplot2::ggplot(nhanes_conf, aes(x = Age, y = BPSysAve, color = SmokeNow)) +
  geom_point(alpha = 0.6) +
  # linear rel 
  geom_smooth(method = "lm", se = FALSE,  size = 1.5,  linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c("No" = "#1b9e77", "Yes" = "#d95f02")) +
  facet_wrap(~ SmokeNow) + 
  labs(
    title = "Scatterplot of Age and Blood Pressure by Smoking Status",
    x = "Age",
    y = "Blood Pressure (mmHg)",
    color = "Smokers"
  )


# # [ONE WAY OF DEALING WITH CONFOUNDERS]{.smaller} ----
# ## [Regression analysis: `controlling for` the confounder]{.r-fit-text} ----

# # Unadjusted linear model  ----
# Unadjusted linear model 
model_unadjusted <- lm(BPSysAve ~ SmokeNow, data = nhanes_conf)
summary(model_unadjusted)$coefficients


# # Adjusted model ----
# Adjusted model
model_adjusted <- lm(BPSysAve ~ SmokeNow + Age, data = nhanes_conf)
summary(model_adjusted)$coefficients

# ## [Compare models (without and with `confounder`)]{.r-fit-text} ----

#| output-location: column

# # Create the table with a custom statistic formatter ----
# Create the table with a custom statistic formatter
conf_sum <- modelsummary::msummary(
  list(
    "NO Confounder" = model_unadjusted, 
    "Confounder" = model_adjusted
  ),
  output = "gt",
  statistic = c(
    "conf.int",
    "s.e. = {std.error}"
  ), 
  fmt = "%.3f", 
  gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|RMSE',
  stars = c('*' = .05, '**' = .01)
)

# # Render the table directly ----
# Render the table directly
conf_sum  %>%    # text and background color
  tab_style(
    style = cell_text(weight = "bold", color = "#005ca1"),
    locations = cells_column_labels(
      columns = c("NO Confounder", "Confounder"))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = c(1,4,7))) %>% 
  tab_style(style = cell_fill(color = 'lightblue'),
            locations = cells_body(rows = 4:5)) %>% 
  # Make 'Age' font color red
  tab_style(style = cell_text(color = "#d02e4c"),
    locations = cells_body(rows = c(7,8,9))
  )

# ## [Compare predicted values from 2 models]{.r-fit-text} ----

# # Add predicted values from the unadjusted and adjusted models ----
# Add predicted values from the unadjusted and adjusted models
nhanes_conf <- nhanes_conf %>%
  dplyr::mutate(
    pred_unadjusted = predict(model_unadjusted),  # Predicted BPSysAve from unadjusted model
    pred_adjusted = predict(model_adjusted)       # Predicted BPSysAve from adjusted model
  )

# # Reshape the data into long format using pivot_longer() ----
# Reshape the data into long format using pivot_longer()
nhanes_long <- nhanes_conf %>%
  tidyr::pivot_longer(cols = c(pred_unadjusted, pred_adjusted), 
               names_to = "model", values_to = "predicted") %>%
  dplyr::mutate(model = recode(model, 
                        pred_unadjusted = "Unadjusted: SmokeNow on BPSysAve", 
                        pred_adjusted = "Adjusted: SmokeNow + Age on BPSysAve")) %>% 
  #Reorder the 'model' factor to change the order of the facets
  dplyr::mutate(model = factor(
   model,   levels = c("Unadjusted: SmokeNow on BPSysAve", "Adjusted: SmokeNow + Age on BPSysAve")
))


dim(nhanes_conf)
dim(nhanes_long)

# ## [Plot predicted values from 2 models]{.r-fit-text} ----

#| label: fig-pred_conf
#| output-location: slide 
#| fig-cap: "As age causes high blood pressure, but also determines smoking status, the Adjusted model (*right*) gives us a more credible relationship between smoking and blood pressure."  
#| fig-cap-location: bottom

# # Violin plot with dashed line connecting the two conditions ----
# Violin plot with dashed line connecting the two conditions
ggplot(nhanes_long, aes(x = factor(SmokeNow), y = BPSysAve, fill = model)) +
  # Create the violin plot to show the distribution of blood pressure
  geom_point(color = "grey", alpha = 0.4, position = position_dodge(width = 0.3) ) +
  # Overlay the predicted dashed line between the two smoking conditions
  geom_line(aes(y = predicted, group = model, color = model), 
            size = 0.8, linetype = "dashed", position = position_dodge(width = 0.3)) +
  # Facet vertically for unadjusted and adjusted models
  facet_wrap(model ~ ., scales = "free_y",ncol = 2) +
  # Add labels and title
  labs(title = "Confounder Analysis: Predicted values in Unadjusted vs Adjusted Regression models",
       subtitle = "Age = Confounder",
       x = "Smoking Status",
       y = "Systolic Blood Pressure (BPSysAve)",
       color = "Model",
       fill = "Model"
       ) +
  # Customize colors for better contrast
  scale_fill_manual(values = c("Unadjusted: SmokeNow on BPSysAve" ="lightcoral" , 
                               "Adjusted: SmokeNow + Age on BPSysAve" = "lightblue")) +
  scale_color_manual(values = c("Unadjusted: SmokeNow on BPSysAve" = "red", 
                                "Adjusted: SmokeNow + Age on BPSysAve" = "blue")) +
  # Minimal theme for clarity
  theme_minimal()+
  theme(legend.position = "none")

# # CAUSAL MODELING WITH `MEDIATOR` ----
# ## DAG with `mediator`   ----
# # df DAG  ----
# df DAG 
dag_med_p

# # [The case of a mediator]{.r-fit-text} ----

data(NHANES)
# # Select relevant columns and drop rows with missing values ----
# Select relevant columns and drop rows with missing values
nhanes_med <- NHANES %>%
  dplyr::select(Gender, Age, BPSysAve, BMI, PhysActiveDays, Diabetes, SmokeNow) %>%
  drop_na()

# # Take a quick look at the data ----
# Take a quick look at the data
summary(nhanes_med)

# ## [Fitting unadjusted (total) model]{.r-fit-text} ----

# # Unadjusted model: total effect of smoking on blood pressure ----
# Unadjusted model: total effect of smoking on blood pressure
total_effect_model <- lm(BPSysAve ~ PhysActiveDays, data = nhanes_med)
summary(total_effect_model)$coefficients

# ## [Set up the mediation models 1/2]{.r-fit-text} ----

# # Mediator model: SmokeNow affects BMI ----
# Mediator model: SmokeNow affects BMI
mediator_model <- lm(BMI ~ PhysActiveDays, data = nhanes_med)
summary(mediator_model)$coefficients

# ## [Set up the mediation models 2/2]{.r-fit-text} ----

# # Outcome model: PhysActiveDays and BMI affect BPSysAve ----
# Outcome model: PhysActiveDays and BMI affect BPSysAve
outcome_model <- lm(BPSysAve ~  PhysActiveDays + BMI, data = nhanes_med)
summary(outcome_model)$coefficients

# ## [Compare 3 models]{.smaller} ----
med_sum <- modelsummary::msummary(
  list("BPSysAve ~ Total Effect" = total_effect_model,
       "BMI ~ Mediator Effect" = mediator_model,
       "BPSysAve ~ Outcome" = outcome_model), 
  output = "gt",
  statistic = c(
    "conf.int",
    "s.e. ={std.error}"),
  fmt = "%.3f", # format
  gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|RMSE',
  stars = c('*' = .05, '**' = .01))

# # Render the table ----
# Render the table
med_sum %>% 
  tab_spanner(label = "Tot (unadj) Mod.",columns = 2) %>%
  tab_spanner(label = "Med Mod.", columns = 3) %>%
  tab_spanner(label = "Outcome Mod.",columns = 4) %>%
   tab_style(
    style = cell_text(weight = "bold", color = "#7f173d"),
    locations = cells_column_labels(columns = 3)) %>% 
  tab_style(
    style = cell_text(weight = "bold", color = "#005ca1"),
    locations = cells_column_labels(columns = c(2,4))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = c(1,4,7))) %>% 
  # Highlight X 
   tab_style(style = cell_fill(color = 'lightblue'),
            locations = cells_body(rows = 4:6)) %>% 
  # Make 'MEdiator' font color red
  tab_style(style = cell_text(color = "#d02e4c"),
    locations = cells_body(rows = 7:9)) 

# ## [Calculate the Indirect, Direct, and Total Effects (1/2)]{.r-fit-text} ----
# # Check the names of the coefficients in models... ----
# Check the names of the coefficients in models...
names(coef(mediator_model))     # [1] BMI ~ PhysActiveDays 
names(coef(outcome_model))      # [1] BPSysAve ~  PhysActiveDays + BMI    
names(coef(total_effect_model)) # [1] BPSysAve ~ PhysActiveDays

# ## [Calculate the Indirect, Direct, and Total Effects (2/2)]{.r-fit-text} ----

# # TOTAL effect: The effect of PhysActiveDays on BPSysAve without adjusting for BMI ----
# TOTAL effect: The effect of PhysActiveDays on BPSysAve without adjusting for BMI
total_effect <- coef(total_effect_model)["PhysActiveDays"]# 0.59
total_effect
# # DIRECT effect: The effect of PhysActiveDays on BPSysAve after adjusting for BMI. ----
# DIRECT effect: The effect of PhysActiveDays on BPSysAve after adjusting for BMI.
direct_effect <- coef(outcome_model)["PhysActiveDays"]# 0.621 
direct_effect
# # Indirect effect: The portion of the effect on BPSysAve that is mediated through BMI ----
# Indirect effect: The portion of the effect on BPSysAve that is mediated through BMI
indirect_effect <- coef(mediator_model)["PhysActiveDays"] * coef(outcome_model)["BMI"] # [-0.0821 X 0.382]  
indirect_effect
# # Proportion mediated: The proportion of the total effect that is mediated through BMI ----
# Proportion mediated: The proportion of the total effect that is mediated through BMI
proportion_mediated <- glue::glue("{round(indirect_effect / total_effect *100, 2)}%")
proportion_mediated #  -0.0531

# ## [Visualise the Indirect, Direct, and Total Effects]{.r-fit-text} ----
# # Create a data frame for the effects ----
# Create a data frame for the effects
effects_df <- data.frame(
  Effect = c("Total Effect", "Direct Effect", 
             "Indirect Effect"),
  Value = c(total_effect, direct_effect, 
            indirect_effect)
)

# # Create the bar plot ----
# Create the bar plot
ggplot(effects_df, aes(x = Effect, y = Value,
                       fill = Effect)) +
  geom_bar(stat = "identity") +
  labs(title = "Mediation Analysis: Total, Direct, and Indirect Effects", 
       subtitle = "Effect (coefficient) of Physical Activity on Systolic Blood Pressure",
       y = "", x = "") +
  theme_minimal()

# ## [Interpreting the results ]{.r-fit-text} ----
# # [Causal mediation analysis using `mediation`]{.smaller} ----
# ## [Using `mediation` package]{.r-fit-text} ----

# # recall ----
# recall
#mediator_model[["call"]][["formula"]]
#outcome_model[["call"]][["formula"]]

# # Calculate the mediation effect ----
# Calculate the mediation effect
mediation_model <- mediate(mediator_model, outcome_model, 
                           treat = "PhysActiveDays", 
                           mediator = "BMI", 
                           boot=TRUE, sims=500)

summary(mediation_model)$d1  # Direct effect
summary(mediation_model)$z1  # Indirect effect
summary(mediation_model)$tau.coef  # Total effect

# ## [Using `mediation` pckg - results]{.r-fit-text} ----

summary(mediation_model)

# ## [Using `mediation` pckg - prediction]{.r-fit-text} ----

# # Add predicted values for BMI, BPSysAve (adjusted), and BPSysAve (total effect) ----
# Add predicted values for BMI, BPSysAve (adjusted), and BPSysAve (total effect)
nhanes_med_pred <- nhanes_med %>%
  mutate(
    pred_bmi = predict(mediator_model),         # Predicted BMI (mediator model)
    pred_bps_adj = predict(outcome_model),      # Predicted BPSysAve (adjusted for Age)
    pred_bps_total = predict(total_effect_model) # Predicted BPSysAve (total effect)
  )

nhanes_med_pred[1:3, c(1:2, 8:10)]

# ## [Plot prediction results 1/2]{.r-fit-text} ----

# # Check column names ----
# Check column names
colnames(nhanes_med_pred)

# # Reshape the data into long format for faceting ----
# Reshape the data into long format for faceting
nhanes_long <- nhanes_med_pred %>%
  gather(key = "model", value = "predicted",  pred_bps_adj, pred_bps_total) %>%
  mutate(model = recode(model, 
                        #pred_bmi = "Mediator Effect: BPSysAve on BMI", 
                        pred_bps_adj = "Adj. Effect: PhysActiveDays + BMI on BPSysAve", 
                        pred_bps_total = "Total Effect: PhysActiveDays on BPSysAve"))

# ## [Plot prediction results 2/2]{.r-fit-text} ----

#| output-location: slide 

# # Plot with vertically aligned facets ----
# Plot with vertically aligned facets
ggplot(nhanes_long, aes(x = BMI)) +
  # Actual data points for BPSysAve
  geom_point(aes(y = BPSysAve), color = "black", shape = 16, alpha = 0.5) +  
  # Predicted values from different models
  geom_line(aes(y = predicted, color = model), size = 1) +  
  # Facet vertically
  facet_grid(model ~ ., scales = "free_y") +  
  labs(title = "Mediation Analysis: Total, Adjusted, and Mediator Effects",
       x = "BMI",
       y = "Blood Pressure | Age",
       color = "Effect Type") +
  theme_minimal()



# # DEFINING POPULAR CAUSAL OUTCOME EFFECTS  ----
# # [CALCULATE COMMONLY USED ESTIMANDs (ATE, ATT, ATU)]{.smaller} ----
# ## [Establishing some common symbols]{.r-fit-text} ----
# ## [EXAMPLE with toy case]{.r-fit-text} ----

basic_po <- tribble(
  ~id, ~age,    ~treated, ~outcome_1, ~outcome_0,
  1,   "Old",   1,        80,         60,
  2,   "Old",   1,        75,         70,
  3,   "Old",   1,        85,         80,
  4,   "Old",   0,        70,         60,
  5,   "Young", 1,        75,         70,
  6,   "Young", 0,        80,         80,
  7,   "Young", 0,        90,         100,
  8,   "Young", 0,        85,         80
) |> 
  dplyr::mutate(
    ice = outcome_1 - outcome_0,
    outcome = ifelse(treated == 1, outcome_1, outcome_0)
  )

# ## [ITE in toy example example (*all* outcomes!)]{.r-fit-text} ----
basic_po <- tribble(
  ~id, ~age,    ~treated, ~outcome_1, ~outcome_0,
  1,   "Old",   1,        80,         60,
  2,   "Old",   1,        75,         70,
  3,   "Old",   1,        85,         80,
  4,   "Old",   0,        70,         60,
  5,   "Young", 1,        75,         70,
  6,   "Young", 0,        80,         80,
  7,   "Young", 0,        90,         100,
  8,   "Young", 0,        85,         80
) |> 
  dplyr::mutate(
    ice = outcome_1 - outcome_0,
    outcome = ifelse(treated == 1, outcome_1, outcome_0)
  )

# # FORMAT TABLE  ----
# FORMAT TABLE 
basic_po |> 
  dplyr::select(
    id, age, treated, outcome_1, outcome_0, ice, outcome) |> 
  gt() |> 
  sub_missing(missing_text = "…") |>
  fmt_number(
    columns = c(starts_with("outcome"), ice),
    decimals = 0) |> 

  # Column labels
  cols_label(
    id = "ID",
    age = md("$$Z_i$$"),
    treated = md("$$X_i$$"),
    outcome_0 = md("$$Y^0_i$$"),
    outcome_1 = md("$$Y^1_i$$"),
    outcome = md("$$Y_i$$"),
    ice = md("$$Y^1_i - Y^0_i$$")) |>
  
  # Level 1 spanner labels
  tab_spanner(
    label = "Age", columns = age, 
    level = 1, id = "level1_a_po") |> 
  tab_spanner(
    label = "Treated", columns = treated, 
    level = 1, id = "level1_b_po") |> 
  tab_spanner(
    label = "Potential outcomes",
    columns = c(outcome_1, outcome_0),
    level = 1, id = "level1_c_po") |> 
  tab_spanner(
    label = "ITE or \\(\\delta_i\\)", 
    #label = "ITE", 
  columns = ice, 
    level = 1, id = "level1_d_po") |> 
  tab_spanner(
    label = "Outcome", columns = outcome, 
    level = 1, id = "level1_e_po") |> 
  
  # Level 2 spanner labels
  tab_spanner(
    label = "Confounder",
    columns = age,
    level = 2, id = "level2_a_po") |> 
  tab_spanner(
    label = "Treatment", columns = treated, 
    level = 2, id = "level2_b_po") |> 
  tab_spanner(
    label = "Unobservable",
    columns = c(outcome_1, outcome_0, ice), 
    level = 2, id = "level2_c_po") |> 
  tab_spanner(
    label = "Realized", columns = outcome, 
    level = 2, id = "level2_d_po") |> 
  
  # Style stuff
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(spanners = starts_with("level1")),
      cells_column_labels(columns = "id")
    )) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_column_spanners(spanners = starts_with("level2"))) |> 
  tab_style(
    style = list(
      cell_fill(color = clrs[2], alpha = 0.5)),
    locations = cells_body(rows = treated == 1)) |> 
  # tab_footnote(
  #   footnote = "ITE = individual causal effect",
  #   locations = cells_column_spanners(spanners = "level1_d_po")) |> 
  opt_footnote_marks(marks = "standard") |> 
  opt_horizontal_padding(scale = 3)

# ## [ATE  = Average Treatment Effect]{.r-fit-text} ----
# # ATE upon stratification by Age ----
# ATE upon stratification by Age
(4/8*(20)) + (4/8*(-11.667))

# ## [ATT = Average Treatment Effect on Treated]{.r-fit-text} ----
# ## [ATU = Average Treatment Effect on Untreated]{.r-fit-text} ----
# ## [Relation among ATE, ATT and ATU]{.r-fit-text} ----
# ## [Relation among ATE, ATT and ATU]{.r-fit-text} ----
# ## [Relation among ATE, ATT and ATU (R)]{.r-fit-text} ----

# # OBTAIN ATT, ATU ----
# OBTAIN ATT, ATU
effect_types <- basic_po |> 
  dplyr::group_by(treated) |> 
  dplyr::summarize(effect = mean(ice), n = n() ) |> 
  dplyr::mutate(prop = n / sum(n),
    weighted_effect = effect * prop) |> 
  dplyr::mutate(estimand = case_match(treated, 0 ~ "ATU", 1 ~ "ATT"), .before = 1)

effect_types

# # OBTAIN ATE ----
# OBTAIN ATE
effect_types |> dplyr::summarize(ATE = sum(weighted_effect))


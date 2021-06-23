# Modelling Framework
library(tidymodels) # Predictive Framework
library(caret) # Predictive Framework

# Data handling Packages
library(tidyverse) # Data handling/ Graphics
library(data.table) # Data handling

#Formating and visualizations
library(knitr) # Table
library(skimr) # Summarise dataframe
library(corrplot) # Correlation plot


# Load data
df_raw <-  data.table::fread("data/Churn_Modelling.csv")

# Summarise datafrmae
skim(df_raw)


# Convert all names to lower case
df <-
  df_raw %>%
  set_names(., tolower(names(.))) %>%
  select(-c(rownumber, customerid))


## Exploring Target Variable

# Visualise the distribution of the data
df %>%
  select(exited) %>%
  mutate(exited = factor(exited, levels = c(0,1), labels = c("Remain", "Churn"))) %>%
  group_by(exited) %>%
  count() %>%
  ungroup() %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = exited, y = p)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL, y = NULL) +
  ggtitle("Proportion of Loan Repayments & Default")

# Change the variable type from numeric to factor
df <- 
  df %>% 
  mutate(exited = factor(exited, levels = c(1,0), labels = c("Churn", "Remain")))

# Random forrests


# Create an index to split the data
set.seed(423827)
l_split_index <- 
  initial_split(data = df,
                prop = 0.70,
                strata = exited
  )

# Create training and testing set using the index
df_train <- training(l_split_index)
df_test <- testing(l_split_index)

# Check same distribution of output
bind_rows(
  as.data.frame(round(prop.table(table(df_train$exited)),4)) %>% mutate(Data = "Train"),
  as.data.frame(round(prop.table(table(df_test$exited)),4)) %>% mutate(Data = "Test")
) %>%
  spread(Var1, Freq) %>%
  kable(style = "pandoc",
        align = c('c','c','c'))


# Preprocessing
recipe <-
  df_train %>%
  recipe(exited ~ .) %>%
  step_rm(surname) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  prep()

# Apply processing to test and training data
df_baked_train <- recipe %>% bake(df_train) # Preprocessed training
df_baked_test <- recipe %>% bake(df_test) # Preprocessed testing

rm(df_train, df_test) # remove old data




#Create control function for training with 10 folds and keep 3 folds for training. search method is grid.
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid')
#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (1:15)) 

df_baked_train$exited <- as.factor(df_baked_train$exited)

# Perform gridsearch for finding optimal parameters
rf_gridsearch <- train(exited ~ .,
                       data = df_baked_train,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid)
print(rf_gridsearch)
plot(rf_gridsearch)

rf_pred <- predict(rf_gridsearch, df_baked_test)

# compare predicted outcome and true outcome
confusionMatrix(rf_pred, as.factor(df_baked_test$exited))

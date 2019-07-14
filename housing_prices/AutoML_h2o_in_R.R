library(tidyverse)
library(data.table)
library(h2o)
library(Amelia)

setwd('C:\\Users\\renan\\Desktop\\kaggle_competitions\\housing_prices')

data <- tbl_df(fread('train.csv'))
names(data)

data %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  gather() %>%
  arrange(desc(value))

missmap(data)

# Drop Id also
data <-  data %>%
  select(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature) %>%
  select(-Id)

# Input with category 'None' the missing values for factors
data <- data %>%
  mutate_if(is.character, function(x) replace_na(x, 'None'))

missmap(data)

# Input with median values for numericals
data <- data %>%
  mutate_if(is.numeric, function(x) replace_na(x, median(x, na.rm = TRUE)))

missmap(data)


# Create log value for y
data <- data %>%
  mutate(logSalePrice = log(SalePrice)) %>%
  select(-SalePrice)


# Starts of AutoML
h2o.init()

# Convert data to h2o frame
data_hf <- as.h2o(data)


# Identify target and features
y <- 'logSalePrice'
x <- setdiff(colnames(data_hf), y)


# Split data into train & validation sets
sframe <- h2o.splitFrame(data = data_hf, ratios = 0.80, seed = 42)
train <- sframe[[1]]
valid <- sframe[[2]]

# Metric for regression (deviance is the default). Check documentation here http://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html
automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           validation_frame = valid,
                           max_runtime_secs = 60 * 3,
                           #sort_metric = "deviance",
                           sort_metric = "RMSE")

lb <- automl_model@leaderboard
View(as.data.frame(lb))


# Assign best model new object name
aml_leader <- automl_model@leader

# Look at best model
summary(aml_leader)


# For predicting, we need to make the same data wrangling that we did for training
test_raw <- tbl_df(fread('test.csv'))
test <-  test_raw %>%
  select(-Alley, -FireplaceQu, -PoolQC, -Fence, -MiscFeature) %>%
  select(-Id) %>%
  mutate_if(is.character, function(x) replace_na(x, 'None')) %>%
  mutate_if(is.numeric, function(x) replace_na(x, median(x, na.rm = TRUE))) %>%
  as.h2o()

y_pred_raw <- h2o.predict(aml_leader, test)

# Pull 'extracts' a specific column of a tibble for a vector (https://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector)
y_pred <-  pull(exp(as_tibble(y_pred_raw)), predict)


final_sub <- data.frame(Id = test_raw$Id, SalePrice = y_pred)
write.csv(final_sub, file = "automl_h2o_first_180sec_sub.csv", row.names = FALSE)

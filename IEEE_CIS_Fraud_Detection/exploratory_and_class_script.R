library(tidyverse)
library(magrittr)
library(caret) # Necessary to install Rtools and update rlang
library(naniar)
library(PRROC)

train_transaction <- read_csv('data/train_transaction.csv')
train_identity <- read_csv('data/train_identity.csv')

train_transaction %<>%
  select(-starts_with('V'))

dim(train_transaction)
dim(train_identity)

# Transaction missing dealing ----

# Visualizing Missing Data
train_transaction %>%
  sample_frac(0.005) %>%
  vis_miss()

# Checking missing percentage of each variable
train_transaction %>%
  summarise_all(funs(sum(is.na(.) / length(.)))) %>%
  gather() %>%
  arrange(desc(value))

# Dicard columns with more than 10% of missing data
train_transaction %<>%
  discard(~sum(is.na(.x))/length(.x) >= 0.1)

# Visualize the missing data after removing these columns
train_transaction %>%
  sample_frac(0.01) %>%
  vis_miss()


# Identity missing dealing ----

train_identity %>%
  sample_frac(0.01) %>%
  vis_miss()

train_identity %<>% 
  discard(~sum(is.na(.x))/length(.x) >= 0.1)

train_identity %>%
  sample_frac(0.01) %>%
  vis_miss()

# Get only the complete.cases ----

train_transaction %<>% drop_na()
train_identity %<>% drop_na()


# Merging datasets

full_data <- full_join(train_transaction, train_identity, on = 'TransactionID')

dim(full_data)

full_data %>%
  sample_frac(0.01) %>%
  vis_miss()

# Too many TransactionsID do not have respective identity informations.
# Conclusion: we first will develop models ignoring identity data.


train_transaction %>%
  ggplot(aes(x = isFraud)) +
  geom_bar()

# Base model ----

base_logistic <- glm(isFraud ~ ., data = train_transaction, family=binomial(link='logit'))

p <- predict(base_logistic, train_transaction, type = "response")

# Check AUC and plot ROC curve
PRROC_obj <- roc.curve(scores.class0 = p, 
                       weights.class0 = train_transaction$isFraud,
                       curve = TRUE)
plot(PRROC_obj)
auc <- PRROC_obj$auc

# Alternative Way:
#library(caTools)
#auc <- colAUC(p, train_transaction$isFraud) # plotROC = TRUE

# Assessing local validation of many models with Cross-Validation
trainIndex <- createDataPartition(train_transaction$isFraud, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)








PRROC_obj <- roc.curve(scores.class0 = p, 
                       weights.class0 = train_transaction$isFraud,
                       curve = TRUE)
plot(PRROC_obj)

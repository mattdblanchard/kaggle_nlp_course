# create list of packages
packages <- c("tidyverse", "quanteda", "caret", "textcat")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

# load packages
lapply(packages, library, character.only = TRUE)

# read data and remove reviews that are not in English
d <- read_csv("data/yelp_ratings.csv")
# filter(!str_detect(text, "终于系scarborough开左分店")) %>% 
# mutate(lang = textcat(text)) %>% 
# filter(lang %in% c("english", "scots", "middle_frisian", "frisian", "catalan", "latin", "romanian", "danish", "slovak-ascii", NA, "manx",
#                    "afrikaans", "rumantsch", "esperanto", "finnish"))

# Tokenize SMS text messages.
# remove numbers, punctuation, symbols and split hyphenated words
d.tokens <- tokens(d$text, what = "word1", 
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, split_hyphens = TRUE)

# clean tokens further: lower case, remove stopwords, and stem
d.tokens <- d.tokens %>% 
  tokens_tolower() %>% 
  tokens_select(stopwords(), selection = "remove") %>% 
  tokens_wordstem(language = "english")

# Create our first bag-of-words model.
# dfm() takes in tokens and creates a document-frequency matrix (dfm)
d.tokens.dfm <- dfm(d.tokens, tolower = FALSE)

# to inspect
# train.tokens.matrix <- as.matrix(train.tokens.dfm)

# Per best practices, we will leverage cross validation (CV) as
# the basis of our modeling process.

# Setup data frame with features and labels.
d.tokens.df <- bind_cols(y_labels = d$sentiment, convert(d.tokens.dfm, to = "data.frame"))

# subset data
d.tokens.df <- bind_cols(y_labels = d$sentiment[1:5000], convert(d.tokens.dfm[1:5000,1:5000], to = "data.frame"))

# train <- train %>% 
#   mutate(sentiment = factor(sentiment, levels = 0:1, labels = c("low", "high")))

# Cleanup column names. Will modify only names that are not syntactically valid
# make.names turns ' into . so it creates duplicate names. Need to manually change those name before running make.names
# d.tokens.df <- d.tokens.df %>% rename(b_c = `b'c`, o_noir = `o'noir`)

names(d.tokens.df) <- make.names(names(d.tokens.df))

# check that each column name only occurs once
data.frame(name = names(d.tokens.df)) %>% 
  group_by(name) %>% 
  mutate(n = n()) %>% 
  filter(n != 1)

# train.tokens.df <- as_tibble(train.tokens.df)
# creating training and test data
# Use caret to create a 70%/30% stratified split. 
# Set the random seed for reproducibility.
set.seed(32984)
indexes <- createDataPartition(d.tokens.df$y_labels, times = 1,
                               p = 0.7, list = FALSE)

train.tokens.df <- d.tokens.df[indexes,]
test.tokens.df <- d.tokens.df[-indexes,]

# Verify proportions are equivalent in the train and test datasets
prop.table(table(train.tokens.df$y_labels))
prop.table(table(test.tokens.df$y_labels))

# Use caret to create stratified folds for 10-fold cross validation repeated 
# 3 times (i.e., create 30 random stratified samples)
# we are using stratified cross validation because we have a class imbalance 
# in the data (negative < positive reviews) each random sample taken is representative 
# in terms of the proportion of spam and ham in the dataset
# why are we repeating the cross validation 3 times? If we take the time to conduct 
# cross validation multiple times we should get more valid estimates. If we take more 
# looks at the data the estimation process will be more robust.

set.seed(48743)
cv.folds <- createMultiFolds(train.tokens.df$y_labels, k = 10, times = 3)

cv.cntrl <- trainControl(method = "repeatedcv", 
                         number = 10,
                         repeats = 3, 
                         index = cv.folds) # since we want stratified cross-validation we need to specify the folds

# Our data frame is non-trivial in size. As such, CV runs will take 
# quite a long time to run. To cut down on total execution time, use
# the doSNOW package to allow for multi-core training in parallel.
#
# WARNING - The following code is configured to run on a workstation-
#           or server-class machine (i.e., 12 logical cores). Alter
#           code to suit your HW environment.

library(doSNOW) # doSNOW works on mac and windows out of box. Some other parallel processing packages do not.

{
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on 7 logical cores.
  # check how many cores your machine has available for parallel processing
  # keep 1 core free for the operating system
  # parallel::detectCores()
  cl <- makeCluster(3, type = "SOCK") # effectively creates multiple instances of R studio and uses them all at once to process the model
  registerDoSNOW(cl) # need to tell R to process in parallel
  
  # As our data is non-trivial in size at this point, use a single decision
  # tree alogrithm (rpart trees) as our first model. We will graduate to using more 
  # powerful algorithms later when we perform feature extraction to shrink
  # the size of our data.
  # e.g., could use random forest (rf) or XGBoost (xgbTree) instead by changing the method
  # formula means predict (~) label using all other variables (.) in dataset
  # trControl is the cross validation parameters
  # tuneLength allows you to set the number of different configurations of the algorithm to test
  # it selects the one best one and builds a model using that configuration
  rpart.cv.1 <- train(y_labels ~ ., data = train.tokens.df, 
                      method = "rpart", 
                      trControl = cv.cntrl, 
                      tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  
  # Total time of execution on workstation was approximately 4 minutes. 
  total.time <- Sys.time() - start.time
  total.time
}

# Check out our results.
# samples = rows
# predictors = features
# best performing model had 94.78% accuracy
# that's pretty good for a simple model with no tuning or feature engineering
rpart.cv.1

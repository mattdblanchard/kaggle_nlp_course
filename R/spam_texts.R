# This code was recycled from another NLP course I completed because it uses the same spam dataset
# This code goes far beyond the content of Kaggle's course
# The other course is:
# Data Science Dojo's Intro to Text Analytics with R
# # https://code.datasciencedojo.com/datasciencedojo/tutorials/tree/master/Introduction%20to%20Text%20Analytics%20with%20R
# # Videos on youtube: https://youtu.be/4vuw0AsHeGw

# create list of packages
# irlba package necessary to perform truncated SVD - means you can specify the x number of most significant extracted features
packages <- c("tidyverse", "quanteda", "caret", "irlba", "randomForest", "lsa", "doSNOW")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

# load packages
lapply(packages, library, character.only = TRUE)

# read data and calculate text length
d <- read_csv("data/spam.csv") %>% 
  mutate(text_length = nchar(text))

# look at a histogram of text_length
# can see that overwhelming number of short texts are ham
# text_length could be a useful feature when we fit ML
d %>% 
  ggplot(aes(x = text_length, fill = label)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Text Count", x = "Length of Text",
       title = "Distribution of Text Lengths with Class labels")

# creating training and test data
# Use caret to create a 70%/30% stratified split. 
# Set the random seed for reproducibility.
set.seed(32984)
indexes <- createDataPartition(d$label, times = 1,
                               p = 0.7, list = FALSE)

train <- d[indexes,]
test <- d[-indexes,]

# Verify proportions are equivalent in the train and test datasets
prop.table(table(train$label))
prop.table(table(test$label))

# need to convert unstructured text data into a structured dataframe
# first need to decompose text document into distinct pieces (tokens)
# each word or punctuation becomes a single token
# then we construct a document-frequency matrix (aka data frame) where:
# - each row represents a document (e.g., a single complete text message)
# - each column represents a distinct token (word or punctuation)
# - each cell is a count of the token for a document

# IMPORTANT: this method does not preserve the word ordering
# this setup is known as the bag of words (BOW) model
# there is a way to add word ordering back into the model (engram) 
# we will look at this later
# the BOW model is typical starting point for text analysis

# some considerations:
# - do you want all tokens to be terms in the model?
#   * casing (e.g., If v if)? better to remove capitalisation by making everything lower case
#   * puncuation (e.g., ", !, ?)? word ordering not preserved so better to remove punctuation
#   * numbers? sometimes depends what you are examining
#   * every word (e.g., the, an, a)? aka stop words typically dont add anything so remove
#   * symbols (e.g., <, @, #)? can be meaningful so depends on your data and questions
#   * similar words (e.g., ran, run, runs, running)? aka stemming - is it ok to collapse similar words into single representation? usually better to collapse. different contexts of use can often be derived from other variables

# PRE-PROCESSING IS MAJOR PART OF TEXT ANALYTICS

# Text analytics requires a lot of data exploration, data pre-processing
# and data wrangling. Let's explore some examples.

# HTML-escaped ampersand character. This is an example of how html records &
# there are 5 characters instead of 1
# could replace all instances of escaped sequence with and or & or something else
# or could remove & and ; to leave amp behind
train$text[20]

# HTML-escaped '<' (&lt;) and '>' (#&gt;) characters. 
# remove &, ;, and # so only lt and gt remain
# Also note that Mallika Sherawat is an actual person, but we will ignore 
# the implications of this for this introductory tutorial.
test$text[16]

# A URL.
# lots of ways to treat URLs
# for this course we will split http and the www... web address
# reasoning is that web addresses probably won't reoccur but http will indicate that a web address was communicated and that may be an important feature
train$text[381]

# Tokenize SMS text messages.
# remove numbers, punctuation, symbols and split hyphenated words
train.tokens <- tokens(train$text, what = "word1", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

# Take a look at a specific SMS message and see how it transforms.
train.tokens[[381]]

# Lower case the tokens.
train.tokens <- tokens_tolower(train.tokens)

train.tokens[[381]]

# Use quanteda's built-in stopword list for English.
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
train.tokens[[381]]

# Perform stemming on the tokens.
train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens[[381]]

# Create our first bag-of-words model.
# dfm() takes in tokens and creates a document-frequency matrix (dfm)
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)

# Transform to a matrix and inspect.
train.tokens.matrix <- as.matrix(train.tokens.dfm)
View(train.tokens.matrix[1:20, 1:100])
dim(train.tokens.matrix)

# Investigate the effects of stemming.
colnames(train.tokens.matrix)[1:50]

# Per best practices, we will leverage cross validation (CV) as
# the basis of our modeling process. Using CV we can create 
# estimates of how well our model will do in Production on new,
# unseen data. CV is powerful, but the downside is that it
# requires more processing and therefore more time.
#
# If you are not familiar with CV, consult the following 
# Wikipedia article:
#
#   https://en.wikipedia.org/wiki/Cross-validation_(statistics)
#

# Setup a feature data frame with labels.
train.tokens.df <- bind_cols(label = train$label, convert(train.tokens.dfm, to = "data.frame"))
View(train.tokens.df)
# Often, tokenization requires some additional pre-processing
# col names are the words in texts. Some of the column names will
# create problems later so we need to clean them up. Here are examples
# of problematic names
names(train.tokens.df)[c(139, 141, 211, 213)]

# Cleanup column names. Will modify only names that are not syntactically valid
names(train.tokens.df) <- make.names(names(train.tokens.df))

# check that each column name only occurs once
data.frame(name = names(train.tokens.df)) %>% 
  group_by(name) %>% 
  mutate(n = n()) %>% 
  filter(n != 1)

# Use caret to create stratified folds for 10-fold cross validation repeated 
# 3 times (i.e., create 30 random stratified samples)
# we are using stratified cross validation because we have a class imbalance 
# in the data (spam < prevalent ham) each random sample taken is representative 
# in terms of the proportion of spam and ham in the dataset
# why are we repeating the cross validation 3 times? If we take the time to conduct 
# cross validation multiple times we should get more valid estimates. If we take more 
# looks at the data the estimation process will be more robust.

set.seed(48743)
cv.folds <- createMultiFolds(train$label, k = 10, times = 3)

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
#

# fit a single tree model
if (file.exists("data/rpart.cv.1.RData")) {
  
  # load single tree model
  load("data/rpart.cv.1.RData")
  
} else {
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on 7 logical cores.
  # check how many cores your machine has available for parallel processing
  # keep 1 core free for the operating system
  # parallel::detectCores()
  cl <- makeCluster(7, type = "SOCK") # effectively creates multiple instances of R studio and uses them all at once to process the model
  registerDoSNOW(cl) # need to tell R to process in parallel
  
  # As our data is non-trivial in size at this point, use a single decision
  # tree alogrithm (rpart trees) as our first model. We will graduate to using more 
  # powerful algorithms later when we perform feature extraction to shrink
  # the size of our data.
  # e.g., could use fandom forest (rf) or XGBoost (xgbTree) instead by changing the method
  # formula means predict (~) label using all other variables (.) in dataset
  # trControl is the cross validation parameters
  # tuneLength allows you to set the number of different configurations of the algorithm to test
  # it selects the one best one and builds a model using that configuration
  rpart.cv.1 <- train(label ~ ., data = train.tokens.df, 
                      method = "rpart", 
                      trControl = cv.cntrl, 
                      tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rpart.cv.1, file = "data/rpart.cv.1.RData")
  
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

# The use of Term Frequency-Inverse Document Frequency (TF-IDF) is a 
# powerful technique for enhancing the information/signal contained
# within our document-frequency matrix. In most instances TF-IDF will
# enhance the performance of the model. Typically this would be done
# immediately after stemming (pre-processing).
# Specifically, the mathematics
# behind TF-IDF accomplish the following goals:
#    1 - The TF calculation accounts for the fact that longer 
#        documents will have higher individual term counts. Applying
#        TF normalizes all documents in the corpus to be length 
#        independent.
#    2 - The IDF calculation accounts for the frequency of term
#        appearance in all documents in the corpus. The intuition 
#        being that a term that appears in every document has no
#        predictive power. Applies penalty to terms that appear more frequently
#    3 - The multiplication of TF by IDF for each cell in the matrix
#        allows for weighting of #1 and #2 for each cell in the matrix.


# Our function for calculating relative term frequency (TF)
# TF = freq of a single term in a document (text) / sum of all freq of all terms in a document (text) 
# each row represents a term and column represents a document (text) 
# TF is document focused (rows)
term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
# log10 (total number of documents / the number of documents that a specific term is present in)
# IDF is corpus focused (columns) 
inverse.doc.freq <- function(col) {
  corpus.size <- length(col) # calculate for each column how many documents there are (should be the same for every column)
  doc.count <- length(which(col > 0)) # calculate the number of rows where the column is not 0
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
# will give the same output as quanteda's tf-idf function if you set normalisation = TRUE
# calculating it manually has the benefit that you can use the components (TF or IDF) in
# later feature engineering/analysis
# TF-IDF = TF * IDF
tf.idf <- function(tf, idf) {
  tf * idf
}


# First step, normalize all documents via TF.
# apply term frequency (TF) function to the rows (1) of train.tokens.matrix
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
dim(train.tokens.df) # matrix has been transposed by apply
View(train.tokens.df[1:20, 1:100])

# Second step, calculate the IDF vector that we will use - both
# for training data and for test data!
# apply inverse document frequency (IDF) function to the columns (2) of train.tokens.matrix
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)
str(train.tokens.idf)

# Lastly, calculate TF-IDF for our training corpus.
# apply TF-IDF function to the columns (2) of train.tokens.df (normalised matrix)
train.tokens.tfidf <-  apply(train.tokens.df, 2, tf.idf, idf = train.tokens.idf)
dim(train.tokens.tfidf) # transposed matrix has been maintained
View(train.tokens.tfidf[1:25, 1:25])

# Transpose the matrix in preparation of training the ML model
train.tokens.tfidf <- t(train.tokens.tfidf)
dim(train.tokens.tfidf)
View(train.tokens.tfidf[1:25, 1:25]) # higher values represent terms that appear less frequently

# When we compute TF-IDF we need to check for incomplete cases 
# after preprocessing (e.g., remove stop words, stemming) some documents could be empty
# Check for incomplete cases.
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train$text[incomplete.cases] # show original text before pre-processing - all contain stop words and punctuation only

# Fix incomplete cases
# for any document (row) that is now empty as a result of pre-processing (e.g., stemming) replace with zeros
# do not want to remove the records because there could be a signal in this data
# zero represents docs made up of stop words only
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))
dim(train.tokens.tfidf)
sum(which(!complete.cases(train.tokens.tfidf)))

# Make a clean data frame using the same process as before.
train.tokens.tfidf.df <- cbind(y_label = train$label, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))

# check that each column name only occurs once
data.frame(name = names(train.tokens.tfidf.df)) %>% 
  group_by(name) %>% 
  mutate(n = n()) %>% 
  filter(n != 1)


# fit random forest model
if (file.exists("data/rpart.cv.2.RData")) {
  
  # load single tree model
  load("data/rpart.cv.2.RData")
  
} else {
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on 7 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # As our data is non-trivial in size at this point, use a single decision
  # tree alogrithm as our first model. We will graduate to using more 
  # powerful algorithms later when we perform feature extraction to shrink
  # the size of our data.
  rpart.cv.2 <- train(y_label ~ ., data = train.tokens.tfidf.df, method = "rpart", 
                      trControl = cv.cntrl, tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rpart.cv.2, file = "data/rpart.cv.2.RData")
  
  # Total time of execution on workstation was 
  total.time <- Sys.time() - start.time
  total.time
}

# Check out our results.
rpart.cv.2


# N-grams allow us to augment our document-term frequency matrices with
# word ordering. This often leads to increased performance (e.g., accuracy)
# for machine learning models trained with more than just unigrams (i.e.,
# single terms). Let's add bigrams (each unique two-term phrase) to our training data and the TF-IDF 
# transform the expanded featre matrix to see if accuracy improves. The data will contain unigram and bigram features.
# as you add higher n-grams the lower the likelihood of those phrases being shared across multiple documents -
# will mostly contain zeros (sparsity problem - curse of dimensionality problem)

# Add bigrams to our feature matrix.
train.tokens <- tokens_ngrams(ç, n = 1:2) # 1:2 requests unigrams and bigrams
train.tokens[[381]]

# Transform to dfm and then a matrix.
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)
train.tokens.matrix <- as.matrix(train.tokens.dfm)
train.tokens.dfm

# Normalize all documents via TF.
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)

# Calculate the IDF vector that we will use for training and test data!
train.tokens.idf <- apply(train.tokens.matrix, 2, inverse.doc.freq)

# Calculate TF-IDF for our training corpus 
train.tokens.tfidf <- apply(train.tokens.df, 2, tf.idf, 
                            idf = train.tokens.idf)

# Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)

# Fix incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))

# Make a clean data frame.
train.tokens.tfidf.df <- cbind(y_labels = train$label, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))

# check that each column name only occurs once
data.frame(name = names(train.tokens.tfidf.df)) %>% 
  group_by(name) %>% 
  mutate(n = n()) %>% 
  filter(n != 1)

# Clean up unused objects in memory.
gc()


# NOTE - The following code requires the use of command-line R to execute
#        due to the large number of features (i.e., columns) in the matrix.
#        Please consult the following link for more details if you wish
#        to run the code yourself:
#
#        https://stackoverflow.com/questions/28728774/how-to-set-max-ppsize-in-r
#
#        Also note that running the following code required approximately
#        38GB of RAM and more than 4.5 hours to execute on a 10-core 
#        workstation!
#

# Time the code execution
# start.time <- Sys.time()

# Leverage single decision trees to evaluate if adding bigrams improves the 
# the effectiveness of the model.
# rpart.cv.3 <- train(y_labels ~ ., data = train.tokens.tfidf.df, method = "rpart", 
#                     trControl = cv.cntrl, tuneLength = 7)

# Total time of execution on workstation was
# total.time <- Sys.time() - start.time
# total.time

# Check out our results.
# rpart.cv.3

#
# The results of the above processing show a slight decline in rpart 
# effectiveness with a 10-fold CV repeated 3 times accuracy of 0.9457.
# As we will discuss later, while the addition of bigrams appears to 
# negatively impact a single decision tree, it helps with the mighty
# random forest!
#

# one way around the above mentioned problems (sparsity, computing power, and 
# length of time to run analysis) is to conduct latent semantic analysis (LSA)
# using a technique called singular value decomposition which is a matrix 
# factorisation technique that allows for the implementation of feature reduction/extraction
# this process will reduce the number of features and increase the representation 
# of the data (creates a smaller and more feature rich matrix - better for random forest)

# Our Progress So Far
# ▪ We’ve made a lot of progress:
#   • Representing unstructured text data in a format amenable to analytics and machine learning.
#   • Building a standard text analytics data pre-processing pipeline.
#   • Improving the bag-of-words model (BOW) with the use of the mighty TF-IDF.
#   • Extending BOW to incorporate word ordering via n-grams.

# ▪ However, we’ve encountered some notable problems as well:
#   • Document-term matrices explode to be very wide (i.e., lots of columns).
#   • The features of document-term matrices don’t contain a lot of signal (i.e., they’re sparse).
#   • We’re running into scalability issues like RAM and huge amounts of computation.
#   • The curse of dimensionality.

# ▪ The vector space model helps address many of the problems above!

# Latent Semantic Analysis
# Intuition – Extract relationships between the documents and terms assuming that terms that are close 
# in meaning will appear in similar (i.e., correlated) pieces of text.

# Implementation – LSA leverages a singular value decomposition (SVD) factorization of a term-document 
# matrix to extract these relationships.

# Latent Semantic Analysis (LSA) often remediates the curse of dimensionality problem in text analytics:
#   • The matrix factorization has the effect of combining columns, potentially enriching signal in the data.
#   • By selecting a fraction of the most important singular values, LSA can dramatically reduce dimensionality.

# However, there’s no free lunch:
#   • Performing the SVD factorization is computationally intensive.
#   • The reduced factorized matrices (i.e., the “semantic spaces”) are approximations.
#   • We will need to project new data into the semantic space.

# High-level steps so far:
#   • Create tokens (e.g., lowercase, remove stop words).
#   • Normalize the document vector (i.e., row) using the term.frequency() function.
#   • Complete the TF-IDF projection using the tf.idf() function.
#   • Apply the SVD projection on the document vector.

# We'll leverage the irlba package for our singular value 
# decomposition (SVD). The irlba package allows us to specify
# the number of the most important singular vectors we wish to
# calculate and retain for features.

# NOTE: it looks like it is possible to use PCA instead of SVD
# need to look into this. A benefit of using PCA is that the latent components may be interpretable

{
  # Time the code execution
  start.time <- Sys.time()
  
  # Perform SVD. Specifically, reduce dimensionality down to 300 columns
  # for our latent semantic analysis (LSA).
  # Note: could potentially use PCA instead - this may lead to interpretable latent components
  # SVD/LSA assumes that in the matrix the rows are the terms and the columns are the documents
  # our matrix is the other way around so we need to transpose t() our matrix to a term-document matrix
  # which is necessary to run the analysis
  # nv extracts features from the document (right singular vectors) - 
  # give me the vector representation of the higher level concepts extracted on a per document basis
  # nu extracts features from the terms (left singular vectors) we want nu = nv (same number as nv)
  # research shows that vectors in the few hundred range tend to offer the best results
  # you should spend some time tuning your model to work out the best number of vectors
  # e.g., test 200, 300, 400, 500 etc...
  # maxit is the number of iterations. typically a mxit value double the nv value works well in 
  # extracting the number of desired features
  train.irlba <- irlba(t(train.tokens.tfidf), nv = 300, maxit = 600)
  
  # Total time of execution on workstation was 
  total.time <- Sys.time() - start.time
  total.time
}

# Take a look at the new feature data up close.
# 𝑆𝑉𝐷 𝑜𝑓 𝑋 = 𝑋 = 𝑈∑𝑉𝑇
# U contains the eigenvectors of the term correlations (left singular vector) 𝑋𝑋t()
# V contains the eigenvectors of the document correlations (right singular vector) 𝑋t()𝑋
# sigma (sum of symbol) contains the singular values of the factorization - denoted by d in the output
# lets take a look at the V matrix
# rows = documents
# columns = extracted features
# this is a black box process we do not know what the columns mean
View(train.irlba$v)


# As with TF-IDF, we will need to project new data (e.g., the test data)
# into the SVD semantic space. The following code illustrates how to do
# this using a row of the training data that has already been transformed
# by TF-IDF, per the mathematics illustrated in the slides.
# the SVD projection for document d is:  𝑑hat = ∑−1 𝑈t() 𝑑
# lets calculate document hat (d hat) to check against our training data
# the below formula is essentially what has been done to our training data
sigma.inverse <- 1 / train.irlba$d # = ∑−1 
u.transpose <- t(train.irlba$u) # 1 𝑈t()
document <- train.tokens.tfidf[1,] # 𝑑  - we will just take the first document (text 1)
document.hat <- sigma.inverse * u.transpose %*% document #   𝑑hat

# Look at the first 10 components of projected document and the corresponding
# row in our document semantic space (i.e., the V matrix)
document.hat[1:10]
train.irlba$v[1, 1:10] # there will likely be some minor differences in the values

# Create new feature data frame using our document semantic space of 300
# features (i.e., the V matrix from our SVD).
# we are adding the labels (ham or spam) to the new training data (extracted document features)
train.svd <- data.frame(y_labels = train$label, train.irlba$v)

# fit single tree model to svd data
if (file.exists("data/rpart.cv.4.RData")) {
  
  # load single tree model
  load("data/rpart.cv.4.RData")
  
} else {
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on 7 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # This will be the last run using single decision trees. With a much smaller
  # feature matrix we can now use more powerful methods like the mighty Random
  # Forest from now on!
  rpart.cv.4 <- train(y_labels ~ ., data = train.svd, method = "rpart", 
                      trControl = cv.cntrl, tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rpart.cv.4, file = "data/rpart.cv.4.RData")
  
  # Total time of execution on workstation was 
  total.time <- Sys.time() - start.time
  total.time
}

# Check out our results.
# when using a single decision tree (rpart) we have slightly worse performance by adding bigrams and SVD
# this is because some of the signal has been lost
# good news is that when we use random forest we will gain performance by adding bigrams and SVD
rpart.cv.4


# let's try PCA instead of SVD
library(psych)
source("R/pca_functions.R")

# select original comms variables
pca <- train.tokens.tfidf.df %>%
  select(-y_labels)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
# KMO(cor(pca, use = "pairwise.complete.obs"))

# Bartlett's test of spherecity
# print("Bartletts test of spherecity")
# print(data.frame(cortest.bartlett(cor(pca, use = "pairwise.complete.obs"), n = 3901)))

# scree plot
# scree(pca, factors = TRUE)
# 
# # conduct parallel analysis to confirm no. components
# paran::paran(pca, iterations = 0, all = T, graph = T)

# 2-component PCA
n_comp <- 300
rotate_method <- "promax" # rotation with kaiser normalization
score_method <- "Bartlett"

fit <- principal(pca, rotate = rotate_method, nfactors = n_comp,
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()

# Create new feature data frame using our document semantic space of 300
# features (i.e., the V matrix from our SVD).
# we are adding the labels (ham or spam) to the new training data (extracted document features)
train.pca <- bind_cols(data.frame(y_labels = factor(train.tokens.tfidf.df$y_labels)), data.frame(fit$scores))

# fit single tree model to svd data
if (file.exists("data/rpart.cv.5.RData")) {
  
  # load single tree model
  load("data/rpart.cv.5.RData")
  
} else {
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Create a cluster to work on 7 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # This will be the last run using single decision trees. With a much smaller
  # feature matrix we can now use more powerful methods like the mighty Random
  # Forest from now on!
  rpart.cv.5 <- train(y_labels ~ ., data = train.pca, method = "rpart", 
                      trControl = cv.cntrl, tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rpart.cv.5, file = "data/rpart.cv.5.RData")
  
  # Total time of execution on workstation was 
  total.time <- Sys.time() - start.time
  total.time
}

# Check out our results.
# when using a single decision tree (rpart) we have slightly worse performance by adding bigrams and SVD
# this is because some of the signal has been lost
# good news is that when we use random forest we will gain performance by adding bigrams and SVD
rpart.cv.5


# NOTE - The following code takes a long time to run. Here's the math.
#        We are performing 10-fold CV repeated 3 times. That means we
#        need to build 30 models. We are also asking caret to try 7 
#        different values of the mtry parameter. Next up by default
#        a mighty random forest leverages 500 trees. Lastly, caret will
#        build 1 final model at the end of the process with the best 
#        mtry value over all the training data. Here's the number of 
#        tree we're building:
#
#             (10 * 3 * 7 * 500) + 500 = 105,500 trees!
#
# On a workstation using 10 cores the following code took 28 minutes 
# to execute.
#

# fit single tree model to svd data
if (file.exists("data/rf.cv.1.RData")) {
  
  # load model
  load("data/rf.cv.1.RData")
  
} else {
  
  # Create a cluster to work on 7 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # Time the code execution
  start.time <- Sys.time()
  
  # We have reduced the dimensionality of our data using SVD. Also, the
  # application of SVD allows us to use LSA to simultaneously increase the
  # information density of each feature. To prove this out, leverage a
  # mighty Random Forest with the default of 500 trees. We'll also ask
  # caret to try 7 different values of mtry to find the mtry value that
  # gives the best result!
  rf.cv.1 <- train(y_labels ~ ., data = train.svd, method = "rf",
                   trControl = cv.cntrl, tuneLength = 7)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rf.cv.1, file = "data/rf.cv.1.RData")
  
  # Total time of execution on workstation was
  total.time <- Sys.time() - start.time
  total.time
  
}

# Check out our results.
# mtry is a parameter that controls how much data gets used when building individual trees 
# in other words mytry constrains the amount of data each tree gets to see
# mytry = 151 means the trees get to see 151 of the 300 features extracted using SVD
rf.cv.1

# Let's drill-down on the results.
# code originally sourced labels from train.svd$label
# but this created errors for some reason the ordering is different
# notice that accuracy is a little higher than our best model
# this is because 10-fold cv is using 90% of the training data
# 9 folds are used for testing and 1 fold is used for testing
confusionMatrix(rf.cv.1$finalModel$y, rf.cv.1$finalModel$predicted)


# accuracy is not the only performance metric to evaluate the model
# true positive (tp) true negative (tn)
# false positive (fp) false negative (fp)
# what percentage of ham and spam were correctly predicted?
# accuracy = tp + tn / tp + fp + fn + tn (nrow) 
# what percentage of ham messages were correctly predicted?
# sensitivity = tp / tp + fn 
# what percentage of spam messages were correctly predicted?
# specificity = tn / fp + tn 
# true positive rate
# Pos Pred Value = tp / tp + fp (total observed ham)
# true negative rate
# Neg Pred Value = tn / tn + fn (total observed spam)

# when a feature increases both sensitivity and specificity it suggests that the
# feature may generalise to other datasets

# OK, now let's add in the feature we engineered previously for SMS 
# text length to see if it improves things.
# texts may be in different order in train.svd than train
# if I run this code will need to check this
train.svd <- bind_cols(train.svd, train %>% select(text_length))

if (file.exists("data/rf.cv.2.RData")) {
  
  # load model
  load("data/rf.cv.2.RData")
  
} else {
  
  # Create a cluster to work on 7 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Re-run the training process with the additional feature.
  # importance = TRUE asks the rf model to keep track of feature importance
  rf.cv.2 <- train(y_labels ~ ., data = train.svd, method = "rf",
                   trControl = cv.cntrl, tuneLength = 7,
                   importance = TRUE)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rf.cv.2, file = "data/rf.cv.2.RData")
  
  # Total time of execution on workstation was
  total.time <- Sys.time() - start.time
  total.time
  
}

# Check the results.
rf.cv.2

# Drill-down on the results.
confusionMatrix(rf.cv.2$finalModel$y, rf.cv.2$finalModel$predicted)

# How important was the new feature?
# higher values on x-axis better for mean decrease gini
varImpPlot(rf.cv.1$finalModel)
varImpPlot(rf.cv.2$finalModel)

# Turns out that our TextLength feature is very predictive and pushed our
# overall accuracy over the training data to 97.1%. We can also use the
# power of cosine similarity to engineer a feature for calculating, on 
# average, how alike each SMS text message is to all of the spam messages.
# The hypothesis here is that our use of bigrams, tf-idf, and LSA have 
# produced a representation where ham SMS messages should have low cosine
# similarities with spam SMS messages and vice versa.

# the angle between two docs is represented by theta. 
# cosine(theta) is the similarity between two docs
# using the cosine between docs is better than using the dot product (correlation proxy)
# for a few reasons:
#   • values range between 0,1 (1 = perfect similarity and 0 = orthogonal (right angle))
#   • works well in high dimensional space

# Use the lsa package's cosine function for our calculations.
# need to transpose as it expects documents to be on the columns
# also need to remove first and last columns (label and text_length)
# will produce a 3901*3901 matrix
train.similarities <- cosine(t(as.matrix(train.svd[, -c(1, ncol(train.svd))])))
dim(train.similarities)

# Next up - take each SMS text message and find what the mean cosine similarity
# is for each SMS text mean with each of the spam SMS messages.
# Per our hypothesis, ham SMS text messages should have relatively low
# cosine similarities with spam messages and vice versa!
# give me the indices of all spam messages
spam.indexes <- which(train$label == "spam")

train.svd <- train.svd %>% mutate(spam_similarity = 0)

# calcualte the mean cosine similarity between each doc (text) and all of the spam messages
# essentially, on average, how similar is each text to all of the spam messages?
# could implement this is in a different way - using nicer code
# 
# Giedrius Blazys commenting on video 11 pointed out (unconfirmed by data science dojo):
# when creating cosine similarities with spam message feature on training data 
# you should exclude the observation itself from the spam messages list.
# This solves the data leakage problem leading to over-fitting. The RF results
# on test data with updated feature are much better:
for(i in 1:nrow(train.svd)) {
  spam.indexesCV <- setdiff(spam.indexes,i)
  train.svd$spam_similarity[i] <- mean(train.similarities[i, spam.indexesCV])
}

# for(i in 1:nrow(train.svd)) {
#   train.svd$spam_similarity[i] <- mean(train.similarities[i, spam.indexes])  
# }

# As always, let's visualize our results
ggplot(train.svd, aes(x = spam_similarity, fill = y_labels)) +
  theme_bw() +
  geom_histogram(binwidth = 0.05) +
  labs(y = "Message Count",
       x = "Mean Spam Message Cosine Similarity",
       title = "Distribution of Ham vs. Spam Using Spam Cosine Similarity")

# Per our analysis of mighty random forest results, we are interested in 
# in features that can raise model performance with respect to sensitivity.
# Perform another CV process using the new spam cosine similarity feature.

if (file.exists("data/rf.cv.3.RData")) {
  
  # load model
  rf.cv.3 <- load("data/rf.cv.3.RData")
  
} else {
  
  # Create a cluster to work on 3 logical cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # Time the code execution
  start.time <- Sys.time()
  
  # Re-run the training process with the additional feature.
  rf.cv.3 <- train(y_labels ~ ., data = train.svd, method = "rf",
                   trControl = cv.cntrl, tuneLength = 7,
                   importance = TRUE)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rf.cv.3, file = "data/rf.cv.3.RData")
  
  # Total time of execution on workstation was
  total.time <- Sys.time() - start.time
  total.time
  
  
}

# Check the results.
rf.cv.3

# Drill-down on the results.
confusionMatrix(rf.cv.3$finalModel$y, rf.cv.3$finalModel$predicted)

# How important was this feature?
# spam_similarity is massively more important than any other features
# it also reduced specificity (worse at predicting spam) and increased sensitivity (better at predicting ham)
# should create some skepticism as both metrics did not increase - feature may cause overfitting which we can check for
varImpPlot(rf.cv.3$finalModel)


# We've built what appears to be an effective predictive model. Time to verify
# using the test holdout data we set aside at the beginning of the project.
# First stage of this verification is running the test data through our pre-
# processing pipeline of:
#      1 - Tokenization
#      2 - Lower casing
#      3 - Stopword removal
#      4 - Stemming
#      5 - Adding bigrams
#      6 - Transform to dfm
#      7 - Ensure test dfm has same features as train dfm

# Tokenization.
test.tokens <- tokens(test$text, what = "word1", 
                      remove_numbers = TRUE, remove_punct = TRUE,
                      remove_symbols = TRUE, split_hyphens = TRUE)

# Lower case the tokens.
test.tokens <- tokens_tolower(test.tokens)

# Stopword removal.
test.tokens <- tokens_select(test.tokens, stopwords(), 
                             selection = "remove")

# Stemming.
test.tokens <- tokens_wordstem(test.tokens, language = "english")

# Add bigrams.
test.tokens <- tokens_ngrams(test.tokens, n = 1:2)

# Convert n-grams to quanteda document-term frequency matrix.
test.tokens.dfm <- dfm(test.tokens, tolower = FALSE)

# Explore the train and test quanteda dfm objects.
# notice in our test set we have less than half the data thus we have less than half the features
# the model is expecting the number of features contained in the training data
# it won't understand anything less than that (you can have more but not less)
# it is very common to see new words in the test set that are not present in the training set
# we will need to fix this later by "creating" new test data the looks like the training data
# and stripping out any unique terms that are only in the test data
# columns need to be in the same order and need to have the same meaning
train.tokens.dfm
test.tokens.dfm

# Ensure the test dfm has the same n-grams as the training dfm and the 
# columns are in the same location across training and test data
#
# NOTE - In production we should expect that new text messages will 
#        contain n-grams that did not exist in the original training
#        data. As such, we need to strip those n-grams out.
test.tokens.dfm <- dfm_match(test.tokens.dfm, train.tokens.dfm@Dimnames$features)

test.tokens.matrix <- as.matrix(test.tokens.dfm)
test.tokens.dfm # now we have the same structure as the training set

# With the raw test features in place next up is the projecting the term
# counts for the unigrams into the same TF-IDF vector space as our training
# data. The high level process is as follows:
#      1 - Normalize each document (i.e, each row)
#      2 - Perform IDF multiplication using training IDF values

# Normalize all documents via TF.
# the original idf values from the training set need to be stored so they can be reused here in the test data
test.tokens.df <- apply(test.tokens.matrix, 1, term.frequency)
str(test.tokens.df)

# Lastly, calculate TF-IDF for our training corpus.
# notice we are reusing the idf values from the training data
# this is very important because new texts need to be incorporated to the vector space 
# (need to be able to maintain them over time in a production system)
test.tokens.tfidf <-  apply(test.tokens.df, 2, tf.idf, idf = train.tokens.idf)
dim(test.tokens.tfidf)
View(test.tokens.tfidf[1:25, 1:25])

# Transpose the matrix
test.tokens.tfidf <- t(test.tokens.tfidf)

# Fix incomplete cases
summary(test.tokens.tfidf[1,])
test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
summary(test.tokens.tfidf[1,])

# With the test data projected into the TF-IDF vector space of the training
# data we can now do the final projection into the training LSA semantic
# space (i.e. the SVD matrix factorization).
test.svd.raw <- t(sigma.inverse * u.transpose %*% t(test.tokens.tfidf))

# Lastly, we can now build the test data frame to feed into our trained
# machine learning model for predictions. First up, add Label and TextLength.
test.svd <- data.frame(y_labels = test$label, test.svd.raw, 
                       text_length = test$text_length)

# Next step, calculate spam_similarity for all the test documents. First up, 
# create a spam similarity matrix.
# take the raw svd features and add rows (docs) that correspond to spam from the training data
# need this to calculate the similarity between test texts and the spam texts from the training data
test.similarities <- rbind(test.svd.raw, train.irlba$v[spam.indexes,])
test.similarities <- cosine(t(test.similarities))

# NOTE - The following code was updated post-video recoding due to a bug.
test.svd$spam_similarity <- rep(0.0, nrow(test.svd))
spam.cols <- (nrow(test.svd) + 1):ncol(test.similarities)
for(i in 1:nrow(test.svd)) {
  # The following line has the bug fix.
  test.svd$spam_similarity[i] <- mean(test.similarities[i, spam.cols])  
}

# Some SMS text messages become empty as a result of stopword and special 
# character removal. This results in spam similarity measures of 0. Correct.
# This code as added post-video as part of the bug fix.
test.svd$spam_similarity[!is.finite(test.svd$spam_similarity)] <- 0

# Now we can make predictions on the test data set using our trained mighty 
# random forest.
preds <- predict(rf.cv.3, test.svd)

# Drill-in on results
# common definition of overfitting is doing much worse on test data than training data
confusionMatrix(preds, factor(test.svd$y_labels))


# The definition of overfitting is doing far better on the training data as
# evidenced by CV than doing on a hold-out dataset (i.e., our test dataset).
# this doesn't appear to be the case here as performance on the test data is
# similar to the training data

# out of curiosity let's run the model again without the cosine similarity 
# feature to compare models

train.svd$SpamSimilarity <- NULL
test.svd$SpamSimilarity <- NULL


# fit random forest without the spam similarity feature
if (file.exists("data/rf.cv.4.RData")) {
  
  # load model
  load("data/rf.cv.4.RData")

} else {
  
  # Create a cluster to work on 7 cores.
  cl <- makeCluster(7, type = "SOCK")
  registerDoSNOW(cl)
  
  # # Time the code execution
  start.time <- Sys.time()
  
  # Re-run the training process with the additional feature.
  set.seed(254812)
  rf.cv.4 <- train(y_labels ~ ., data = train.svd, method = "rf",
                   trControl = cv.cntrl, tuneLength = 7,
                   importance = TRUE)
  
  # Processing is done, stop cluster.
  stopCluster(cl)
  
  # save model for quick loading next time
  save(rf.cv.4, file = "data/rf.cv.4.RData")
  
  # Total time of execution on workstation was
  total.time <- Sys.time() - start.time
  total.time
  
}


# Make predictions and drill-in on the results
# results slightly better without cosine similarity
preds <- predict(rf.cv.4, test.svd)
confusionMatrix(preds, factor(test.svd$y_labels))


# What next?
#   ▪ Feature Engineering:
#   • How about tri-grams, 4-grams, etc.?
#   • We engineered TextLength – are there other features as well?
#   ▪ Algorithms - we leveraged a mighty random forest, but could other algorithms do more with the data?
#   • Boosted decision trees (XGBoost)?
#   • Support vector machines (SVM)? They are very commonly used in text analytics and tend to work ok in high dimensional space
#     could allow for the analysis of term columns (pre-SVD)
#   ▪ Learn more ways to analyze, understand, and work with text!

# Start Here – The Basics
# Text analysis with R for students of literature
# ▪ Best introduction to thinking analytically about text.
# ▪ Accessible to a very broad audience. 
# ▪ Illustrated many techniques not in series (e.g., topic modeling).

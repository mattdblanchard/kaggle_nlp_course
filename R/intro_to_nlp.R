packages <- c("tidyverse", "quanteda")

# install any packages not currently installed
if (any(!packages %in% installed.packages())) {
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
}

# load packages
lapply(packages, library, character.only = TRUE)

# create character string
x <- c("Tea is healthy and calming, don't you think?")

# tokenize character string (x)
tokens <- tokens(x, what = "word1")


# part 1 ------------------------------------------------------------------

# text preprocessing ------------------------------------------------------
# next need to stem/lemmatize words - this reduces each word to its base
# e.g. calming becomes calm
train_tokens <- tokens_wordstem(tokens, language = "english")

# to remove punctuation, symbols, and numbers
train_tokens <- tokens(train_tokens, what = "word1", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

# when analysing text need often you will remove stop words ("the", "is" etc..)
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
# check stopwords() to make sure important domain specific words are not removed
train_tokens <- tokens_select(train_tokens, stopwords(), 
                              selection = "remove")

# stemming/lemmatizing and dropping stopwords might result in your models performing worse
# so treat this preprocessing as part of your hyperparameter optimization process


# pattern matching --------------------------------------------------------
# select target patterns
target <- phrase(c('Galaxy Note', 'iPhone 11', 'iPhone XS', 'Google Pixel'))

# create text doc of phone reviews
text_doc <- c("Glowing review overall, and some really interesting side-by-side ",
               "photography tests pitting the iPhone 11 Pro against the ",
               "Galaxy Note 10 Plus and last year’s iPhone XS and Google Pixel 3.") 

# identify the occurrence of each target pattern in the phone reviews
match <- data.frame(kwic(text_doc, pat1, valuetype = "glob"))


# exercise ----------------------------------------------------------------
# could load data from kaggle URL
# library(httr)
# library(jsonlite)
# url <- "https://storage.googleapis.com/kagglesdsdata/datasets/362178/763778/restaurant.json?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20210428%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20210428T000547Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=2196ef69eba0b202dd8ff55753f17ab1aecb5557e8d99610435341ad3c4ba959ba7911fb24e860d6c45b7d0b123bedfd7bc28a4bb102b038cf123e3d3d2e05b9274d5aa2de6c71b2e345271069883fcd0e17c15d7c31bcb5132e84ff2e213b7fdbca1e3881d6ecaef33362bdaf24e144355ccc07d2170dff790e4725b2d83853826f68649634805af60b4d6828459c86f06a8caa68213f121cab77d8d3db6d2ab06f3fab4e8191685f79efcc17310e980ec763170d5bbed8ce7a27292974d427771e68ccd063fbe723d49d64571b6475f50bf4c443cbe6d10777d106d44b7db50f51d3ee7945db311120042f45dfbb55c9738f96a505e3733243281749ea09de"
# resp <- GET(url)
# http_error(resp)
# parsed_resp <- content(resp, as="parsed")

# load yelp review data
d <- read_csv("data/yelp_ratings.csv")

# tokenize character string (text)
tokens <- tokens(d$text, what = "word1")

# text preprocessing ------------------------------------------------------
# stem/lemmatize words - this reduces each word to its base
# e.g. calming becomes calm
train_tokens <- tokens_wordstem(tokens, language = "english")

# remove punctuation, symbols, and numbers
train_tokens <- tokens(train_tokens, what = "word1", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

# remove stop words ("the", "is" etc..)
# stopwords()
train_tokens <- tokens_select(train_tokens, stopwords(), 
                              selection = "remove")

# menu items
target <- c("Cheese Steak", "Cheesesteak", "Steak and Cheese", "Italian Combo", "Tiramisu", "Cannoli",
          "Chicken Salad", "Chicken Spinach Salad", "Meatball", "Pizza", "Pizzas", "Spaghetti",
          "Bruchetta", "Eggplant", "Italian Beef", "Purista", "Pasta", "Calzones",  "Calzone",
          "Italian Sausage", "Chicken Cutlet", "Chicken Parm", "Chicken Parmesan", "Gnocchi",
          "Chicken Pesto", "Turkey Sandwich", "Turkey Breast", "Ziti", "Portobello", "Reuben",
          "Mozzarella Caprese",  "Corned Beef", "Garlic Bread", "Pastrami", "Roast Beef",
          "Tuna Salad", "Lasagna", "Artichoke Salad", "Fettuccini Alfredo", "Chicken Parmigiana",
          "Grilled Veggie", "Grilled Veggies", "Grilled Vegetable", "Mac and Cheese", "Macaroni",  
          "Prosciutto", "Salami")

# identify the occurrence of each target pattern in the phone reviews
match <- data.frame(kwic(train_tokens, target, valuetype = "glob"))

# create index of rows that contain a review with a target word
index <- match %>% 
  mutate(text_num = str_remove(docname, "text")) %>% 
  select(keyword, text_num) %>% 
  unique()

# select reviews with target words
select_reviews <- d[as.numeric(index$text_num), ]

# create df of keywords to add to select_reviews
key <- match %>% 
  select(docname, keyword) %>% 
  unique() %>% 
  mutate(keyword = tolower(keyword))

# add keywords
select_reviews <- select_reviews %>% 
  bind_cols(keyword = key$keyword) 

# compute the mean and sd number of stars and the number of reviews for each keyword 
# important to consider the number of occurrences
scores <- select_reviews %>%   
  group_by(keyword) %>% 
  summarise(mean = mean(stars),
            sd = sd(stars),
            n = n()) %>% 
  arrange(mean)

# top 10 mean ratings
slice_max(scores, mean, n = 10)

# worst 10 mean ratings
slice_min(scores, mean, n = 10)


# part 2 ------------------------------------------------------------------


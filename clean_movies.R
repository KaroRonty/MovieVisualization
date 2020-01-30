library(dplyr)
library(tidyr)
library(readr) # faster import of .tsv files
library(qdapTools) # creating dummy variables from multiple columns

# Read in the datasets
imdb_rating <- read_delim("01_Data/title_ratings.tsv", delim = "\t") %>% 
  as.data.frame()

imdb_basic <- read_delim("01_Data/title_basics.tsv", delim = "\t") %>% 
  as.data.frame() 

movie_revenues <- read_delim("01_Data/movie_revenues.csv", delim = ";") %>% 
  as.data.frame()

# Combine datasets by unique movie identifiers
movies <- imdb_basic %>% 
  left_join(imdb_rating, by = c("tconst" = "tconst")) %>% 
  left_join(movie_revenues, by = c("tconst" = "imdb_id")) 

# Remove redundant columns
movies <- movies %>% 
  select(-originalTitle,
         -isAdult,
         -endYear,
         -runtimeMinutes,
         -id,
         -original_title:-popularity,
         -release_date,
         -spoken_languages:-title,
         -spoken_languages_number)

# Set all column names to lower case
colnames(movies) <- colnames(movies) %>% tolower()

# Rename some column names
movies <- movies %>% 
  rename(imdb_id = tconst,
         title_class = titletype,
         title = primarytitle,
         year = startyear,
         genre_imdb = genres.x,
         avg_rating = averagerating,
         votes = numvotes,
         genre_kaggle = genres.y,
         language = original_language,
         prod_ct = production_countries,
         prod_ct_nr = production_countries_number,
         prod_cp = production_companies,
         prod_cp_nr = production_companies_number)

# Keep only movies with certain charasteristics
movies <- movies %>% 
  filter(title_class %in% c("movie", "short")) %>%
  select(-vote_count, -vote_average)

# Differentiate into genres by seperating the characters
# keeping both, the IMDb (max. 3), and the Kaggle (max. 5)
# categorizations per movie
movies <- movies %>% 
  separate(genre_kaggle, c("g_kaggle_1", "g_kaggle_2", "g_kaggle_3", 
                           "g_kaggle_4", "g_kaggle_5"), 
           sep = "[|]", extra = "drop", remove = FALSE) %>%
  separate(genre_imdb, c("g_imdb_1", "g_imdb_2", "g_imdb_3", 
                         "g_imdb_4", "g_imdb_5"), 
           sep = ",", extra = "drop", remove = FALSE)

# Get unique movie categories
unique_categories <- movies %>% 
  select(g_imdb_1:g_imdb_5) %>% 
  unlist() %>% 
  unique() %>% 
  sort()

# Select three first categories, make dummy variables,
# arrange columns and turn them into factors
movies <- movies %>% 
  select(g_imdb_1:g_imdb_3) %>% 
  t() %>% 
  as.data.frame() %>% 
  mtabulate() %>% 
  mutate_all(as.factor) %>% 
  select(noquote(unique_categories)) %>%
  cbind(movies, .)

# Reset rownames that were affected by creating the dummy variables
rownames(movies) <- 1:nrow(movies)

# Remove columns that were one-hot encoded
movies <- movies %>% 
  select(-genre_imdb:-g_imdb_5,
         -genre_kaggle:-g_kaggle_5)

# Remove movies in the "western" or "news" category
movies <- movies %>% 
  filter(Western == 0, News == 0) %>% 
  select(-Western, -News) 

# Change language column to factor and keep only certain columns for modelling
movie_clean <- movies %>% 
  mutate(language = as.factor(language)) %>% 
  select(year:budget,
         revenue:prod_ct_nr,
         language,
         Action:War)



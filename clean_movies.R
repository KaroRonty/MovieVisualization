library(dplyr)
library(tidyr)
library(readr) # faster import of .tsv files
library(qdapTools) # creating dummy variables from multiple columns

library(rworldmap)
library(devtools)
library(ggplot2)
library(gganimate)
library(naniar)
require(maps)
require(countrycode)
require(ggthemes)
install_github("dgrtwo/gganimate", ref = "26ec501")

library(curl)
library(tweenr)
library(viridis)
library(rgeos)
library(readxl)
library(data.table)

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
  filter(title_class %in% c("movie", "short"),
         year > 2019) %>% 
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

# ----- 
# ALEKS Visualization Part

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 



# get world map
wmap <- getMap(resolution="low")

# small edits
wmap <- spTransform(wmap, CRS("+proj=robin")) # reproject
wmap <-   subset(wmap, !(NAME %in% c("Antar"))) # Remove Antarctica

# get centroids of countries
centroids <- gCentroid(wmap , byid=TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")


# cleaning dataset for world map plotting
movies_map <- 
  movies %>% 
  filter(!prod_ct %in% c("Czechoslovakia", "East Germany", "Kosovo", "Netherlands Antilles", 
                         "none", "Serbia and Montenegro", "Yugoslavia", "Soviet Union", NA))

# mapping iso-conform country names to df
movies_map$country_iso3c <- countrycode(movies_map$prod_ct, 'country.name', 'iso3c', warn = TRUE)

# creating a grouped df
movies_map <- 
  movies_map %>%
  group_by(country_iso3c, year) %>% 
  count() %>% 
  arrange(year)

# joining df on worldmap, moviedata & location (longitude & lattitude)
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, movies_map, by = c("id" = "country_iso3c"))
wmap_df <- left_join(wmap_df, centroids, by = c('id'='country_iso3c')) # centroids

movies_map <- left_join(movies_map, centroids, by = c("country_iso3c"="country_iso3c"))

# plotting
map <- world + 
  geom_point(aes(x = long, y = lat, size = n, 
                 frame = year,
                 cumulative = TRUE),
             data = movies_map, colour = 'purple', alpha = .5)



# based on old gganimate package
o <- ggplot(data=wmap_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=n), color="gray90") +
  geom_text(aes(x = x, y = y, label = round(n)), hjust=0, vjust=0, size = 4.5) +
  scale_fill_viridis(name="Number of Movies", begin = 0, end = 1, limits = c(0,200), na.value="gray99") +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Number of Movies produced per year and country") +
  labs(caption = "Map by Group 3 // BAN422 \nsource: IMDb and OMDb datasets") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.05, size=25)) +
  theme(plot.caption = element_text(hjust = 0, color="gray40", size=15)) +
  coord_cartesian(xlim = c(-11807982, 14807978)) + 
  theme( legend.position = c(.5, .08), 
         legend.direction = "horizontal", 
         legend.title.align = 0,
         legend.key.size = unit(1.3, "cm"),
         legend.title=element_text(size=17), 
         legend.text=element_text(size=13) )

# save gif
gg_animate(o, "output4020_old.gif", title_frame =T, 
           ani.width=1600, ani.height=820, dpi=800, interval = .4)




# Change language column to factor and keep only certain columns for modelling
movie_clean <- movies %>% 
  mutate(language = as.factor(language)) %>% 
  select(year:budget,
         revenue:prod_ct_nr,
         language,
         Action:War)



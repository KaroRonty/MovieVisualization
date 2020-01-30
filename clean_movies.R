library(dplyr)
library(tidyr)
library(readr) # faster import of .tsv files
library(qdapTools) # creating dummy variables from multiple columns

# library(rworldmap)
library(tibble)
library(lubridate)
library(devtools)
library(ggplot2)
library(gganimate)
library(naniar)
require(maps)
require(countrycode)
require(ggthemes)
# install_github("dgrtwo/gganimate", ref = "26ec501")
devtools::install_github('thomasp85/gganimate')
# install.packages("https://github.com/thomasp85/gganimate/releases/tag/v0.1.1")
library(ggmap)

library(curl)
library(tweenr)
library(viridis)
library(rgeos)
library(readxl)
library(data.table)

options(digits = 2)
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
         year < 2019) %>% 
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
  mutate(year = as.integer(year)) %>% 
  group_by(country_iso3c, year) %>% 
  count() %>% # summing up number of movies
  drop_na(year) %>% # to reduce mapping failures while rendering graphs
  arrange(year)

# generate a df for cumulative movie releases
movies_map_cum <- movies_map %>% 
  group_by(country_iso3c) %>% 
  mutate(cum_n = cumsum(n)) %>% # summing up number of movies
  arrange(year)


# creating first frame world map
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

# get centroid coordinates
centroid <- read_delim("01_Data/country_centroids_az8.csv", delim = ",") %>% 
  as.data.frame() %>% 
  select(adm0_a3, Longitude, Latitude)

# joining moviedata & location (longitude & lattitude)
movies_map <- left_join(movies_map, centroid, by = c("country_iso3c"="adm0_a3"))

# plotting

br <- c(1,2,5,10,25,50,100,250,500,1000)

a <- ggplot(data = movies_map) +
  borders("world", colour = "gray90", fill = "gray85") +
  theme_map() +
  geom_jitter(aes(x = Longitude, y = Latitude, size = n, frame = year), 
             colour = "#351C4D", alpha = 0.3) +
  # geom_text(aes(x = Longitude, y = Latitude, label = n), hjust=0, vjust=0, size = 2) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) + 
  labs(title = "Movies released in year: {frame_time}", size = "Nr. of Movies") +
  # coord_cartesian(ylim = c(-50, 90)) +
  transition_time(year) 

b <- ggplot(data = movies_map_cum) +
  borders("world", colour = "gray90", fill = "gray85") +
  theme_map() +
  geom_jitter(aes(x = Longitude, y = Latitude, size = cum_n*5, frame = year), 
              colour = "#351C4D", alpha = 0.3) +
  # geom_text(aes(x = Longitude, y = Latitude, label = n), hjust=0, vjust=0, size = 2) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) + 
  labs(title = "Movies released in year: {frame_time}", size = "Nr. of Movies") +
  # coord_cartesian(ylim = c(-50, 90)) +
  transition_time(year) 

animate(b,
        duration = 30,
        fps = 1, height = 800, width =1600) # every 5 years

animate(a,
        duration = 60,
        fps = 2) # every year


# -----
# Try 1:

p <- movies_map %>% 
  ggplot(aes(x = Longitude, y = Latitude)) +
  geom_map(data = world, 
           map = world,
           aes(long,lat, map_id = region),
           colour = '#7f7f7f', fill = 'white',
           alpha = 0.5) +
  geom_map(data = movies_map, map = world,
           aes(fill = n, map_id = country_iso3c),
           colour="#7f7f7f", size=0.5) +
  # geom_text(data = movies_map, 
  #           aes(x = x, y = y, label = round(n)), size = 50.0) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))+ 
  # scale_fill_viridis(name="Life Expectancy", begin = 0, end = 1, limits = c(0,120), na.value="gray99") +
  scale_fill_gradient(name = "legend", trans = "log", breaks = br, labels = br) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Title", x="", y="") +
  theme_bw() 

animate(p, )

map <- world + 
  geom_point(aes(x = x, y = y, size = n, frame = year, cumulative = T), 
             data = movies_map, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1,6), breaks = c(100, 500, 1000, 30000)) +
  geom_point(aes(x = x, y = y, size = n, # this is the init transparent frame
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_points_ini, alpha = 0) +
  geom_point(aes(x = x, y = y, size = n, # this is the final transparent frames
                 frame = created_at,
                 cumulative = TRUE),
             data = ghost_points_fin, alpha = 0) +
  labs(size = "Number of Movies released")

map + transition_time(movies_map$year) +
  labs(title = "Year: {frame_time}")

gganimate(map)

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



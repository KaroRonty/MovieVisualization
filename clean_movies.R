library(dplyr)
library(tidyr)
library(readr) # faster import of .tsv files
library(qdapTools) # creating dummy variables from multiple columns

# library(rworldmap)
library(tibble)
library(ddpcr)
# library(lubridate)
library(devtools)
library(ggplot2)
library(naniar)
require(maps)
require(countrycode)
require(ggthemes)
# install_github("dgrtwo/gganimate", ref = "26ec501")
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
# install.packages("https://github.com/thomasp85/gganimate/releases/tag/v0.1.1")
library(ggmap)
library(ggrepel)
library(patchwork)

# library(curl)
# library(tweenr)
# library(viridis)
# library(rgeos)
# library(readxl)
# library(data.table)

# options(digits = 2)
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

rm(movie_revenues, imdb_basic, imdb_rating)

# Remove redundant columns (removing data from other sources than IMDb)
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
         year <= 2017,
         year >= 1885) %>% 
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


# BASIC PLOTTING 
# Change of (average) ratings throughout the time
# Get overview and create rating average df
summary(movies$avg_rating)

movies_yearly_rating <- movies %>% 
  group_by(year) %>% 
  summarize(mean(avg_rating, na.rm = TRUE)) %>% 
  rename(average_rating = 'mean(avg_rating, na.rm = TRUE)')

# Plot it with base R and save
png(file="yearly_ratings.png")
plot(
  movies_yearly_rating$year, 
  movies_yearly_rating$average_rating,
  col="#EB299B",
  xlab="Year", 
  ylab="Average rating per year",
  main="Development of movie ratings over time",
  abline(lm(movies_yearly_rating$average_rating ~ movies_yearly_rating$year), 
  col="dark grey"))
dev.off()

# Now compared to ggplot: create the same as part of a double-plot
# Goal: Show importance of technology milestones

# Milestones:
# 1885: Moving pictures (Area of silent films -> 1927)
# 1916: Color movies
# 1927: Synchronized dialog
# 1955: Portable cameras
# 1973: Computer-Generated Imagery (CGI)
# 1991: VCR - Home Viewing
# 2000: Digital age - Internet and special effects

# Plot 1: Average rating throughout the year (as before)
p_ratings_gg <- movies_yearly_rating %>% 
  ggplot(mapping = aes(x = year)) +
  geom_point(aes(y = average_rating), color ="dark blue", alpha = 0.5) +
  stat_smooth(aes(y = average_rating), color="dark grey") +
  scale_x_continuous(breaks=seq(1885,2020,15)) +
  geom_vline(xintercept = c(1885,1916,1927,1955,1973,1991,2000), col = "red") +
  labs(y = "Average rating",
       title = "Influence of technological improvements on movies",
       subtitle = "Yearly average movie ratings") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# Extra: Add the milestone in the graph -> Manual way/bruteforcing:
# ggplot(data = mtcars, aes(x = disp, y = mpg, color = factor(vs))) +
#   geom_line(show.legend = FALSE) +
#   geom_vline(aes(xintercept = mean(disp)), color = "red") +
#   geom_vline(aes(xintercept = median(disp)), color = "blue") +
#   geom_label(aes(mean(disp), 4, label = "mean"), show.legend = FALSE) +
#   geom_label(aes(median(disp), 6, label = "median"), show.legend = FALSE) +
#   facet_wrap(~ factor(am))

# Plot 2: No. of films produced throughout the time
p_milestones_gg <- movies %>% 
  ggplot(mapping = aes(x = year)) + 
  geom_bar(color="dark blue", alpha = 0.2) +
  scale_x_continuous(breaks=seq(1885,2020,15)) +
  geom_vline(xintercept = c(1885,1916,1927,1955,1973,1991,2000),
             col = "red") +
  #geom_text(aes(x = c(1885,1916,1927,1955,1973,1991,2000), 
  #              y = 45000, 
  #              label = c("Moving pictures","Color movies","Synchronized dialog","Portable cameras","CGI","VCR","Digital")), 
  #              size = 3, vjust = 0, hjust = 0, nudge_x = 50) +
  labs(x = "Year",
       y = "Total number of movies",
       subtitle ="Yearly number of released movies",
       caption = "Each red line marks a technological milestone in the movie industry.")

# Arrange plots 1 and 2 with Patchwork

patchwork <- p_ratings_gg / p_milestones_gg
patchwork + plot_layout(ncol=1,heights=c(3,3))
ggsave('tech_milestones_ratings.png', patchwork)

# creating a animated worldmap with yearly released movies

# cleaning dataset for world map plotting (excl. countries without ISO-based country codes)
movies_map <- 
  movies %>% 
  filter(!prod_ct %in% c("Czechoslovakia", "East Germany", "Kosovo", "Netherlands Antilles", 
                         "none", "Serbia and Montenegro", "Yugoslavia", "Soviet Union", NA),
         year < 2018)

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

movies_map <- within(movies_map, uid <- paste(country_iso3c, year, sep = "."))

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

# using preset breaks for easier classification than the automatically set ones
br <- c(1,2,5,10,25,50,100,500,1000) 


# plotting
a <- ggplot(data = movies_map) +
  borders("world", colour = "gray90", fill = "gray85") +
  theme_map() +
  geom_jitter(aes(x = Longitude, y = Latitude, size = n), 
             colour = "darkred", alpha = 0.3) +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = ifelse((movies_map$n >= 50),n,"")), 
                   fill = "darkred", color = "white", size = 3) +
  # adjusting coord so that Antarctica is not on the map
  coord_cartesian(ylim = c(-50, 90)) + 
  
  labs(title = "Movies released in year: {frame_time}", 
       subtitle = "Despite dominance of American and European movie production countries, relevance of Central Asian studios grows. Special emphasis on yearly production of more than 50 movies",
       caption = "Map by Group 3 // BAN422; source: IMDb and OMDb dataset", 
       size = "Nr. of Movies") +

  scale_size(breaks = br) +

    theme( plot.title = element_text(color = "black", size = 14, face = "bold"),
         plot.subtitle = element_text(color = "darkgrey", size = 12),
         legend.position = c(0,0), 
         legend.direction = "horizontal", 
         legend.title.align = 0,
         legend.key.size = unit(0.8, "cm"),
         legend.title=element_text(size=10), 
         legend.text=element_text(size=10)) +

  # including animation dimension
  transition_time(year)

# create animation
a_anim <- 
  animate(a,
        duration = 30,
        fps = 5, 
        height = 500, 
        width = 1000)
        # renderer = gifski_renderer(loop = F)) # stop at last frame

# save gif
save_animation(a_anim, "released_movies")
anim_save("released_movies", animation = last_animation())





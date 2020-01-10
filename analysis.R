# Load libraries and data, setup
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}

load("rda/top50.rda")

# Convert categoricals to factors
topsongs <- topsongs %>% mutate_at(vars(genre), as.factor)

# Group subgenres into more general genres
length(levels(topsongs$genre))
levels(topsongs$genre)

topsongs <- topsongs %>% 
  mutate(genre_group = as.factor(case_when(
    genre %in% c("atl hip hop", "escape room", "canadian hip hop", 
                 "dfw rap", "country rap") ~ "Hip-Hop",
    genre %in% c("brostep", "australian pop", "canadian pop",
                 "panamanian pop", "boy band", "electropop",
                 "pop") ~ "Pop",
    genre %in% c("latin", "r&b en espanol", "reggaeton", 
                 "reggaeton flow") ~ "Latin",
    genre %in% c("dance pop", "pop house", "trap music", 
                 "big room", "edm") ~ "Dance")))

# Summarize just the numeric data
summary(topsongs[,4:13])
sd(topsongs$popularity)

# Peek at popularity distribution
topsongs %>% ggplot(aes(x = popularity)) +
  theme_light() +
  geom_histogram(col = "black", binwidth = 1) +
  labs(x = "Popularity", y = "Song Count") +
  ggtitle("Popularity Distribution")

# Which is the most common genre of the top 50?
topsongs %>% ggplot(aes(x = genre_group)) +
  theme_minimal() +
  geom_bar(aes(fill = genre_group)) +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Song Count") +
  ggtitle("Song Counts by Genre")

table(topsongs$genre_group)

# What are the top 5 songs by popularity?
topsongs %>% arrange(desc(popularity)) %>%
  slice(1:5) 

# Is there a correlation between energy and bpm?
topsongs %>% ggplot(aes(x = bpm, y = energy)) +
  geom_point()

# Loudness and energy?
topsongs %>% ggplot(aes(x = loudness, y = energy)) +
  geom_point(aes(color = valence)) + 
  geom_smooth(method = "lm")

# Danceability and energy?
topsongs %>% ggplot(aes(x = danceability, y = energy)) +
  geom_point() 

# Acousticness and energy?
topsongs %>% ggplot(aes(x = acousticness, y = energy)) +
  geom_point()

# Explore distributions
topsongs %>% ggplot(aes(x = liveness)) +
  geom_histogram(col = "black", binwidth = 5)

topsongs %>% ggplot(aes(x = acousticness)) +
  geom_histogram(col = "black", binwidth = 5)

topsongs %>% ggplot(aes(x = genre_group, y = energy)) +
  theme_light() +
  geom_boxplot(aes(fill = genre_group)) + 
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Energy") +
  ggtitle("Energy by Genre")

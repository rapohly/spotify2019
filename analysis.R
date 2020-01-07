library(ggplot2)
library(dplyr)
library(stringr)

load("rda/top50.rda")

# Convert genre to factor
topsongs <- topsongs %>% mutate_at(vars(genre), as.factor)

# Summarize just the numeric data
summary(topsongs[,4:ncol(topsongs)])

# Group subgenres into more general genres
ind <- str_detect(topsongs$genre, "[^*a-z]rap|hip|room")
hiphop.grp <- topsongs$genre[ind]
ind <- str_detect(topsongs$genre, "pop")
pop.grp <- topsongs$genre[ind]
ind <- str_detect(topsongs$genre, "latin|reggaeton|espanol")
latin.grp <- topsongs$genre[ind]
ind <- str_detect(topsongs$genre, "trap|bro|big|boy|edm")
dance.grp <- topsongs$genre[ind]

topsongs <- topsongs %>% 
  mutate(genre_group = as.factor(case_when(
    genre %in% hiphop.grp ~ "Hip-Hop",
    genre %in% pop.grp ~ "Pop",
    genre %in% latin.grp ~ "Latin",
    genre %in% dance.grp ~ "Dance")))

rm(ind, hiphop.grp, pop.grp, latin.grp, dance.grp)

# What are the top 10 most popular songs?
topsongs %>% arrange(desc(popularity)) %>%
  slice(1:10)

# Are songs with higher valence more popular?
topsongs %>% ggplot(aes(x = valence, y = popularity)) +
  geom_point()

# Is there a correlation between energy and bpm?
topsongs %>% ggplot(aes(x = bpm, y = energy)) +
  geom_point()

# Loudness and energy?
topsongs %>% ggplot(aes(x = loudness, y = energy,
                        color = genre_group)) +
  geom_point()

# Danceability and energy?
topsongs %>% ggplot(aes(x = danceability, y = energy)) +
  geom_point()

# Acousticness and energy?
topsongs %>% ggplot(aes(x = acousticness, y = energy)) +
  geom_point()

# Energy and popularity?
topsongs %>% ggplot(aes(x = popularity, y = energy)) +
  geom_point()

# Which is the most common genre of the top 50?
topsongs %>% ggplot(aes(x = genre_group)) +
  theme_minimal() +
  geom_bar(fill = "red") +
  labs(x = "Genre", y = "Song Count")

# Explore distributions
topsongs %>% ggplot(aes(x = liveness, fill = genre_group)) +
  geom_histogram(col = "black", binwidth = 5)

topsongs %>% ggplot(aes(x = acousticness)) +
  geom_histogram(col = "black", binwidth = 5)
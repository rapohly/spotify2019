library(ggplot2)
library(dplyr)

load("rda/top50.rda")

# Summarize just the numeric data
summary(topsongs[,4:ncol(topsongs)])

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
topsongs %>% ggplot(aes(x = loudness, y = energy)) +
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

# Explore distributions
topsongs %>% ggplot(aes(x = liveness)) +
  geom_histogram(col = "black", binwidth = 2)

topsongs %>% ggplot(aes(x = acousticness)) +
  geom_histogram(col = "black", binwidth = 2)

# Load libraries and data, setup
options(digits = 3)

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
popdist.plt <- topsongs %>% ggplot(aes(x = popularity)) +
  theme_light() +
  geom_histogram(col = "black", binwidth = 1) +
  labs(x = "Popularity", y = "Song Count") +
  ggtitle("Popularity Distribution")
popdist.plt

ggsave("figs/popdist.png", plot = popdist.plt)

# Which is the most common genre of the top 50?
genres.plt <- topsongs %>% ggplot(aes(x = genre_group)) +
  theme_minimal() +
  geom_bar(aes(fill = genre_group)) +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Song Count") +
  ggtitle("Song Counts by Genre")
genres.plt

ggsave("figs/genres.png", plot = genres.plt)

table(topsongs$genre_group)

# Explore distributions
energybox.plt <- topsongs %>% 
  ggplot(aes(x = genre_group, y = energy)) +
  theme_light() +
  geom_boxplot(aes(fill = genre_group)) + 
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Energy") +
  ggtitle("Energy by Genre")
energybox.plt

ggsave("figs/energybox.png", plot = energybox.plt)

energydens.plt <- topsongs %>% 
  ggplot(aes(x = energy, fill = genre_group)) +
  theme_light() +
  geom_density(alpha = 0.4) + 
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Energy", y = "Density") +
  ggtitle("Energy Density by Genre")
energydens.plt

ggsave("figs/energydens.png", plot = energydens.plt)

bpmbox.plt <- topsongs %>%
  ggplot(aes(x = genre_group, y = bpm)) +
  theme_light() +
  geom_boxplot(aes(fill = genre_group)) + 
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Beats Per Minute") +
  ggtitle("Beats Per Minute by Genre")
bpmbox.plt

ggsave("figs/bpmbox.png", plot = bpmbox.plt)

bpmdens.plt <- topsongs %>% 
  ggplot(aes(x = bpm, fill = genre_group)) +
  theme_light() +
  geom_density(alpha = 0.4) + 
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Beats Per Minute", y = "Density") +
  ggtitle("BPM Density by Genre")
bpmdens.plt

ggsave("figs/bpmdens.png", plot = bpmdens.plt)

# EXPLORE CORRELATIONS
# BPM vs energy
bpmenergy.plt <- topsongs %>% group_by(genre_group) %>%
  mutate(rho = cor(energy, bpm)) %>%
  ggplot(aes(x = bpm, y = energy)) +
  theme_light() +
  geom_point() +
  facet_wrap(genre_group ~ rho) +
  labs(x = "Beats Per Minute", y = "Energy") +
  ggtitle("Stratified BPM vs Energy")
bpmenergy.plt

ggsave("figs/bpmenergy.png", plot = bpmenergy.plt)

# Energy vs loudness
energyloud.plt <- topsongs %>%
  ggplot(aes(x = loudness, y = energy)) +
  theme_light() +
  geom_point(aes(color = genre_group), 
             alpha = 0.8) +
  scale_color_brewer(palette = "RdBu") +
  geom_smooth(method = "lm") +
  labs(x = "Loudness", y = "Energy") +
  ggtitle("Energy vs Loudness")
energyloud.plt

ggsave("figs/energyloud.png", plot = energyloud.plt)

cor(topsongs$loudness, topsongs$energy)

speechbox.plt <- topsongs %>% ggplot(aes(x = genre_group, 
                        y = speechiness,
                        fill = genre_group)) +
  theme_light() +
  geom_boxplot() +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Genre", y = "Speechiness") +
  ggtitle("Speechiness by Genre")
speechbox.plt

ggsave("figs/speechbox.png", plot = speechbox.plt)

topsongs %>% group_by(genre_group) %>%
  summarize(mean = mean(speechiness)) %>%
  arrange(desc(mean))
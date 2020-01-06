library(tidyverse)

topsongs <- read_csv("data/top50.csv")

topsongs <- topsongs %>% select(-X1)

colnames(topsongs) <- c("track_name", "artist", "genre", "bpm",
                        "energy", "danceability", "loudness", "liveness",
                        "valence", "length", "acousticness", "speechiness",
                        "popularity")

topsongs$track_name <- str_remove(topsongs$track_name, "-\\s[A-Za-z]+")
topsongs$track_name <- str_to_title(topsongs$track_name)

ind <- c(1, 17, 18, 33, 40, 47)
titles <- c("Sexf1orita", "La Cancion", "Sunflower",
            "Unknown Title", "Fuck, I'm Lonely",
            "Te Robar")

topsongs$track_name[ind] <- titles

topsongs$artist[45] <- "Rosala"

save(topsongs, file = "rda/top50.rda")

rm(list=ls())
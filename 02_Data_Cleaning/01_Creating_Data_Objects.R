###############################################################################
# Dieses Skript erstellt die R Objekte für die weitere Analyse.
###############################################################################

library(tidyverse)
library(furrr) # Weiterentwicklung des purr package

# Parallelverarbeitung, um das Laden der csv deutlich zu beschleunigen
plan(multisession)


### 1.) Video Metadaten
files <- list.files("Data/Video_Meta", full.names = TRUE, recursive = TRUE)

# Paralleles Laden mit furrr package
F <- future_map(files, read_csv)

# Dataframe erstellen
D <- bind_rows(F)

# Parteizugehörigkeit der ausgewählten YouTube-Kanäle
D <- D %>%
  mutate(
    party = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "AfD",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "BSW",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "Die Linke",
      channel %in% c("@spdde", "@larsklingbeil") ~ "SPD",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "CDU/CSU",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "FDP",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "Die Grünen",
      TRUE ~ "Keine Partei" 
    )
  )

# Klassifizierung Populismus
D <- D %>%
  mutate(
    pop = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "populistisch",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "populistisch",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "populistisch",
      channel %in% c("@spdde", "@larsklingbeil") ~ "nicht-populistisch",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "nicht-populistisch",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "nicht-populistisch",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "nicht-populistisch",
      TRUE ~ "Keine Daten" 
    )
  )
# Sichern der Meta Daten
saveRDS(D, file = "Video_Meta_All.rds")




### 2.) Kommentare
files <- list.files("Data/Comments", full.names = TRUE, recursive = TRUE)

F <- future_map(files, read_csv)


D <- bind_rows(F)

# Parteizugehörigkeit der ausgewählten YouTube-Kanäle
D <- D %>%
  mutate(
    party = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "AfD",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "BSW",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "Die Linke",
      channel %in% c("@spdde", "@larsklingbeil") ~ "SPD",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "CDU/CSU",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "FDP",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "Die Grünen",
      TRUE ~ "Keine Partei"
    )
  )


# Klassifizierung Populismus
D <- D %>%
  mutate(
    pop = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "populistisch",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "populistisch",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "populistisch",
      channel %in% c("@spdde", "@larsklingbeil") ~ "nicht-populistisch",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "nicht-populistisch",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "nicht-populistisch",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "nicht-populistisch",
      TRUE ~ "Keine Daten" 
    )
  )

# Sichern der Kommentardaten
saveRDS(D, file = "Comments_All.rds")




### 3.) Untertitel
files <- list.files("Data/Captions", full.names = TRUE, recursive = TRUE)


F <- future_map(files, read_csv)

D <- bind_rows(F)

D <- D %>%
  mutate(
    party = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "AfD",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "BSW",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "Die Linke",
      channel %in% c("@spdde", "@larsklingbeil") ~ "SPD",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "CDU/CSU",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "FDP",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "Die Grünen",
      TRUE ~ "Keine Partei" 
    )
  )
D <- D %>%
  mutate(
    pop = case_when(
      channel %in% c("@AfDFraktionimBundestag", "@AfDTV", "@Alice.Weidel") ~ "populistisch",
      channel %in% c("@BuendnisSahraWagenknecht", "@SahraWagenknechtMdB") ~ "populistisch",
      channel %in% c("@DIELINKE", "@janine_wissler", "@martinschirdewan1556") ~ "populistisch",
      channel %in% c("@spdde", "@larsklingbeil") ~ "nicht-populistisch",
      channel %in% c("@cdutv", "@csumedia", "@FriedrichMerzCDU", "@cducsu") ~ "nicht-populistisch",
      channel %in% c("@FDP", "@c_lindner", "@fdpbt") ~ "nicht-populistisch",
      channel %in% c("@DieGruenen", "@annalenabaerbock5905", "@Nouripour", "@gruenebundestag", "@robert.habeck") ~ "nicht-populistisch",
      TRUE ~ "Keine Daten" 
    )
  )


# Sichern der Untertiteldaten
saveRDS(D, file = "Captions_All.rds")

###############################################################################
# Dieses Skript fragt die Kanal-ID's von einer vordefinierten Liste von 
# YouTube Kanälen über die YouTube API ab.
###############################################################################

library(tidyverse)
library(tuber) # https://github.com/gojiplus/tuber
# Docs: https://cran.r-project.org/web/packages/tuber/tuber.pdf
library(ytcol) # devtools::install_github("Vintonm49/ytcol", build_vignettes = TRUE)
# https://github.com/Vintonm49/ytcol


# Authentifizierung des YouTube API Accounts
# Entfernen der cached Authentifizierung, falls notwendig:
# system("rm .httr-oauth")
# system("del .httr-oauth")
yt_oauth(client_id, client_secret, token = "")


# Dataframe mit den relevanten YouTube Kanälen und @-Handles
D <- data.frame(
  channel_name = c(
    # Populistisch
    "Sarah Wagenknecht",
    "AfD-Fraktion Bundestag",
    "Bündnis Sahra Wagenknecht",
    "AfD TV",
    "Janine Wissler",
    "Martin Schirdewan",
    "Alice Weidel",
    
    # Nicht-Populistisch
    "CDU",
    "CSU",
    "DIE LINKE",
    "BÜNDNIS 90/DIE GRÜNEN",
    "FDP",
    "SPD",
    "Nouripour",
    "Annalena Baerbock",
    "Christian Lindner",
    "Lars Klingbeil",
    "Friedrich Merz",
    "Fraktion der Freien Demokraten",
    "CDU•CSU Fraktion",
    "Grüne im Bundestag",
    "Robert Habeck"
  ),
  channel = c(
    # Populistisch
    "@SahraWagenknechtMdB",
    "@AfDFraktionimBundestag",
    "@BuendnisSahraWagenknecht",
    "@AfDTV",
    "@janine_wissler",
    "@martinschirdewan1556",
    "@Alice.Weidel",
    
    # Nicht-Populistisch
    "@cdutv",
    "@csumedia",
    "@DIELINKE",
    "@DieGruenen",
    "@FDP",
    "@spdde",
    "@Nouripour",
    "@annalenabaerbock5905",
    "@c_lindner",
    "@larsklingbeil",
    "@FriedrichMerzCDU",
    "@fdpbt",
    "@cducsu",
    "@gruenebundestag",
    "@robert.habeck"
    
  )
)

# Erstellen einer csv Datei aus dem Dataframe
write_csv(D, "Channels.csv")

C <- read_csv("Channels.csv")

# Neue Variable, die mit der alpha-numberischen ID jedes Kanals gefüllt werden soll
C$channel_id <- NA


# Funktion yt.GetChannelID() fragt die API für die alpha-numerische Kanal-ID ab

for(i in 1:nrow(C)) {
  
  # Wenn die Channel ID nicht NA und nicht leer ist:
  if(!is.na(C$channel[i]) & C$channel[i] != "") {
    
    channel_id <- yt.GetChannelID(C$channel[i])
    
    # Nur, wenn die Funktion einen Wert erhält, 
    # überschreibt sie die leere Variable.
    if(length(channel_id) > 0) {
      C$channel_id[i] <- channel_id
    }
    
  }
}

# Erstellen einer csv Datei aus den Channel_IDs
write_csv(C, "Channel_IDs.csv")

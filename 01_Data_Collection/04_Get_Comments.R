###############################################################################
# Dieses Skript wird:
#
# (1) Alle Daten der Video-IDs laden, die beim Ausführen der
# Datei 2_Get_Video_IDs.R abgefragt wurden.
# (2) Loop durch alle Video-IDs (d.h. über alle Videos, für die wir
# Kommentare) und lädt die Kommentare zu jedem Video herunter
###############################################################################

library(tidyverse)
library(tuber)

# Authentifizierung des YouTube API Accounts
# Entfernen der cached Authentifizierung, falls notwendig:
# system("rm .httr-oauth")
# system("del .httr-oauth")
yt_oauth(client_id, client_secret, token = "")

# Liste der Video IDs laden.
D <- readRDS("Video_ID_Zuschnitt.rds")

# Kontrollieren, ob die Kommentare schon heruntergeladen worden, um sie nicht doppelt abzufragen:
# Ist vor allem dann wichtig, um zwischen den Sitzungen nicht den Überblick zu verlieren.
# Falls ich die Kommentare updaten will, weil beispielsweise neue dazugekommen sind, dann 
# diesen Teil nicht benutzen.

S <- c(unique(D$channel))
files <- c()

for (i in 1:length(S)) {
  channel <- S[i]
  files2 <- list.files(str_c("Data/Comments/",channel))
  files <- c(files, files2)
}


files <- str_sub(files, start = 1, end = 11)

# Manche Video's produzieren Errors, werden also bei jeder neuen Sitzung wieder
# Abgefragt. Deshalb wird eine Datei mit den Video IDs erstellt und aus der 
# Liste der Video IDs, die abgefragt werden, entfernt:
#error <- c("A4YrrkXB2ws")
#saveRDS(error, file = "error_video_ids.rds")

#error_video_ids <- readRDS("error_video_ids.rds")
#files <- c(files, error_video_ids)

#for(i in 1:length(files)){
#  D <- D %>% filter(contentDetails.videoId != files[i])
#}


# For Loop, die die Kommentare über die spezifischen Video IDs abfragen.
for(i in 1:nrow(D)) {
  
  video_id <- D$contentDetails.videoId[i]
  channel_id <- D$channel_id[i]
  channel_name <- D$channel_name[i]
  channel <- D$channel[i]
  
  
  print(paste("Trying to collect video comments Nr.", i, "with Video ID:", video_id))
  
  # get_all_comments() fragt alle Kommentare ab und tryCatch sorgt dafür, dass
  # bei einer Fehlermeldung eine Nachricht erscheint und die fehlerhafte Video ID
  # gespeichert wird
  Comments <- tryCatch(get_all_comments(video_id = video_id),
                       error = function(error_message) {
                         message("Error in collecting video, or are no comments:", video_id) 
                         message(error_message)
                         # '<<-' updates variables in the global (or parent) environment
                         error_video_ids <<- c(error_video_ids, video_id) 
                         return(NULL)
                       })
  
  # Check, ob die Kommentarliste abgefragt wurde
  if(!is.null(Comments)) {
    
    Comments <- Comments %>%
      mutate(channel_name = channel_name,
             channel = channel,
             channel_id = channel_id) %>%
      relocate(channel_name, channel, channel_id)
    
    # Erstellt einen Pfad für jeden Kanal, in den die Kommentarlisten gespeichert werden
    if(!channel %in% list.files("Data/Comments/")) {
      system(str_c("mkdir Data/Comments/", channel))
    }
    
    # Datei im Pfad:
    # "Data/Comments/[channel_id]/[video_id].csv.gz"
    file_path_name <- str_c("Data/Comments/", channel, "/", video_id, "-", as.factor(Sys.Date()), ".csv.gz")
    
    # Write out the comments for a specific video to file
    write_csv(Comments, file_path_name)
    
    
    Sys.sleep(2) 
    
    
  }
  
}


# Speichern Sie die Videos ohne Kommentare (führt zu einem Fehler)
# Notwendig, wenn die Daten in mehreren Sessions heruntergeladen werden sollen (um eine
# Überlastung der YT-API Cap zu verhindern):
# saveRDS(error_video_ids, file = "error_video_ids.rds")

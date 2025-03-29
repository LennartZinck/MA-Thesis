library(tidyverse)
library(tuber)
library(reticulate) # Python Integration mit R

# Dateiort des Anaconda Environment
use_condaenv("C:/Users/user_name/anaconda3/envs/R_youtube_caption/python.exe")

# Liste der Video IDs laden.
D <- readRDS("Video_ID_Zuschnitt.rds")


# Kontrollieren, ob die Transkripte schon heruntergeladen worden sind:
S <- c(unique(D$channel))

files <- c()

for (i in 1:length(S)) {
  channel <- S[i]
  files2 <- list.files(str_c("Data/Captions/",channel))
  files <- c(files, files2)
}


files <- str_sub(files, start = 1, end = 11)
# Das sind Videos, bei denen die automatischen Untertitel deaktiviert sind,
# oder immer zu einem Fehler führen, wenn sie nicht ausgeschlossen werden.
# error_cap_ids <- c("")
# saveRDS(error_cap_ids, "error_cap_ids.rds")

error_cap_ids <- readRDS("error_cap_ids.rds")
files <- c(files, error_cap_ids)

for(i in 1:length(files)){
  D <- D %>% filter(contentDetails.videoId != files[i])
}

# For-Loop abgewandelt von Joo Young Seo (2020) 'youtubecaptions' package
# Siehe: https://github.com/jooyoungseo/youtubecaption
for(i in 1:nrow(D)) {
  
  video_id <- D$contentDetails.videoId[i]
  channel_id <- D$channel_id[i]
  channel_name <- D$channel_name[i]
  channel <- D$channel[i]
  
  print(paste("Trying to collect video transcript Nr.", i, "with Video ID:", video_id))
  
  l <- tryCatch(
    reticulate::import("youtube_transcript_api")$YouTubeTranscriptApi$get_transcripts(video_ids = list(video_id), languages = list("de", "en", "nl")),
    error = function(error_message) {
      message("Error in collecting video:", video_id) 
      message(error_message)
      
      
      error_cap_ids <<- c(error_cap_ids, video_id) # "<<-" updates variables in the global (or parent) environment
      return(NULL)
    }
  )
  
  captions <- l[[1]][[1]] %>%
    purrr::map_dfr(~ tibble::as_tibble(.)) %>%
    tibble::rowid_to_column("segment_id") %>%
    dplyr::mutate(video_id = video_id)
  
  captions$text <- paste(captions$text, collapse = " ")
  
  captions <- captions[2, c("text")]
  
  captions <- captions %>%
    mutate(channel_name = channel_name,
           channel = channel,
           channel_id = channel_id,
           video_id = video_id) %>%
    relocate(channel_name, channel, channel_id, video_id, text)
  
  if(!is.na(captions$text)){
    file_path_name <- str_c("Data/Captions/", channel, "/", video_id, ".csv.gz")
    
    write_csv(captions, file_path_name)
  }
  Sys.sleep(2)
}

# Die Videos, die eine Fehlermeldung produzieren, können hiermit gespeichert
# werden, um zwischen einzelnen Sitzungen nicht den Überblick zu verlieren.
saveRDS(error_cap_ids, file = "error_cap_ids.rds")

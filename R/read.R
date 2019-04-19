library(readxl)
library(dplyr)

read_oracleselixir_match_data <- function(path) {
  df <-  read_excel(path, col_names = TRUE)
  
  cols_to_remove <- c("url")
  
  df_model <- df[, !(colnames(df) %in% cols_to_remove)]
  
  df_model[["player"]] <- tolower(df_model[["player"]])
  df_model[["player"]] <- clean_name(df_model[["player"]])
  df_model[["team"]] <- toupper(df_model[["team"]])
  
  df_cleaned <- abbreviate_positions(df_model, "position")
  df_cleaned <- abbreviate_teams(df_cleaned, "team")
  
  # Get the correct datatypes
  df_cleaned[["week"]] <- as.integer(df_cleaned[["week"]])
  df_cleaned[["patchno"]] <- as.character(df_cleaned[["patchno"]])
  df_cleaned[["gamelength"]] <- as.double(df_cleaned[["gamelength"]])
  
  tibble::as_tibble(df_cleaned)
}

clean_name <- function(name) {
  to_replace_list = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  
  cleaned <- chartr(paste(names(to_replace_list), collapse=''),
                    paste(to_replace_list, collapse=''),
                    name)
  
  cleaned
}
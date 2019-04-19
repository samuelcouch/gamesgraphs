# Parse teams from game_info string
parse_teams <- function(x) {
  m <- regexec("([A-Za-z0-9]{2,3})@([A-Za-z0-9]{2,3})", x)
  regs <- regmatches(x, m)
  
  # replace empty vectors with NAs
  regs[] <- lapply(regs, function(r) {
    if (length(r) == 0) {
      rep(NA_character_, 3)
    } else {
      r
    }
  })
  
  # convert list to matrix
  regs <- matrix(unlist(regs), ncol = 3, byrow = TRUE)
  regs
}

assert_has_cols <- function(df, cols) {
  is_in_df <- cols %in% colnames(df)
  has_all_cols <- all(is_in_df)
  if (!has_all_cols) {
    missing <- paste0(cols[!is_in_df], collapse = ", ")
    stop(paste("missing columns", missing), call. = FALSE)
  }
}

unnest_col <- function(df, colname) {
  # original column order
  cnames <- colnames(df)
  
  # add dummy id
  n <- nrow(df)
  df[[".i"]] <- seq_len(n)
  
  # unnest column
  df_unnest <- lapply(seq_len(n), function(i) {
    pos <- df[[colname]][[i]]
    d <- data.frame(i, pos, stringsAsFactors = FALSE)
    colnames(d) <- c(".i", colname)
    d
  })
  
  df_unnest <- do.call(rbind, df_unnest)
  
  # remove col from orig data frame
  df <- df[setdiff(colnames(df), colname)]
  
  # join back to data
  df_merge <- merge(df, df_unnest, by = c(".i"))
  df_merge <- df_merge[setdiff(colnames(df_merge), ".i")]
  
  # use original column order
  df_merge[cnames]
}

abbreviate_positions <- function(df, colname) {
  cnames <- colnames(df)
  
  new_df <- left_join(df, positions_df(), by = colname )
  new_df$position <- NULL
  
  names(new_df)[names(new_df) == "pos_abbrv"] <- colname
  
  new_df[cnames]
}

abbreviate_teams <- function(df, colname) {
  cnames <- colnames(df)
  
  new_df <- left_join(df, teams_df(), by = colname)
  new_df$team <- NULL
  
  names(new_df)[names(new_df) == "short_team"] <- colname
  
  new_df[cnames]
}

positions_df <- function() {
  long <- c("Top", "Middle", "Jungle", "ADC", "Support", "Team")
  short <- c("TOP", "MID", "JNG", "ADC", "SUP", "TEAM")
  
  df <- data.frame(long, short)
  
  colnames(df) <- c("position", "pos_abbrv")
  df
}

teams_df <- function() {
  df <- read.csv("data/team_data.csv")
  df[["team"]] <- toupper(df[["team"]])
  df$league <- NULL
  
  df
}
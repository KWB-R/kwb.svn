# read_histories ---------------------------------------------------------------

#' Read Histories
#'
#' @param history_dir directory with history files (as created with 
# 'read_files_history() )
#' @return data.frame with columns
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate bind_rows 
#' @importFrom stringr str_detect str_subset str_remove str_split
#' @importFrom fs dir_info
#' @importFrom kwb.file remove_common_root
read_histories <- function(history_dir)
{
  path_info <- fs::dir_info(
    history_dir, recurse = TRUE, all = FALSE, type = "file"
  )
  
  files <- path_info$path
  paths <- kwb.file::remove_common_root(dirname(files))
  
  result <- lapply(seq_along(files), function(i) {
    
    file <- files[i]
    path <- paths[i]
    
    #file <- path_info$path[2]
    changes <- kwb.utils::catAndRun(
      messageText = paste("Reading", basename(file)), 
      expr = readLines(file)
    )
    
    author_start_pattern <- "^r[0-9]{4} \\| "
    author_start <- which(stringr::str_detect(changes, author_start_pattern))
    
    which_detect <- function(x, p) which(stringr::str_detect(x, pattern = p))
    
    start <- which_detect(changes, "^===========")
    end <- c(which_detect(changes, "^-------------")[-1])
    
    if (length(author_start) > length(start)) {
      n_rep <- length(author_start) - length(start)
      start <- c(start, rep(author_start[length(author_start)], n_rep))
    }
    
    if (length(author_start) > length(end)) {
      n_rep <- length(author_start) - length(end)
      end <- c(end, rep(author_start[length(author_start)], n_rep))
    }
    
    if (length(author_start) < length(end)) {
      n_drop <- length(end) - length(author_start) 
      end <- end[1:(length(end)-n_drop)]
    }
    
    if (length(start) > length(end)) {
      n_drop <- length(start) - length(end) 
      start <- start[1:(length(start)-n_drop)]
    }
    
    
    # Split the revision header lines and prepare a revision summary data frame
    hist_log_df <- changes[author_start] %>%
      split_revision_lines() %>%
      init_revision_summary(file, path, start, end)
    
    for (i in seq_len(nrow(hist_log_df))) {
      
      sel_lines <- changes[hist_log_df$lines_start[i]:hist_log_df$lines_end[i]]
      
      added <- sum(stringr::str_detect(sel_lines, pattern = "^+"))
      deleted <- sum(stringr::str_detect(sel_lines, pattern = "^-"))
      
      hist_log_df$lines_added[i] <- ifelse(added > 0, added, NA_integer_)
      hist_log_df$lines_deleted[i] <- ifelse(deleted > 0, deleted, NA_integer_)
    }
    
    hist_log_df
  })
  
  dplyr::bind_rows(result)
}

# split_revision_lines ---------------------------------------------------------
split_revision_lines <- function(x)
{
  stringr::str_split(x, pattern = "\\s*\\|\\s*", n = 4, simplify = TRUE) %>% 
    as.data.frame() %>%
    stats::setNames(c("revision", "username", "datetime", "lines_comment"))
}

# init_revision_summary --------------------------------------------------------

#' Initialise Revision Summary
#' @keywords internal
init_revision_summary <- function(hist_log, file, path, start, end)
{
  # Define helper function
  extract_int <- function(x, p) as.integer(stringr::str_remove(x, p))

  hist_log %>%  
    dplyr::mutate(
      file = basename(file),
      path = path,
      revision = extract_int(.data$revision, "^r"), 
      datetime = stringr::str_sub(.data$datetime, 1, 25),
      lines_comment = extract_int(.data$lines_comment, "\\slines?$"),
      lines_start = start, 
      lines_end = end, 
      lines_added = NA_integer_,
      lines_deleted = NA_integer_
    )
}

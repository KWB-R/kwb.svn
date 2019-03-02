# readLogFilesInDirectory ------------------------------------------------------
#' Read SVN Logfiles In A Directory
#'
#' @param logdir directory containing svn log information
#' @param pattern pattern (default: "_log_")
#' @return data frame with svn log data 
#' @importFrom kwb.utils resetRowNames
#' @export
readLogFilesInDirectory <- function(logdir, pattern = "_log_")
{
  # Get paths to log files
  logfiles <- dir(logdir, pattern = pattern, full.names = TRUE)
  
  # Read all log files and bind their content together
  logdata <- do.call(rbind, lapply(logfiles, readLogFile))

  # Generate POSIXct timestamps from "date" column  
  times_utc <- substr(kwb.utils::selectColumns(logdata, "date"), 1, 19) %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    
  # Set column "dateTimeUTC"
  logdata$dateTimeUTC <- times_utc
  
  # Order rows by time and renumber rows
  kwb.utils::resetRowNames(logdata[order(times_utc), ])
}

# readLogFile ------------------------------------------------------------------
#' Read Logfile
#'
#' @param logfile logfile
#' @return logfile
#' @importFrom dplyr bind_rows
#' @importFrom XML xmlTreeParse xmlChildren
#' @export

readLogFile <- function(logfile)
{
  kwb.utils::catAndRun(
    
    messageText = paste0("Reading", basename(logfile)), 
    
    expr = {
      
      xmllines <- readLines(logfile)
      
      indices <- c(grep("^<\\?xml", xmllines), length(xmllines) + 1)
      
      xmls <- lapply(seq_len(length(indices) - 1), function(i) {
        
        xmllines[seq(indices[i], indices[i + 1] - 1)]
      })
      
      xmltexts <- sapply(xmls, paste, collapse = "\n")
      
      xmltexts <- xmltexts[! duplicated(xmltexts)]
      
      dplyr::bind_rows(lapply(xmltexts, function(xmltext) {
        
        xmltree <- XML::xmlTreeParse(xmltext, asText = TRUE)
        
        extractCommitInfo(XML::xmlChildren(xmltree$doc$children$lists))
      }))
    }
  )  
}

# extractCommitInfo ------------------------------------------------------------

#' Get Commit Infos
#'
#' @param commits commits
#'
#' @return commit infos
#' @export
#' @importFrom kwb.utils safeRowBind
extractCommitInfo <- function(commits)
{  
  i <- 1
  
  commitInfo <- NULL
  
  for (commit in commits) {
    commitInfo <- kwb.utils::safeRowBind(commitInfo, extractCommit(commit))
    i <- i + 1
  }
  
  commitInfo
}

# extractCommit ----------------------------------------------------------------
#' Title
#'
#' @param commit commit number
#' @importFrom XML xmlChildren xmlAttrs
#' @return data frame with commit information
#' @export
#' 
extractCommit <- function(commit)
{
  result <- NULL
  
  entries <- xmlChildren(commit)
  
  for (entry in entries) {
    
    entryParts <- XML::xmlChildren(entry)
    
    basicinfo <- data.frame(
      revision = as.integer(XML::xmlAttrs(entryParts$commit)["revision"]),
      kind = XML::xmlAttrs(entry)["kind"],
      stringsAsFactors = FALSE
    )
    
    result <- rbind(
      result, 
      cbind(
        basicinfo,
        extractEntryNameAndCommit(entryParts$name, entryParts$commit)
      )
    )
  }
  
  result
}

# extractEntryNameAndCommit ----------------------------------------------------
#' Extract Entry Name and Commit
#' @param entryName entryName
#' @param entryCommit entryCommit
#' @return data.frame with name, author and data
#' @export
#' @importFrom XML xmlChildren xmlValue 
extractEntryNameAndCommit <- function(entryName, entryCommit)
{
  commitInfo <- XML::xmlChildren(entryCommit)
  
  data.frame(
    name = XML::xmlValue(xmlChildren(entryName)$text),
    author = XML::xmlValue(xmlChildren(commitInfo$author)$text),
    date = XML::xmlValue(xmlChildren(commitInfo$date)$text),
    stringsAsFactors = FALSE
  )
}


#' Default SVN Repository
#' 
#' @param root_dir root directory (default: "svn/kwb")
#' @param username username for repo (default: getOption("svn_username") )
#' @param password password for repo (default: getOption("svn_password") )
#' @param serverip serverip for repo (default: getOption("svn_serverip") )
#' @return string to svn repo with login
#' @export

default_repo <- function(
  root_dir = "svn/kwb", 
  username = getOption("svn_username"), 
  password = getOption("svn_password"),
  serverip = getOption("svn_serverip")
)
{
  sprintf("http://%s:%s@%s/%s",  username, password, serverip, root_dir)
}

#' Default SVN Repository RScripts
#' @export

default_rscripts <- function()
{
  default_repo(root_dir = "svn/kwb/R_Development/trunk/RScripts")
}

#' Default SVN Repository RScripts Paths
#'
#' @export

get_rscript_paths <- function() {
  
  cmd <- sprintf("svn ls -R %s", default_rscripts())
  
  paths <- shell(cmd = cmd, intern = TRUE) 
  
  paths %>% 
    stringr::str_subset("/$", negate = TRUE) %>%  
    stringr::str_subset("\\.[rR]([mM][dD])?$") %>% 
    stringr::str_subset("\\.$", negate = TRUE)
}

# getRevisionInfo---------------------------------------------------------------
#' Get Revision Information
#'
#' @param revision revision number
#' @param tDir target directory
#' @param repo repository
#' @param logs if TRUE logs info is extracted, if FALSE size info (default: TRUE)
#' @param dbg debug messages (default: TRUE)
#' @return revision infos
#' @export
getRevisionInfo <- function(
  revision,
  tDir = tempdir(),
  repo = default_repo(), 
  logs = TRUE,
  ### if TRUE logs info is extracted, if FALSE size info
  dbg = TRUE
)
{
  recursive <- "--recursive"
  fPrefix <- "ente"
  sizeToGrep <- "| grep size"
  label <- "size"
  
  if (logs) {
    recursive <- ""
    fPrefix <- paste0(fPrefix, "_log")
    sizeToGrep <- ""
    label <- "commit info"
  }  
  
  tName <- sprintf("%s_r%d.txt", fPrefix, revision)
  tPath <- file.path(tDir, tName)
  
  cmd <- sprintf(
    "svn list --xml %s -r%d %s %s > \"%s\"", 
    recursive, revision, repo, sizeToGrep, tPath
  )
  
  msg <- sprintf(
    "Get '%s' of r %d from repo %s ...",  
    label, revision, repo
  )
  
  if (dbg) cat(msg)
  shell(cmd = cmd )
  if (dbg) cat("Done!\n") 
  
  tDir
}

#' Get Repo Info
#'
#' @param currentRevision latest SVN revision
#' @param startRevision start SVN revision
#' @param tDir target directory (default: tempdir())
#' @param repo repository (default: default_repo())
#' @param logs if TRUE logs info is extracted, if FALSE size info (default: TRUE)
#' @param dbg debug messages (default: TRUE)
#'
#' @return writes files with repo log info into target directory and returns 
#' path to target directory 
#' @export
getRepoInfo <- function(
  currentRevision = 1920, 
  startRevision = 2, 
  tDir = tempdir(),
  repo = default_repo(), 
  logs = TRUE, 
  dbg = TRUE
)
{
  for (i in  startRevision:currentRevision) { 
    
    getRevisionInfo(
      revision = i, tDir = tDir, repo = repo, logs = logs, dbg = dbg
    )
  }
  
  tDir
}

# readSizeFiles-----------------------------------------------------------------
#' Read Size Files
#' @param fDir directory containing data with file size information
#' @param fPattern file pattern (default: "ente_r")
#' @param dbg debug (default: FALSE)
#' @return list with size file information
#' @export
#' @importFrom plyr rbind.fill
#' 
readSizeFiles <- function (fDir, fPattern = "ente_r", dbg = FALSE) 
{
  x <- list()
  x$size <- data.frame()
  x$sizePerFile <- NULL
  
  files <- dir(fDir,pattern = fPattern, full.names = TRUE)
  fileNames <- dir(fDir,pattern = fPattern, full.names = FALSE)
  revisions <- as.numeric(gsub(pattern = "ente_r|.txt", "", fileNames))
  
  for (id in seq_len(length(files))) {
    revision <- revisions[id]
    if (dbg) cat(sprintf("R %4d : ", revision))
    fileSize <- readLines(con = files[id]) 
    condition <- grep(pattern = "<size>|</size>", x = fileSize)
    fileSize <- fileSize[condition]
    
    sizes <- as.numeric(gsub("<size>|</size>", replacement = "", x = fileSize))
    
    ### convert from byte to  MB
    sizesInMB <- sizes * 10^-6
    x$sizePerFile[[revision]] <- sizesInMB
    
    size  <- data.frame(revision=revision, 
                        sumMB = sum(sizesInMB, na.rm=TRUE),
                        maxMB = max(sizesInMB, na.rm=TRUE))
    x$size <- plyr::rbind.fill(x$size, size)
    
    if (dbg) cat(sprintf("%6.3f (MB) max: %6.3f (MB)\n", size$sumMB , size$maxMB))
  }
  
  x$size <- x$size[order(x$size$revision),]
  
  return(x)
}

#' Read SVN History For Files
#'
#' @param file_paths relative paths to files (relative to repo path)
#' @param repo path to repository (default: default_repo())
#' @param tdir target directory (default: tempdir())
#' @param dbg debug (default: TRUE)
#' @return writes history files to target directory and returns target directory
#' @export
#' @importFrom fs dir_exists dir_create
read_files_history <- function(
  file_paths = get_rscript_paths(), 
  repo = default_rscripts(), 
  tdir = tempdir(), 
  dbg = TRUE
)
{
  target_dirs <- file.path(tdir, unique(dirname(file_paths)))
  
  fs::dir_create(target_dirs[! file.exists(target_dirs)], recursive = TRUE)
  
  sapply(file_paths, FUN = function(file_path) {  
    fpath <- file.path(repo, file_path)
    
    cmd <- sprintf(
      "svn log --diff %s > \"%s\"", 
      file.path(repo, file_path), file.path(tdir, file_path)
    )
    
    msg <- sprintf(
      "Get '%s' from repo %s and export to %s", file_path, repo, tdir
    )
    
    if (dbg) cat(msg)
    shell(cmd = cmd )
    if (dbg) cat("Done!\n") 
    
  })
  
  tdir
}

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

read_histories <- function(history_dir)
{
  history_paths <- fs::dir_info(
    history_dir, recursive = TRUE, all = FALSE, type = "file"
  )$path
  
  result <- lapply(history_paths, function(history_path) {
    
    print(history_path)
    
    history_script <- readLines(history_path)
    
    author_start <- which(stringr::str_detect(
      history_script, pattern = "^r[0-9]{4} \\| "
    ))
    
    start <- which(stringr::str_detect(
      history_script, pattern = "^==========="
    ))
    
    end <- c(which(stringr::str_detect(
      history_script, pattern = "^-------------"
    ))[-1])
    
    if (length(author_start) > length(start)) {
      n_rep <- length(author_start) - length(start)
      start <- c(start, rep(author_start[length(author_start)],n_rep))
    }
    
    if (length(author_start) > length(end)) {
      n_rep <- length(author_start) - length(end)
      end <- c(end, rep(author_start[length(author_start)],n_rep))
    }
    
    if (length(author_start) < length(end)) {
      n_drop <- length(end) - length(author_start) 
      end <- end[1:(length(end)-n_drop)]
    }
    
    hist_log <- stringr::str_subset(history_script, pattern = "^r[0-9]{4} \\| ") %>% 
      stringr::str_split(pattern = "\\s*\\|\\s*",n = 4, simplify = TRUE)  %>% 
      as.data.frame()
    
    names(hist_log) <- c("revision", "username", "datetime", "lines_comment")
    
    hist_log_df <- hist_log %>%  
      dplyr::mutate(
        file = basename(history_path),
        revision = as.integer(stringr::str_remove(.data$revision, "^r")), 
        datetime = stringr::str_sub(.data$datetime, 1, 25),
        lines_comment = as.integer(stringr::str_remove(.data$lines_comment, "\\slines$")),
        lines_start = start, 
        lines_end = end, 
        lines_added = NA_integer_,
        lines_deleted = NA_integer_
      )
    
    for (i in seq_len(nrow(hist_log_df))) {
      
      sel_lines <- history_script[hist_log_df$lines_start[i]:hist_log_df$lines_end[i]]
      
      added <- sum(stringr::str_detect(sel_lines, pattern = "^+"))
      deleted <- sum(stringr::str_detect(sel_lines, pattern = "^-"))
      
      hist_log_df$lines_added[i] <- ifelse(added > 0, added, NA_integer_)
      hist_log_df$lines_deleted[i]  <- ifelse(deleted > 0, deleted, NA_integer_)
    }
    
    hist_log_df
  })
  
  dplyr::bind_rows(result)
}

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
#' @importFrom dplyr bind_rows
extractCommitInfo <- function(commits)
{  
  dplyr::bind_rows(lapply(commits, extractCommit))
}

# extractCommit ----------------------------------------------------------------
#' Title
#'
#' @param commit commit number
#' @importFrom XML xmlChildren xmlAttrs
#' @importFrom dplyr bind_rows
#' @importFrom kwb.utils noFactorDataFrame
#' @return data frame with commit information
#' @export
#' 
extractCommit <- function(commit)
{
  dplyr::bind_rows(lapply(xmlChildren(commit), function(x) {
    
    parts <- XML::xmlChildren(x)
    
    info <- kwb.utils::noFactorDataFrame(
      revision = as.integer(XML::xmlAttrs(parts$commit)["revision"]),
      kind = XML::xmlAttrs(x)["kind"]
    )
    
    cbind(info, extractEntryNameAndCommit(parts$name, parts$commit))
  }))
}

# extractEntryNameAndCommit ----------------------------------------------------
#' Extract Entry Name and Commit
#' @param entryName entryName
#' @param entryCommit entryCommit
#' @return data.frame with name, author and data
#' @export
#' @importFrom XML xmlChildren xmlValue
#' @importFrom kwb.utils noFactorDataFrame
extractEntryNameAndCommit <- function(entryName, entryCommit)
{
  commitInfo <- XML::xmlChildren(entryCommit)
  
  # Define helper function
  childrens_text_value <- function(x) XML::xmlValue(xmlChildren(x)$text)
  
  kwb.utils::noFactorDataFrame(
    name = childrens_text_value(entryName),
    author = childrens_text_value(commitInfo$author),
    date = childrens_text_value(commitInfo$date)
  )
}


#' Default SVN Repository
#' 
#' @param serverip serverip for repo (default: getOption("svn_serverip") )
#' @param root_dir root directory (default: "svn/kwb")
#' @return string to svn repo with login
#' @export

default_repo <- function(
  serverip = getOption("svn_serverip"),
  root_dir = "svn/kwb"
)
{
  sprintf("http://%s/%s",  
          serverip, 
          root_dir)
  
}

#' #' @param password password for repo (default: getOption("svn_password") )
#' #' @param serverip serverip for repo (default: getOption("svn_serverip") )
#' username = getOption("svn_username")
#' password = getOption("svn_password"),
#' --username %s --password 


#' Default SVN Repository RScripts
#' @export

default_rscripts <- function()
{
  default_repo(root_dir = "svn/kwb/R_Development/trunk/RScripts")
}

#' Default SVN Repository RScripts Paths
#'
#' @export

get_rscript_paths <- function()
{
  sprintf("svn ls -R %s", default_rscripts()) %>%
    shell(intern = TRUE) %>% 
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
#' @param logs if TRUE logs info is extracted, if FALSE size info (default:
#'   TRUE)
#' @param dbg debug messages (default: TRUE)
#' @return revision infos
#' @export
#' @importFrom kwb.utils catAndRun
getRevisionInfo <- function(
  revision, tDir = tempdir(), repo = default_repo(), logs = TRUE, dbg = TRUE
)
{
  if (logs) {
    
    recursive <- ""
    fPrefix <- paste0(fPrefix, "_log")
    sizeToGrep <- ""
    label <- "commit info"
    
  } else {
    
    recursive <- "--recursive"
    fPrefix <- "ente"
    sizeToGrep <- "| grep size"
    label <- "size"
  }
  
  target_path <- file.path(tDir, sprintf("%s_r%d.txt", fPrefix, revision))
  
  cmd <- sprintf(
    "svn list --xml %s -r%d %s %s > \"%s\"", 
    recursive, revision, repo, sizeToGrep, target_path
  )
  
  msg <- sprintf(
    "Get '%s' of r %d from repo %s ...",  label, revision, repo
  )
  
  kwb.utils::catAndRun(dbg = dbg, messageText = msg, expr = shell(cmd = cmd))

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
  currentRevision = 1920, startRevision = 2, tDir = tempdir(), 
  repo = default_repo(), logs = TRUE, dbg = TRUE
)
{
  for (i in  startRevision:currentRevision) { 
    
    getRevisionInfo(i, tDir = tDir, repo = repo, logs = logs, dbg = dbg)
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
  
  files <- dir(fDir, pattern = fPattern, full.names = TRUE)
  fileNames <- dir(fDir, pattern = fPattern, full.names = FALSE)
  revisions <- as.numeric(gsub(pattern = "ente_r|.txt", "", fileNames))
  
  for (id in seq_along(files)) {
    
    revision <- revisions[id]
    
    kwb.utils::catIf(dbg, sprintf("R %4d : ", revision))
    
    fileSize <- readLines(con = files[id]) 
    condition <- grep(pattern = "<size>|</size>", x = fileSize)
    fileSize <- fileSize[condition]
    
    sizes <- as.numeric(gsub("<size>|</size>", replacement = "", x = fileSize))
    
    ### convert from byte to  MB
    sizesInMB <- sizes / (1024^2)
    x$sizePerFile[[revision]] <- sizesInMB
    
    size  <- data.frame(
      revision = revision, 
      sumMB = sum(sizesInMB, na.rm = TRUE),
      maxMB = max(sizesInMB, na.rm = TRUE)
    )
    
    x$size <- plyr::rbind.fill(x$size, size)
    
    kwb.utils::catIf(dbg, sprintf(
      "%6.3f (MB) max: %6.3f (MB)\n", size$sumMB , size$maxMB
    ))
  }
  
  x$size <- x$size[order(x$size$revision), ]
  
  x
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
  
  fs::dir_create(target_dirs[! file.exists(target_dirs)], recurse = TRUE)
  
  sapply(file_paths, FUN = function(file_path) {  

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


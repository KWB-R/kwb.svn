# readLogFilesInDirectory ------------------------------------------------------
#' Read SVN Logfiles In A Directory
#'
#' @param logdir directory containing svn log information
#' @param pattern pattern (default: "_log_")
#' @return data frame with svn log data 
#' @export
readLogFilesInDirectory <- function(logdir, pattern = "_log_")
{
  logfiles <- dir(logdir, pattern = pattern, full.names = TRUE)
  
  loginfo <- lapply(logfiles, FUN = readLogFile)
  
  logdata <- do.call(rbind, args = loginfo)
  
  logdata$dateTimeUTC <- as.POSIXct(
    substr(logdata$date, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
  )
  
  row.names(logdata) <- NULL
  
  logdata[order(logdata$dateTimeUTC), ]
}

# readLogFile ------------------------------------------------------------------
#' Read Logfile
#'
#' @param logfile logfile
#' @return logfile
#' @importFrom  kwb.utils safeRowBind
#' @importFrom XML xmlTreeParse xmlChildren
#' @export

readLogFile <- function(logfile)
{
  cat("Reading", basename(logfile), "... ")
  
  xmllines <- readLines(logfile)
  
  indices <- c(grep("^<\\?xml", xmllines), length(xmllines) + 1)
  
  xmls <- list()
  
  for (i in seq_len(length(indices) - 1)) {
    xmls[[i]] <- xmllines[seq(indices[i], indices[i+1] - 1)]
  }
  
  xmltexts <- sapply(xmls, function(x) paste(x, collapse = "\n"))
  
  xmltexts <- xmltexts[!duplicated(xmltexts)]
  
  commitInfo <- NULL
  
  for (xmltext in xmltexts) {
    
    xmltree <- XML::xmlTreeParse(xmltext, asText = TRUE)
    
    commits <- XML::xmlChildren(xmltree$doc$children$lists)
    
    numberOfCommits <- length(commits)  
    
    commitInfo <- kwb.utils::safeRowBind(commitInfo, extractCommitInfo(commits))
  }
  
  cat("ok.\n")
  
  commitInfo  
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

default_repo <- function(root_dir = "svn/kwb", 
                         username = getOption("svn_username"), 
                         password = getOption("svn_password"),
                         serverip = getOption("svn_serverip")) {
  
 sprintf("http://%s:%s@%s/%s",  username, password, serverip, root_dir) 


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
getRevisionInfo <- function (revision,
                             tDir = tempdir(),
                             repo = default_repo(), 
                             logs = TRUE,
                             ### if TRUE logs info is extracted, if FALSE size info
                             dbg = TRUE
) {
  
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
  
  
  cmd <- sprintf("svn list --xml %s -r%d %s %s > \"%s\"", 
                 recursive,
                 revision,
                 repo, 
                 sizeToGrep, 
                 tPath)
  
  msg <- sprintf("Get '%s' of r %d from repo %s ...",  
                 label, 
                 revision, 
                 repo)
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
getRepoInfo <- function (currentRevision = 1920, 
                         startRevision = 2, 
                         tDir = tempdir(),
                         repo = default_repo(), 
                         logs = TRUE, 
                         dbg = TRUE) {
  
  for (i in  startRevision:currentRevision) { 
    getRevisionInfo(revision = i, 
                    tDir = tDir, 
                    repo = repo,
                    logs = logs, 
                    dbg = dbg)
    
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
readSizeFiles <- function (fDir,
                           fPattern = "ente_r", 
                           dbg = FALSE) {
  
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

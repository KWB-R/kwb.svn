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


kwb.pkgbuild::use_pkg(pkg = list(name = "kwb.svn", 
                                 title = "Helper Functions for Migration from Svn to Git" , 
                                 desc = "Helper Functions for Migration from Svn to Git."))


### Make the repo "public" before using "autopkgdown"
kwb.pkgbuild::use_autopkgdown("kwb.svn")
kwb.pkgbuild::add_gitlabci_to_ghpages("kwb.svn",dest_dir = "temp")

library(pkgbuild)
rt_path = gsub("\\","/",pkgbuild::rtools_path(),fixed=T)
rt_bin = paste0(substr(rt_path,1,nchar(rt_path)-4),"/mingw_$(WIN)/bin/")
writeLines(paste0('PATH="',rt_path,';${PATH}"'), con = "~/.Renviron")
writeLines(paste0('Sys.setenv(BINPREF = "',rt_bin,'")'), con = "~/.Rprofile")

install.packages("jsonlite",type="source")

Sys.getenv("BINPREF")
readLines("~/.Rprofile")
file.path(Sys.getenv("HOME"), ".Rprofile")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
writeLines('PATH="C:\\rtools40\\usr\\bin;${PATH}"', con = "~/.Renviron")

#writeLines('PATH="${rtools40}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make")

install.packages("jsonlite", type = "source")


Sys.getenv("PATH")
file.exists("C:\\Rtools\\bin\\make.exe")

file.exists("C:\\rtools40\\usr\\bin\\make.exe")
writeLines('PATH="C:\\rtools40\\usr\\bin;${PATH}"', con = "~/.Renviron")

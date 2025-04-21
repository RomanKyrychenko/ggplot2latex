#.onLoad <- function(libname, pkgname) {
#  if (!tinytex::is_tinytex()) {
#    message("Installing TinyTeX...")
#    tinytex::install_tinytex()
#  }
#  
#  # Check if TikZ is installed
#  tikz_installed <- "pgf" %in% tinytex::tlmgr_search("pgf")
#  
#  if (!tikz_installed) {
#    message("Installing TikZ (pgf package)...")
#    tinytex::tlmgr_install("pgf")
#  }
#}
#
#.onAttach <- function(libname, pkgname) {
#  if (!tinytex::is_tinytex()) {
#    packageStartupMessage("TinyTeX is not installed. Run tinytex::install_tinytex() to install it.")
#  }
#  
#  # Check if TikZ is installed
#  tikz_installed <- "pgf" %in% tinytex::tlmgr_search("pgf")
#  
#  if (!tikz_installed) {
#    packageStartupMessage("TikZ (pgf package) is missing. Install it with: tinytex::tlmgr_install('pgf')")
#  }
#}
#
LoadFavouriteLibraries <- function(extra.libraries = NULL){
  # Loads favourite libraries, i.e. ISLR, dplyr, ggplot, xtable & MASS.
  # Args:
  #   extra.libraries: extra libraries besides the favourite ones.
  fav.libraries <- c("ISLR","dplyr","ggplot2","xtable","MASS")
  libraries <- append(fav.libraries, extra.libraries)
  # create vector with library names
  for (current.library in libraries){
    load.error <- try(require(current.library, character.only = TRUE))
    # error handling try to install package
    if (!load.error){
      cat(paste("Trying installing ", current.library, " package.",sep = ""))
      install.packages(current.library, quiet = TRUE)
    }
  }
}

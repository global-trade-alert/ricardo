# CLEAN MENU NAMES FUNCTION FOR URL AND BACKEND
clean <- function(x) {
  return(tolower(gsub(" |_|-|#|!|?|,","",x)))
}
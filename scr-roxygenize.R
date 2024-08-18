#
# *roxygenize*
# apply as needed.
#

if(!require(roxygen2)) install.packages("roxygen2", repos = "https://cloud.r-project.org")
library(roxygen2)
roxygenize(package.dir = "src/", roclets = NULL, load_code = NULL, clean = FALSE)

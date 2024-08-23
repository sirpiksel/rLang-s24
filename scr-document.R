#
# *document*
# apply as needed.
#

library(devtools)
library(roxygen2)
build_vignettes(pkg = "src/")
roxygenize(package.dir = "src/", roclets = NULL, load_code = NULL, clean = FALSE)

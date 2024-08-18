#
# *package auto formatting*
# apply as needed.
#

if(!require(styler)) install.packages("styler", repos = "https://cloud.r-project.org")
library(styler)
styler::style_pkg("src/")

#
# *package auto formatting*
# apply as needed.
#

install.packages('styler', repos = 'https://cloud.r-project.org', quiet = TRUE)
library(styler)
styler::style_pkg('src/')

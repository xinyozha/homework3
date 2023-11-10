# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
install.packages("devtools")
install.packages("roxygen2")
library(roxygen2)
library(devtools)
devtools::document()
usethis::use_rcpp()
usethis::use_vignette("my-vignette")

library(usethis)
create_package("path/to/MyLinearModels")
library(simulatorR)

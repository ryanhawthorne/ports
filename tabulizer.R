install.packages("remotes")
install.packages("rJava")
install.packages("tabulizer")

install.packages("curl")
install.packages("xml2")
install.packages("openssl")

library(rJava)
remotes::install_github("ropensci/tabulizer")

devtools::install_github("ropensci/tabulapdf")

devtools::install_github("ropensci/tabulizer")
library(tabulapdf)


install.packages("tabulapdf", dependencies = TRUE)

library(tabulizer)

remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-24/")


library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Cyclic-models")

setwd("D:/R Git projects/Cyclic-models")


devtools::create("cyclic")

setwd("./cyclic")

devtools::document()

devtools::build_vignettes(pkg = "cyclic") 
devtools::use_data(smokedat, cyclic)
devtools::use_data(pdat, cyclic)
devtools::use_package("lme4") 

setwd("..")
devtools::install("cyclic")



library(cyclicESM)


library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)


setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Cyclic-models")

setwd("D:/R Git projects/Cyclic-models")


create("cyclicESM")

setwd("./cyclicESM")
document()

devtools::build_vignettes(pkg = "cyclicESM") 
devtools::use_data(smokedat, cyclicESM)
devtools::use_package("lme4") 

setwd("..")
install("cyclicESM")



library(cyclicESM)


library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)


setwd("~/Documents/Open Universiteit/Onderzoek/Methodologie/Cyclic-models")

setwd("D:/R Git projects/Cyclic-models")


create("cyclicESM")

setwd("./cyclicESM")
document()

setwd("..")
install("cyclicESM")



library(cyclicESM)

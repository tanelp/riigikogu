cabinets = read.table("data-raw/cabinets.txt", header=TRUE, sep="\t", as.is = T)
devtools::use_data(cabinets, overwrite = TRUE)

factions = read.csv("data-raw/factions.csv", header=TRUE, as.is = T)
devtools::use_data(factions, overwrite = TRUE)

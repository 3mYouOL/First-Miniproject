library(plyr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    csv_files <- list.files(path = directory)[id]
    csv_read <- vector(mode = "list")

    for (csv_file in csv_files) {
       csv_read <- append(csv_read, list(read.csv(csv_file)))
    }

    csv_cumulative <- rbind.fill(csv_read)

    mean(csv_cumulative[, pollutant], na.rm = TRUE)
}
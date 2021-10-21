library(plyr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    csv_files <- list.files(path = directory)[id]
    csv_read <- vector(mode = "list")

    for (csv_file in csv_files) {
       csv_read <- append(csv_read, list(read.csv(paste(directory, "/", csv_file, sep=""))))
    }

    csv_cumulative <- rbind.fill(csv_read)

    mean(csv_cumulative[, pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
    csv_files <- list.files(path = directory)[1:332]
    csv_cumulative <- c()

    for (csv_idx in id) {
       csv_clean <- na.omit(read.csv(paste(directory, "/", csv_files[csv_idx], sep="")))
       csv_cumulative[csv_idx] <- nrow(csv_clean)
    }

    clean_data <- data.frame(id = id, nobs = na.omit(csv_cumulative))
    clean_data
}

corr <- function(directory, threshold = 0) {
    csv_files <- list.files(path = directory)[1:332]
    csv_correlations <- vector(mode = "numeric")

    for (csv_file in csv_files) {
        csv_clean <- na.omit(read.csv(paste(directory, "/", csv_file, sep="")))

        if(nrow(csv_clean) > threshold) {
            csv_correlation <- cor(csv_clean[, "sulfate"], csv_clean[, "nitrate"])
            csv_correlations <- append(csv_correlations, csv_correlation)
        }
    }

    csv_correlations
}

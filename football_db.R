# Loading files
# First: sets the working directory
setwd("~/Documents/Ligue 1")

# Second: gets files list
files <- list.files(path = ".", full.names = TRUE, recursive = TRUE, pattern = "*.csv")

# Third: loads every file
data <- data.frame(read.csv(files[1]))
for (i in 2:length(files)) {
  print(i)
  data <- rbind(data, read.csv(files[i]))
}
# Fourth : eventually cleans results with no half time scores
data <- subset(x = data, is.na(data$HT) == FALSE)

## Adds full time result to data base
library(stringr)
Score <- str_split_fixed(paste(data$FT), "-", 2)
data<- cbind(data, Score[,1], Score[,2])
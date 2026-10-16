install.packages(c("httr2", "rvest", "jsonlite", "readr", "stringr"))

setwd("~/Desktop/Research")
source("extract_transcripts.R")

# Read the CSV, take first 5 rows, save as a test file
library(readr)
df <- read_csv("Jimmy Kimmel Live! - jk_episodes.csv")
write_csv(df[1:5, ], "test_5_episodes.csv")

# Run on the test file
process_csv("test_5_episodes.csv", "test_transcripts/")

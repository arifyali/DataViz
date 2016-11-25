#### Data Visualization for Policy Analysis
#### Alex Engler

## Converting Tabular Data into a HighCharts-Acceptable Format

bls <- read.csv("bls_jobs_wide.csv")
bls[is.na(bls)] <- "null" ## null is the equivalent of NA in javascript

# Toy Example:

example1 <- "data : ["
example1 <- paste0(example1, "5,", "7,", "10,", "12")
example1 <- paste0(example1, "]")

## Simple Example with data:
example2 <- "data: ["
example2 <- paste(example2, bls$Mining_Logging[1], ",", bls$Mining_Logging[2], ",", bls$Mining_Logging[3], ",", bls$Mining_Logging[4])
example2 <- paste0(example2, "]")


## For one entire variable:
example3 <- paste(bls$Mining_Logging, collapse=", ")
example3 <- paste0("data: [", example3, "]")


## Write reformatted data out to file:
fileConn<-file("output.txt")
writeLines(example3, fileConn)
close(fileConn)



## Complex Example with many columns (called 'Series' in HighCharts):
column_range <- 4:17 ## This is going to go through 14 columns

lines <- "[" 

for(i in column_range){
  
  ## for each column of data, add the column name after 'name'
  ## and add the data with the same paste function as before:
  lines <- paste0(lines, paste0("{name: '", colnames(bls)[i], "', data: [", paste(bls[,i], collapse=","), "]}"))
  
  ## Only add a column between series if it is not the last series:
  if(i != max(column_range)){lines <- paste0(lines, ",")}
}
lines <- paste0(lines, "]")


fileConn <- file("output_full.txt")
writeLines(lines, fileConn)
close(fileConn)


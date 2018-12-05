mergeResults <- function(csvdir = "C:/Users/mrustl/Desktop/LNDW_2015")
{
  csvfiles <- dir(csvdir, "^spiel.*\\.csv$", full.names = TRUE)
  
  contents <- lapply(csvfiles, read.csv)
  
  content <- do.call(rbind, contents)
  
  content <- content[!duplicated(content), ]
  
  write.csv(content, file.path("C:/Users/mrustl/Desktop/LNDW_results.csv"))
}

mergeResults()

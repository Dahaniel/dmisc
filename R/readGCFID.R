#' readGCFID
#'
#' Read data recorded with a Thermo GCFID. Data has to be manually
#' exported as .txt file (export chromatogram to the clipboard and pasted into a
#' .txt file)
#'
#' @param path the path that conatins the recordings (has to end in a trailing
#'   /)
#' @param animals a character vector containing the names of the recordings /
#'   animals
#'
#' @details \strong{animals} the elements need to exist as folder in 'path', the
#'   individual recordings need to be named 'animal\strong{_}odor\strong{-}concentration.txt'
#'
#' @return a data.frame
#' @export
#'

readGCFID <- function(path = "./", animals) {
  GC <- data.frame()

  for (animal in animals) {
    #animalx <- data.frame()
    pathx <- paste0(path, animal)
    nfile <- dir(pathx, pattern = "(txt)$", ignore.case = T)
    if (length(nfile) < 1 ) stop(paste("No *.txt files found @", pathx, "\n"))
    message(paste0("animal: ", animal, " - ", length(nfile)," files found."))

    for(measurement in nfile) {
      file          <- paste0(path, animal, "/", measurement)
      measx         <- read.table(file, skip = 4, header = TRUE)
      split         <- unlist(strsplit(measurement, "_|-|.txt"))
      measx$Tanimal <- animal
      measx$TOdour  <- split[5]
      measx$NOConc  <- paste0("-",split[6])
      measx$file    <- readLines(file)[2]
      GC            <- rbind(GC, measx)
    }
  }
  GC$time  <- GC$Time * 60 #time in seconds
  GC$value <- GC$Intensity
  GC <- GC[,-c(1:2)]
  return(GC)
}

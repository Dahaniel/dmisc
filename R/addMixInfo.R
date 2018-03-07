#' addMixInfo
#'
#'
#'
#' @param data.object
#' @param type
#' @param experiment
#' @param refs
#' @param ref.mol
#'
#' @return
#' @export
#'
#' @examples
addMixInfo <- function (    data.object,			# the object containing your data
                            type = "notRAW",      # data type, leave empty for mean ctv, enter "raw" for gloDatamix
                            experiment,			    # a dataframe containing the levels to plot, first columns contain reference and controls, nr defined in refs, last column contains the
                              refs = 3,				    # number of references
                              ref.mol         =   "MOL"
                            )
{

  #get data.object name
  data.object.name <- deparse(substitute(data.object))
  data.object.name <- gsub("[^a-zA-Z0-9 :._-]", "", data.object.name)

  if (type == "raw") names(data.object)[which(names(data.object) == "NGloTag")] <- "glomerulus"; names(data.object)[which(names(data.object) == "TOdour")] <- "odor"

  ### get odorants and references to plot
  cat(paste("\n!!! make sure to define correct number of references (set to >>>>> ",refs," <<<<< right now) !!!\n",sep=""))

  odorants <- toupper(names(experiment)[1:(length(experiment)-1)])
  references <- experiment[1,1:refs]
  odorants <- odorants[(refs+1):length(odorants)]
  mixtures <- toupper(experiment[["mixtures"]])
  glomeruli <- levels(factor(data.object$glomerulus))

  res <- data.frame()
  for (g in 1:length(glomeruli))
  {
    glomx <- subset(data.object, glomerulus==glomeruli[g])

      for (m in 1:length(mixtures))
      {
        components <- strsplit(as.character(mixtures[m]),"_")[[1]]
        A   <- subset(glomx, odor==paste(components[1],"_", ref.mol,sep=""));       A$role   <- factor("A")
        AA  <- subset(glomx, odor==paste(components[1],"_", components[1],sep="")); AA$role  <- factor("AA")
        B   <- subset(glomx, odor==paste(components[2],"_", ref.mol,sep=""));       B$role   <- factor("B")
        BB  <- subset(glomx, odor==paste(components[2],"_", components[2],sep="")); BB$role  <- factor("BB")
        MIX <- subset(glomx, odor==paste(components[1],"_", components[2],sep="")); MIX$role <- factor("MIX")
        MOL <- subset(glomx, odor==paste(ref.mol,"_", ref.mol,sep=""));             MOL$role   <- factor("MOL")

        mixture <- rbind(A,AA,B,BB,MIX,MOL)
        mixture$mixture <- mixtures[m]
        res <- rbind(res,mixture)
      }

  }
  if (type == "raw") names(data.object)[which(names(data.object) == "glomerulus")] <- "NGloTag"; names(data.object)[which(names(data.object) == "odor")] <- "TOdour"
  return(res)
}

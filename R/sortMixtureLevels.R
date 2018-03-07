#' sortMixtureLevels
#'
#'
#'
#' @param data.object
#' @param experiment
#'
#' @return
#' @export
#'
#' @examples
sortMixtureLevels <- function(  data.object, #data from gloDatamix Files
                                experiment #object containing mixture names
)
{
  experiment <- data.frame(lapply(experiment, as.character), stringsAsFactors=F) #convert to characters
  mixtures <- toupper(experiment$mixtures)
  odors <- toupper(cbind(experiment[1,-length(experiment)], experiment[2,4:(length(experiment)-1)]))
  for (i in 1:length(mixtures))
  {
    mix.i <- mixtures[i]
    pos.mix.i <- grep(mix.i,levels(data.object$TOdour), ignore.case=T)
    levels(data.object$TOdour)[pos.mix.i] <- mix.i

    rev.mix.i <- paste(substr(mix.i,6,9),"_",substr(mix.i,1,4),sep="")
    pos.rev.mix.i <- grep(rev.mix.i,levels(data.object$TOdour), ignore.case=T)
    levels(data.object$TOdour)[pos.rev.mix.i] <- mix.i
  }

  for (i in 1:length(odors))
  {
    odor.i <- odors[i]
    pos.odor.i <- grep(odor.i,levels(data.object$TOdour), ignore.case=T)
    levels(data.object$TOdour)[pos.odor.i] <- as.character(odor.i)
  }

  #data.object$TOdour <- toupper(data.object$TOdour)
  droplevels(data.object)

  return(data.object)
}


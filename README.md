## Chunk Indifferent Ranking Algorithm

### Description:

     Calculates an accuracy for a ranked list of binary, monotonic
     groupings, "chunks" for which an ordering of the groups is
     desired, but no order of the elelmens within the groups is
     specified.  For example, if doing a query to return a list of
     results, we may want the first N to be of a particular type, but
     we don't care about the ordering within those first N. We may next
     want a group of type G, followed by R, and for both we are
     indifferent about the rankings within the groups. An accuracy is
     returned for each group, and it is up to the user to decide which
     to use, weighting, etc.

### Usage:

     chunkRank(lookup, predicted)
     

### Install:

     library(devtools)
     install_github('chunkRank','vsoch')


### Arguments:

 - lookup: A vector of ints, with each int corresponding to the actual
          group assignment, and names(lookup) corresponding the unique
          id. This variable will be sorted to derive the "ideal" gold
          standard [eg, 1 1 1 1 2 2 2 2... N]

 - predicted: The predicted ordering, all labels in predicted must overlap
          with labels in chunk, and lengths must be equal

### Example:

     ### group assignment lookup [order doesn't matter, will be sorted increasing]
     lookup = c(1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,1)
     names(lookup) = paste("n1",seq(1,length(lookup)),sep="")
     
     ### The predicted order
     predicted = sample(names(lookup),length(lookup))
     results = chunkRank(lookup,ideal)


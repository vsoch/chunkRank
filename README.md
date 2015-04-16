## Chunk Indifferent Ranking Algorithm

### Description:

     Calculates an accuracy for a ranked list of binary, monotonic
     groupings, "chunks" for which an ordering of the groups is
     desired, but no order of the elements within the groups is
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

 - **lookup**: A vector of ints, with each int corresponding to the actual group 
          assignment, and names(lookup) corresponding the unique id. If gold standard 
          "ideal" is not provided, this variable will be sorted to derive the "ideal" 
          gold standard [eg, 1 1 1 1 2 2 2 2... N]

 - **predicted**: The predicted ordering, all labels in predicted must be present in 
              lookup, and lengths must be equal

 - **ideal**: The ideal gold standard ordering, increasing [eg, 1 1 1 1 2 2 2 2.. N]

 - **cutoff**: The cutoff position for giving partial accuracy. This can be the number of  
          images returned in the first pagination of an image search, the size of the 
          first group (no partial accuracy given), or the length of the list (partial
          accuracy given for all) [default = 100] Cases not included in the accuracy 
          calculation have distances returned as NA


### Value:

 - **accuracy**: a list of accuracy values for each group, indexed by group id

 - **table**: includes percent and absolute distances off, classification [correct/
          incorrect], predicted, and actual labels. Values of "NA" indicate element 
          was after cutoff, and not used in accuracy calculation.

 - **cutoff**: the cutoff for giving partial accuracy


### Example:

     ### group assignment lookup [order doesn't matter, will be sorted increasing]
     lookup = c(1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,1)
     names(lookup) = paste("n1",seq(1,length(lookup)),sep="")
     
     ### The predicted order
     predicted = sample(names(lookup),length(lookup))
     results = chunkRank(lookup,ideal)

#' Chunk Indifferent Ranking Algorithm
#'
#' Calculates an accuracy for a ranked list of binary, monotonic groupings, "chunks" for which an ordering of the groups is desired, but no order of the elelmens within the groups is specified.  For example, if doing a query to return a list of results, we may want the first N to be of a particular type, but we don't care about the ordering within those first N. We may next want a group of type G, followed by R, and for both we are indifferent about the rankings within the groups. An accuracy is returned for each group, and it is up to the user to decide which to use, weighting, etc.
#' @param lookup A vector of ints, with each int corresponding to the actual group assignment, and names(lookup) corresponding the unique id. This variable will be sorted to derive the "ideal" gold standard [eg, 1 1 1 1 2 2 2 2... N]
#' @param predicted The predicted ordering, all labels in predicted must overlap with labels in chunk, and lengths must be equal 
#' @keywords ranking, chunk indifferent ranking, sorting
#' @export
#' @examples
#' 
#' ### group assignment lookup [order doesn't matter, will be sorted increasing]
#' lookup = c(1,2,1,2,1,1,2,2,2,2,2,2,1,2,2,1)
#' names(lookup) = paste("n1",seq(1,length(lookup)),sep="") 
#' 
#' ### The predicted order
#' predicted = sample(names(lookup),length(lookup)) 
#' results = chunkRank(lookup,ideal)

chunkRank = function(lookup,predicted){
 
    # We will return results in a list
    results = list()
 
    # ideal is the sorted lookup
    ideal = as.numeric(sort(lookup,decreasing=FALSE))
 
    # Check that all predicted names are in lookup
    if (any(!predicted %in% names(lookup))) {
        missing = names(lookup)[-which(predicted %in% names(lookup))]
        stop(paste(missing,"are missing from the lookup."))
    }

    # Check that counts are the same for gold standard and lookup
    if (!all(as.numeric(table(chunks)) == as.numeric(table(lookup)))){
        stop("Counts and group identifiers of labels in chunks and lookup must be equivalent!")
    }
  
    # Unique groups in ordering
    groups = sort(as.numeric(unique(ideal)))
            
    # Keep a vector, in order of ideal, that says if we got it right
    correct = array(dim=length(ideal))
    names(correct) = predicted

    # Sort the lookup based on the predicted
    lookup = lookup[predicted]

    # Let's also save how far off we were (percentage and absolute distances)
    distances = array(dim=length(ideal))
    abs_distances = array(dim=length(ideal))
    names(distances) = predicted
    names(abs_distances) = predicted
    accuracies = list()
    
    # For each chunk
    for (group in groups){
      group_chunk = which(ideal==group)
      group_first = which(ideal==group)[1]
      group_last = which(ideal==group)[length(group_chunk)]

      ideal_subset = ideal[ideal==group]
      predicted_subset = lookup[group_first:group_last]

      # For each correct prediction, we give 1/N to accuracy
      accuracy_each =  1 / length(ideal_subset)
      accuracy = length(which(ideal_subset==predicted_subset)) * accuracy_each
      
      # Which names did we get right?
      corr = names(predicted_subset)[predicted_subset==group]
      if (length(corr)!=0){
        correct[corr] = "correct"
        distances[corr] = 0
        abs_distances[corr] = 0
        incorrect = names(predicted_subset)[predicted_subset!=group]
      } else {
        incorrect = names(predicted_subset)        
      }
      correct[incorrect] = "incorrect"
    
      # For the wrong predictions, we need to know how far off we were
      actual_indices = which(lookup==group)
      actual_indices = actual_indices[-which(names(actual_indices) %in% corr)]
      
      # Case 1: If we are at the first group, we will measure from the last position of the group 1 label
      # [1,1,1<--last ok position,2,2,2,2<--worst case]
      if (group==1){
        last_member = as.numeric(group_chunk[length(group_chunk)])
        # Calculate the errors, the number of places we were off for each group member
        errors = abs(actual_indices - last_member)
        maximum_distance_away = (length(predicted) - length(which(predicted_subset==group)))
        # We will calculate weights for the distance as a percentage of the length of the entire vector minus the group
        additional_error = errors / maximum_distance_away        
      }
      # Case 2: If we are at the last group, we will measure from the first index to the first position of the group label
      # [worst case-->1,1,1,last ok position-->2,2,2,2]      
      else if (group==length(groups)){
        first_index = 1
        first_member = as.numeric(group_chunk[1])
        # Calculate the errors, the number of places we were off for each group member
        errors = abs(first_member-actual_indices)
        # Give some portion of remaining accuracy based on where falls between first index
        # and first member (the worst scenario, if distance == first member, we give 0 accuracy)
        maximum_distance_away = first_member - first_index
        # Calculate weights as the actual distance (errors) as a percentage of the maximum distance away
        additional_error = errors / maximum_distance_away
        # Case 3: If we are at a middle group, we must measure distances in both directions (to end and front of list)
        # and depending on the direction of each incorrect, calculate distance in that direction. 
        # This approach makes the assumption that an error moving up in the list is equally bad to an error 
        # moving down in the list.
        # Given incorrect "2" grouped with 1: [worst case-->1,2,1,1,last ok position-->2,2,2,2,3,3,3,3]
        # Given incorrect "2" grouped with 3: [1,1,1,2,2,2,2<-- last ok position,3,3,2,3,3<--worst case]
      } else {
        first_index = 1
        first_member = as.numeric(group_chunk[1])
        last_member = as.numeric(group_chunk[length(group_chunk)])
        # Now we split the actual indices into two groups based on the direction
        up_in_list = actual_indices[actual_indices < first_member]
        down_in_list = actual_indices[actual_indices > last_member]
        # Calculate the errors, the number of places we were off for each group member
        errors_up = abs(first_member-up_in_list)
        errors_down = abs(last_member-down_in_list)
        errors = abs(c(errors_up,errors_down))
        # Give some portion of remaining accuracy based on distances away
        maximum_distance_away_up = abs(first_member - first_index)
        maximum_distance_away_down = abs(length(predicted) - last_member)
        # Calculate weights as the actual distance (errors) as a percentage of the maximum distance away
        additional_error_up = errors_up / maximum_distance_away_up
        additional_error_down = errors_down / maximum_distance_away_down
        additional_error = c(additional_error_up,additional_error_down)
      }
        distances[incorrect] = additional_error
        abs_distances[incorrect] = errors
        additional_accuracy_weights = 1-additional_error
        additional_accuracy = additional_accuracy_weights * accuracy_each
        accuracy = accuracy + sum(additional_accuracy)
        accuracies[group] = accuracy
    }
    # We will return a data frame ordered by the predicted names, so the ordering shows the predictions
    df = data.frame(row.names=names(predicted))
    df[names(distances),"perc_distances"] = distances
    df[names(correct),"classified"] = correct
    df[names(abs_distances),"abs_distances"] = abs_distances
    df[predicted,"predicted"] = lookup[predicted]
    df[names(lookup),"actual"] = lookup[predicted]
    results[["accuracy"]] = accuracies
    results[["table"]] = df
    results[["ideal"]] = ideal
    return(results)
}

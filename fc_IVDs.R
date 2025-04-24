#function for calculate IVDs based on selected method
IVD <- function(x, y, AlphaLevels, method = c("IALC", "NU", "NUW")) {
  method <- match.arg(method)

  if (method == "IALC") {
    results <- list()
    intervalSet <- AlphaCuts(x, y, AlphaLevels)
    aggregatedInterval <- IALC(intervalSet)
    results[["interval1"]] <- aggregatedInterval
    return(results)
    
  } else if (method %in% c("NU", "NUW")) {
    weight <- ifelse(method == "NUW", "alpha", "equal")
    
    # Calculate alpha cuts
    cuts <- AlphaCuts(x, y, AlphaLevels)
    
    # Find nested intervals from alpha cuts
    nested_intervals <- FindNestedIntervals(x, y, AlphaLevels)
    
    # Aggregate the intervals based on the specified weighting method
    aggregated_intervals <- NUW(nested_intervals, weight)
    
    # Calculate the union of the aggregated intervals
    unioned_intervals <- UnionIntervals(aggregated_intervals)
    
    return(unioned_intervals)
  } 
}

# Apply alpha-cuts to a fuzzy set (FS) defined by x and y at specified alpha levels. Used for all IVD methods
AlphaCuts <- function(x, y, AlphaLevels) {
  step <-x[2]-x[1]
  AlphaCut <- function(AlphaLevel) {
    if (AlphaLevel == 0) {
      # Special handling for alpha=0
      zero_points <- which(y > 0)  # Points where membership function is greater than 0
      if (length(zero_points) > 0) {
        return(list(c(max(min(x[zero_points])-step, min(x)), min(max(x[zero_points])+step,max(x)))))
      }
      return(NULL)
    }
    #Y <- ifelse(y >= AlphaLevel, AlphaLevel, 0)
    tolerance <- .Machine$double.eps^0.5
    Y <- ifelse(abs(y - AlphaLevel) < tolerance | y >= AlphaLevel, AlphaLevel, 0) #Newly modified to avoid issues like 0.5<0.5
    
    transitions <- which(diff(c(0, Y, 0)) != 0)
    starts <- transitions[c(TRUE, FALSE)]
    ends <- transitions[c(FALSE, TRUE)] - 1
    if (length(starts) == 0 || length(ends) == 0) return(NULL)
    pairs <- mapply(c, x[starts], x[ends], SIMPLIFY = FALSE)
    pairs
  }
  
  results <- lapply(AlphaLevels, AlphaCut)
  results <- results[!sapply(results, is.null)]
  names(results) <- as.character(AlphaLevels[!sapply(results, is.null)])
  results
}

### functions for Interval-valued Average Level Cuts (IALC)
IALC <- function(intervalSet) {
  aggregatedByAlpha <- list()
  
  # Iterate through each alpha level in the interval set
  for (alpha in names(intervalSet)) {
    intervals <- intervalSet[[alpha]]
    sumStarts <- 0
    sumEnds <- 0
    totalLength <- 0
    
    # Calculate total length of all intervals to establish weights
    for (interval in intervals) {
      if (!is.null(interval) && !any(is.na(interval))) {
        intervalLength <- interval[2] - interval[1]
        totalLength <- totalLength + intervalLength
      }
    }
    
    # Aggregate intervals within the same alpha level using weights based on interval length
    if (totalLength > 0) {
      for (interval in intervals) {
        if (!is.null(interval) && !any(is.na(interval))) {
          intervalLength <- interval[2] - interval[1]
          weight <- intervalLength / totalLength
          sumStarts <- sumStarts + interval[1] * weight
          sumEnds <- sumEnds + interval[2] * weight
        }
      }
      aggregatedByAlpha[[alpha]] <- c(sumStarts, sumEnds)
    } else {
      aggregatedByAlpha[[alpha]] <- NULL
    }
  }
  
  # Aggregate across different alpha levels
  sumStarts <- 0
  sumEnds <- 0
  countStarts <- 0
  countEnds <- 0
  for (aggregatedInterval in aggregatedByAlpha) {
    if (!is.null(aggregatedInterval)) {
      sumStarts <- sumStarts + aggregatedInterval[1]
      countStarts <- countStarts + 1
      sumEnds <- sumEnds + aggregatedInterval[2]
      countEnds <- countEnds + 1
    }
  }
  
  # Compute the final aggregated interval
  avgStart <- if (countStarts > 0) sumStarts / countStarts else NA
  avgEnd <- if (countEnds > 0) sumEnds / countEnds else NA
  
  # Return the final aggregated interval, handling cases where no valid intervals were found
  if (is.na(avgStart) || is.na(avgEnd)) {
    return(NULL)
  } else {
    return(c(avgStart, avgEnd))
  }
}

### functions for Nested, Union-based (and Weighted) (NU(W)) IVD
# Identify and organize nested interval sets from alpha-cuts based on alpha levels
FindNestedIntervals <- function(x, y, AlphaLevels ) {
  
  cuts <- AlphaCuts(x, y, AlphaLevels )
  
  nestedSets <- list()
  
  isNested <- function(interval, parent) {
    if (is.null(interval) || is.null(parent)) {
      return(FALSE)
    }
    if (any(is.na(interval)) || any(is.na(parent))) {
      return(FALSE)
    }
    return(interval[1] >= parent[1] && interval[2] <= parent[2])
  }
  
  if (length(cuts) > 0 && !is.null(cuts[[1]])) {
    for (i in 1:length(cuts[[1]])) {
      nestedSets[[i]] <- list(list(interval = cuts[[1]][[i]], alpha = AlphaLevels[1]))
    }
  }
  
  for (level in 2:length(AlphaLevels)) {
    if (is.null(cuts[[level]])) next
    currentCuts <- cuts[[level]]
    previousSets <- nestedSets
    nestedSets <- list()
    setId <- 1
    
    for (prevSet in previousSets) {
      lastIntervalSet <- tail(prevSet, 1)[[1]]
      lastInterval <- lastIntervalSet$interval
      foundNested <- FALSE
      for (interval in currentCuts) {
        if (isNested(interval, lastInterval)) {
          foundNested <- TRUE
          newSet <- c(prevSet, list(list(interval = interval, alpha = AlphaLevels[level])))
          nestedSets[[setId]] <- newSet
          setId <- setId + 1
        }
      }
      if (!foundNested) {
        nestedSets[[setId]] <- prevSet
        setId <- setId + 1
      }
    }
  }
  
  return(nestedSets)
}

# Aggregate nested intervals into single intervals using either alpha or equal weights
NUW <- function(nestedSets, weight = "alpha") {
  aggregatedSets <- list()
  
  # Calculate weighted average for interval starts or ends
  weightedAverage <- function(intervals, weights) {
    sum(weights * intervals) / sum(weights)
  }
  
  # Process each set of nested intervals
  for (set in nestedSets) {
    weights <- if (weight == "alpha") sapply(set, function(s) s$alpha) else rep(1, length(set))
    
    starts <- sapply(set, function(s) s$interval[1])
    ends <- sapply(set, function(s) s$interval[2])
    
    weightedStart <- weightedAverage(starts, weights)
    weightedEnd <- weightedAverage(ends, weights)
    
    aggregatedSets[[length(aggregatedSets) + 1]] <- c(weightedStart, weightedEnd)
  }
  
  aggregatedSets
}


UnionIntervals <- function(intervals) {
  # Sort intervals by their start points
  intervals <- intervals[order(sapply(intervals, `[`, 1))]
  
  # Initialize the list to store the union of intervals
  unioned_intervals <- list()
  
  # Start with the first interval
  current_start <- intervals[[1]][1]
  current_end <- intervals[[1]][2]
  
  # Iterate through each interval and merge if necessary
  for (interval in intervals) {
    if (interval[1] <= current_end) {  # Check if the interval overlaps or is contiguous
      # Extend the current interval if necessary
      current_end <- max(current_end, interval[2])
    } else {
      # Save the finished interval and start a new one
      unioned_intervals[[paste0("$interval", length(unioned_intervals) + 1)]] <- c(current_start, current_end)
      current_start <- interval[1]
      current_end <- interval[2]
    }
  }
  
  # Add the last processed interval to the list
  unioned_intervals[[paste0("interval", length(unioned_intervals) + 1)]] <- c(current_start, current_end)
  
  # Return the list of unioned intervals
  unioned_intervals
}





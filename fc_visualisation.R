# Function to plot valley points over the membership function plot
troughPlot <- function(x, y, plot) {
  
  trough_df <- find_trough_points(x, y, xonly = FALSE)
  
  if (nrow(trough_df) == 0) {
    warning("No trough points to plot.")
    return(plot)
  }
  
  plot + geom_point(
    data = trough_df,
    aes(x = x, y = y),
    color = "red",
    size = 1.5,
    shape = 21,
    stroke = 1
  )
}


ConvexPlot<- function(plot, subsets) {
  # Get the number of subsets (assuming each segment has an x and y, hence /2)
  num_subsets <- length(subsets) / 2
  
  # Generate a color palette with distinct colors for each segment
  #colors <- paletteer_d("ggsci::nrc_npg") # You can customize the color palette as needed
  #colors <- paletteer_d("ggsci::default_jco")
  colors <- paletteer_d("ggsci::springfield_simpsons")
  # Loop through each segment and add it to the plot
  for (i in 1:num_subsets) {
    x_key <- paste0("x", i)
    y_key <- paste0("y", i)
    
    # Ensure keys exist in the subsets list to avoid errors
    if (!x_key %in% names(subsets) || !y_key %in% names(subsets)) {
      next  # Skip if the segment does not exist
    }
    
    # Add the segment to the plot
    plot <- plot + geom_line(data = data.frame(x = subsets[[x_key]], y = subsets[[y_key]]),
                             aes(x = x, y = y), color = colors[i])
  }
  
  return(plot)
}


#Add discontinuous interval valued defuzzificaiton result to current plot.
IVDPlot <- function(plot, allResults, y_axis=0) {
  # Create a dataframe from the intervals in allResults
  intervals_df <- data.frame(
    x_start = sapply(allResults, `[`, 1),  # Extract the start of each interval
    x_end = sapply(allResults, `[`, 2),    # Extract the end of each interval
    y = rep(0, length(allResults))         # Set y to 0 for all intervals
  )
  
  # Add an identifier for each interval, optional
  intervals_df$interval_id <- paste("Interval", seq_along(allResults))
  
  # Iterate over each row in the dataframe to determine how to plot
  for (i in 1:nrow(intervals_df)) {
    if (intervals_df$x_start[i] == intervals_df$x_end[i]) {
      # Add a point instead of a segment when start and end are the same
      plot <- plot + geom_point(data = data.frame(x = intervals_df$x_start[i], y = y_axis),
                                aes(x = x, y = y), color = "#e74c3c", size = 1, shape = 15)
    } else {
      # Add the segment to the plot
      plot <- plot + geom_segment(data = data.frame(x = intervals_df$x_start[i], y = y_axis, 
                                                    xend = intervals_df$x_end[i], yend = y_axis),
                                  aes(x = x, y = y, xend = xend, yend = yend),
                                  color = "#e74c3c", linewidth = 1)
    }
  }
  
  # Return the updated plot
  return(plot)
}


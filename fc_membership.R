# Define the triangular membership function with height parameter
triangleMF <- function(start, peak, end, x, height = 1) {
  rise <- (x >= start) & (x <= peak)
  fall <- (x > peak) & (x <= end)
  y <- ifelse(rise, (x - start) / (peak - start) * height, 
              ifelse(fall, (end - x) / (end - peak) * height, 0))
  return(y)
}

# Define the uniform membership function with height parameter
uniformMF <- function(start, end, x, height = 1) {
  y <- ifelse(x >= start & x <= end, height, 0)
  return(y)
}

# Define the trapezoidal membership function with height parameter
trapezoidalMF <- function(a, b, c, d, x, height = 1) {
  rise <- (x >= a) & (x <= b)
  top <- (x > b) & (x < c)
  fall <- (x >= c) & (x <= d)
  
  y <- ifelse(rise, (x - a) / (b - a) * height, 
              ifelse(top, height, 
                     ifelse(fall, (d - x) / (d - c) * height, 0)))
  return(y)
}

IAA <- function(data, domain, step = 0.001) {
  # Define the step size for the interval calculation
  
  # Calculate the minimum and maximum values for the responses within the data
  min_value <- min(data$Min)
  max_value <- max(data$Max)
  
  # Define the internal calculation boundaries to be more precise in interval calculations
  intervals <- seq(from = min_value - step, to = max_value + step, by = step)
  
  # Initialize an empty vector to store the agreement values
  agreements <- numeric(length(intervals))
  
  # Calculate the agreement for each data point in the internal intervals
  for (i in 1:length(intervals)) {
    count <- sum((data$Min <= intervals[i]) & (data$Max >= intervals[i]))
    agreements[i] <- count
  }
  
  # Create the full range of x values based on the specified domain and step
  full_x_values <- seq(from = domain[1]- step, to = domain[2]+ step, by = step)
  
  # Create a full y_values vector initialized to zero
  full_y_values <- numeric(length(full_x_values))
  
  # Map the calculated agreements onto the full_x_values
  for (i in 1:length(full_x_values)) {
    if (full_x_values[i] >= min_value && full_x_values[i] <= max_value) {
      # Find the closest interval index (this assumes the intervals include this x value)
      idx <- which.min(abs(intervals - full_x_values[i]))
      full_y_values[i] <- agreements[idx] / nrow(data)
    }
  }
  
  # Create a dataframe for plotting
  FS <- data.frame(x = full_x_values, y = full_y_values)
  return(FS)
}


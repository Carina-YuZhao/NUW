# Main script - save as main.R
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(paletteer)
library(ggpubr)

# Load the required functions
source("fc_membership.R")
source("fc_visualisation.R")
source("fc_IVDs.R")
IALC
# Define the non-convex FS by x and y
step = 0.001
x <- seq(0, 10, by = step)

#G <- numeric(0)

x_1 <- c(0, 2, 4, 6, 8, 10)
y_1 <- c(0, 0, 1, 1, 0, 0)
y1  <- approx(x_1, y_1, x)$y

# Define original data points
x_2 <- c(0, 2, 4, 5, 6, 8, 10)
y_2 <- c(0, 0, 1, 0.75, 1, 0, 0)
y2  <- approx(x_2, y_2, x)$y

x_3 <- c(0, 2, 4, 5, 6, 8, 10)
y_3 <- c(0, 0, 1, 0.5, 1, 0, 0)
y3  <- approx(x_3, y_3, x)$y

x_4 <- c(0, 2, 4, 5, 6, 8, 10)
y_4 <- c(0, 0, 1, 0.25, 1, 0, 0)
y4  <- approx(x_4, y_4, x)$y

x_5 <- c(0, 2, 4, 4.001, 5.999, 6, 8, 10)
y_5 <- c(0, 0, 1, 0, 0, 1, 0, 0)
y5  <- approx(x_5, y_5, x)$y

x_6 <- c(0, 2, 4, 4.5, 5, 5.5, 6, 8, 10)
y_6 <- c(0, 0, 1, 0.75, 1, 0.75, 1, 0, 0)
y6  <- approx(x_6, y_6, x)$y

x_7 <- c(0, 2, 4, 4.5, 5, 5.5, 6, 8, 10)
y_7 <- c(0, 0, 1, 0.5, 1, 0.5, 1, 0, 0)
y7  <- approx(x_7, y_7, x)$y

x_8 <- c(0, 2, 4, 4.5, 5, 5.5, 6, 8, 10)
y_8 <- c(0, 0, 1, 0.25, 1, 0.25, 1, 0, 0)
y8  <- approx(x_8, y_8, x)$y

x_9 <- c(0, 2, 4, 4.35, 4.65, 5, 5.35, 5.65, 6, 8, 10)
y_9 <- c(0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0)
y9  <- approx(x_9, y_9, x)$y


# Define the alpha levels for alpha cut
AlphaLevels <- seq(0, 1, by = step)

# List to store plots
plots <- list()
#subsets <- list()

#set <- c(2,7)
# Loop through y1 to y7
for (i in 2:9) {
  y <- get(paste("y", i, sep=""))  # dynamically get y1 to y7
  
  # Plot original non-convex FS
  p <- ggplot(data.frame(x, y), aes(x = x, y = y)) +
    geom_line(color = '#3498db',size=1) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10), labels = NULL) +
    #geom_point(data = data.frame(x = centroid_x, y = 0), aes(x = x, y = y), shape = 16, color = "blue3", size = 2) +
    #xlab("x") + 
    ylab("μ(x)") +
    #ylab(NULL) + 
    xlab(NULL) +  
    theme_minimal() +
    theme(plot.margin = unit(c(0.1, 0.2, 0.1, 0.4), "cm")) +
    theme(axis.title.y = element_text(size=9))
  #theme(
  #  axis.title.x = element_text(color = "white"),
  #  axis.title.y = element_text(color = "white",size=13)
  #)
  
  p2 <- ggplot() +
    scale_y_continuous(breaks = c(1, 2, 3), labels = c("IALC", "UW", "NUW")) +  # Define breaks at 1 and 2 with labels A and B
    scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
    xlab(NULL) +
    ylab(NULL) +
    #labs(x = expression(d[2]), y = "") + 
    theme_minimal() +
    theme(axis.text = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 9))+
    theme(plot.margin = unit(c(0.1,0.2,0.1,0.45), "cm"))
  
  #####Apply IVDs and visualize them on p2

  ialc <- IVD(x, y, AlphaLevels, method = "IALC")
  p2 <- IVDPlot(p2, ialc, 1)
  
  uw <- list(interval1 = c(3.33333, 6.6666))
  p2 <- IVDPlot(p2, uw, 2)
  
  nuw <- IVD(x, y, AlphaLevels, method = "NUW")
  p2 <- IVDPlot(p2, nuw, 3)
  print(nuw)
  #duw <- IVD(x, y, AlphaLevels, method = "DUW")
  #p2 <- IVDPlot(p2, duw, 3)
  
  
  #du <- IVD(x, y, AlphaLevels, method = "DU")
  #p2 <- IVDPlot(p2, du, 4)
  
  p1 <- grid.arrange(
    p, p2, 
    ncol = 1,
    nrow = 2,
    heights = c(2, 1.8)  # Set the height of the first row larger than the second row
  )

  
  # Add plot to list
  plots[[i-1]] <- p1
  
  file_name <- paste0("Syn_", i-1, ".png")
  # Save the plot as PNG with specified background
   ggsave(file_name, plot = p1, device = "png", width = 610, height = 350, units = "px", bg = "white")
} 

# Combine all plots in a single view with 3 columns
combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow=4)

# Display the final combined plot
print(combined_plot)




# Create the vector with the specified values
# S <- c(0.25, 0.50, 0.75, 2, 0.13, 0.25, 0.38, 0.70)

# Print the rounded values
# print(S)

# print(G)

# S<- c(0.38, 0.70)
# Create R by dividing S by G element-wise
# R <- G / S

# R <- round(R, 2)

# Print R to see the results
# print(R)

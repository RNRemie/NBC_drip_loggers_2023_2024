#Summary:
#Read and import data from your Excel files using readxl.
#checks the data is properly formatted for a time series objects 
#Plot the results to visualize the cross-correlation patterns.
#Save plots output to a png file

#Step 1: Install Required Packages
# List of required packages
required_packages <- c("readxl", "fda", "ggplot2", "gridExtra", "cluster", 
                       "writexl", "GPArotation","conflicted" )

# Function to install packages if not already installed
install_if_needed <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Install and load required packages
for (pkg in required_packages) {
  install_if_needed(pkg)
} 

library(readxl)  # For reading Excel files
library(dplyr)   # For data manipulation
library(fda)
library(ggplot2) # For plotting
library(gridExtra) # For arranging multiple plots
library(cluster) # For clustering
library(writexl)  # For writing Excel files
library(GPArotation) # For PCA rotation
library(conflicted)

conflicts_prefer(stats::lag)
conflicts_prefer(stats::filter)

setwd("C:/Users/rnremie0411/Documents/Rdata")

#Step 2: Load and Inspect the Data from Excel
file_path <- "C:/Users/rnremie0411/Documents/Rdata/COMBINATIONJANMAY2024_cleanMay16NEW.xlsx"
data <- read_excel(file_path)

# Step 1: Inspect the first few rows of the data (assuming it's loaded into `data`)
head(data)
str(data)  # Check the structure of the 'data' object
summary(data)  # Get a summary of the data, including NAs and non-numeric columns

# Extract the date information and convert it to a Date or POSIXct object
dates <- as.POSIXct(data[[1]], format = "%Y-%m-%d %H:%M:%S")

# Ensure the Date column is in Date format
data$Date <- as.Date(data$Date)

# Check the date format
head(dates)

#convert all columns in data to numeric
data_numeric <- as.data.frame(lapply(data, as.numeric))

# Extract the time series data for the loggers (columns 2 to 21)
time_series_data <- data_numeric[, 2:21]

# Removes rows with NA values
data_clean <- na.omit(data[, 2:21])  

# Alternatively, impute missing values:
#data_clean <- apply(data[, 2:21], 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Convert back to a data frame
data_clean <- as.data.frame(data_clean)  

# Log-transform the data (log10(x + 1)) to reduce extremes in magnitude
log_transformed_data <- log10(time_series_data + 1)

# Step 2: Set Smoothing Bandwidth for Mean and Covariance
# Calculate the number of observations (rows) in the time series data
n_obs <- nrow(time_series_data)

# Calculate the number of observations corresponding to 2.5% and 5%
n_2_5_percent <- floor(0.025 * n_obs)
n_5_percent <- floor(0.05 * n_obs)

# Calculate the smoothing parameters (lambda) based on these percentages
lambda_mean <- n_2_5_percent / n_obs
lambda_cov <- n_5_percent / n_obs

# Print out the lambda values
cat("Smoothing for mean (lambda):", lambda_mean, "\n")
cat("Smoothing for covariance (lambda):", lambda_cov, "\n")

# Step 3: Create a Functional Data Object
# Create time points (assuming equally spaced time intervals, e.g., 15 minutes)
time_points <- seq(0, 1, length.out = n_obs)

# Define the number of basis functions for the smoothing
nbasis <- 10  # This is a starting point; experiment with this value

# Create the B-spline basis
basis <- create.bspline.basis(rangeval = c(0, 1), nbasis = nbasis)

# Smooth the log-transformed time series data for each logger using the lambda values
fd_data_list <- lapply(1:ncol(log_transformed_data), function(i) {
  # Create fdPar object with lambda for smoothing
  fd_par <- fdPar(basis, Lfd = 2, lambda = lambda_mean)  # Lfd = 2 refers to the second derivative penalty (smoothness)
  
  # Apply smoothing for each logger using the fdPar object
  smooth.basis(time_points, log_transformed_data[, i], fd_par)$fd
})

# Step 4: Combine the smoothed data into a matrix (fd_matrix)
# Number of loggers (i.e., the number of columns in log_transformed_data)
n_loggers <- length(fd_data_list)

# Extract the values for each fd object (i.e., the smoothed function values) and combine them into a matrix
fd_matrix <- sapply(fd_data_list, function(fd_obj) {
  # Extract the values (functional data object) for each logger
  return(eval.fd(time_points, fd_obj))
})

#
summary(fd_matrix)

# fd_matrix [dimension (n_obs x n_loggers)] with rows as time points and columns as loggers
# Convert fd_matrix into a functional data object
# Create a new basis for the combined data
combined_basis <- create.bspline.basis(rangeval = c(0, 1), nbasis = nbasis)

# Create a functional data object (fd) from the combined data matrix
combined_fd <- Data2fd(time_points, fd_matrix, combined_basis)

# Step 5: Perform FPCA on the combined functional data object (combined_fd)
fpca_result <- pca.fd(combined_fd, nharm = 5)

# Check the structure of fpca_result to see if `mu` exists
str(fpca_result)

dev.new()
# Set up a 1x2 grid for side-by-side plots
par(mfrow = c(1, 2))

# Step 6: Extract and plot the mean function (average drip rate across all loggers and time points)
if (is.null(fpca_result$mu)) {
  cat("Computing the mean function manually...\n")
  
  # Calculate mean function (across all loggers and time points)
  mean_function <- apply(fd_matrix, 1, mean)  # Apply mean across rows (time points)
  
  # Ensure the length of the extracted dates matches the length of the mean function
  if(length(dates) != length(mean_function)) {
    cat("Warning: Length of dates does not match length of mean_function. Adjusting...\n")
    # Trim or extend dates to match the length of mean_function
    dates <- dates[1:length(mean_function)]
  }
  
  # Plot manually computed mean function with Date on the x-axis
  plot(dates, mean_function, type = "l",  lwd = 2,
       main = "Computed Mean Function", 
       xlab = "Date", ylab = "Log (Mean Value)")
  
} else {
  cat("Plotting the mean function...\n")
  plot(fpca_result$mu, main = "Mean Function from FPCA", xlab = "Date")
}
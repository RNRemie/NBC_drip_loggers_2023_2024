#Summary:
#Read and import data from your Excel files using readxl.
#Convert the data into time series objects using xts or zoo.
#Shift rainfall data by 7,14,21,30,45 days and SWC data by 7,14,21,30,45, 60 days  (antecedent period).
#Compute CCF for SWC and each individual drip logger in the drip data time series time series 
#Compute CCF between the antecedent rainfall and the drip logger data.
#Plot the results to visualize the cross-correlation patterns.
#Save plots output to a png file


# 1. Install and Load Required Packages
# List of required packages
required_packages <- c("readxl", "zoo", "xts", "stats", "ggplot2", "gridExtra" ,"grid", "conflicted")

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

# Load the necessary package and libraries
library(readxl)  # For reading Excel files
library(zoo)     # For handling irregular time series and interpolation
library(xts)     # For time series data structure
library(stats)   # For ACF and CCF functions
library(ggplot2) # For plotting
library(grid)
library(gridExtra) # For arranging multiple plots
library(conflicted)
conflicts_prefer(stats::lag)


setwd("C:/Users/rnremie0411/Documents/Rdata")

# Step 2: Read and Import Data from the Excel File
data <- read_excel("C:/Users/rnremie0411/Documents/Rdata/Rainfall_MM_60_SWC_45_Loggers_daily_SUM.xlsx")

# Step 3: Prepare the Data (Convert Date column to Date type)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Convert columns to time series objects
rainfall_ts <- xts(data$Rainfall, order.by = data$Date)
soil_water_ts <- xts(data$SoilWaterContent, order.by = data$Date)
logger_data <- data[, 4:23]
logger_ts <- xts(logger_data, order.by = data$Date)

# Step 5: Create Functions for Lagging Data
# Function to generate shifted data for multiple antecedent periods
shift_rainfall <- function(rainfall_ts, days) {
  return(lag(rainfall_ts, k = -days))
}

shift_soil_water <- function(soil_water_ts, days) {
  return(lag(soil_water_ts, k = -days))
}

# Step 6: Create a List of Antecedent Periods
antecedent_periods <- c(7, 14, 21, 30, 45, 60)

# Create lists for shifted rainfall and soil water content for each period
rainfall_shifted_list <- lapply(antecedent_periods, function(x) shift_rainfall(rainfall_ts, x))
soil_water_shifted_list <- lapply(antecedent_periods, function(x) shift_soil_water(soil_water_ts, x))



compute_and_plot_ccf <- function(shifted_data_list, logger_ts, antecedent_periods, data_type) {
  ccf_results <- list()
  plot_list <- list()
  
  for (period_index in 1:length(antecedent_periods)) {
    plot_list[[period_index]] <- list()  # Initialize for each antecedent period
    shifted_data <- shifted_data_list[[period_index]]
    
    ccf_results[[period_index]] <- list()
    
    for (i in 1:ncol(logger_ts)) {
      logger_col <- logger_ts[, i]
      
      # Remove NAs from both shifted_data and logger_col
      valid_data <- complete.cases(shifted_data, logger_col)
      shifted_data_valid <- shifted_data[valid_data]
      logger_col_valid <- logger_col[valid_data]
      
      # Print number of valid data points
      #print(paste("Logger", i, "has", length(shifted_data_valid), "valid data points"))
      
      # Skip if there are not enough valid data points
      if (length(shifted_data_valid) < 10) {
        print("Not enough valid data points, skipping this combination.")
        next
      }
      
      # Compute CCF for the current logger
      ccf_result <- ccf(as.vector(shifted_data_valid), as.vector(logger_col_valid), lag.max = 21, lag.min = 0, plot = FALSE)
      
      # Ensure CCF results are valid
      if (length(ccf_result$acf) == 0 || length(ccf_result$lag) == 0) {
        print("Empty CCF result, skipping this logger.")
        next
      }
      
      # Prepare the ccf_data for plotting
      ccf_data <- data.frame(lag = ccf_result$lag, acf = ccf_result$acf)
      
      # Find the lag with the highest correlation
      max_lag <- ccf_data$lag[which.max(ccf_data$acf)]
      max_acf <- max(ccf_data$acf)
      
      # Confidence Interval at 95% level (example of a constant CI for demonstration)
      ci_upper <- 1.96 / sqrt(length(shifted_data_valid))
      ci_lower <- -1.96 / sqrt(length(shifted_data_valid))
      
      # Create the plot for the current logger
      ccf_plot <- ggplot(ccf_data, aes(x = lag, y = acf)) +
        geom_segment(aes(xend = lag, yend = 0), linewidth = 0.7, color = "black") +
        ggtitle(paste(data_type, "Logger", i, "(", antecedent_periods[period_index], "days)")) +
        theme_minimal() +
        labs(x = "Lag (days)", y = "Cross-Correlation") +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        geom_hline(yintercept = ci_upper, linetype = "dashed", color = "blue") +
        geom_hline(yintercept = ci_lower, linetype = "dashed", color = "blue") +
        # Use annotate instead of geom_point to avoid the warning and place the asterisk correctly
        annotate("point", x = max_lag, y = max_acf, color = "red", size = 4, shape = 8) +
        coord_cartesian(xlim = c(min(ccf_data$lag), max(ccf_data$lag)), ylim = c(min(ccf_data$acf) - 0.1, max(ccf_data$acf) + 0.1)) # Set fixed coordinate limits
      
      # Add the plot to the plot list for this period
      plot_list[[period_index]] <- c(plot_list[[period_index]], list(ccf_plot))
    }
    
    # Fill up to 20 plots (empty ones if necessary)
    while (length(plot_list[[period_index]]) < 20) {
      plot_list[[period_index]] <- c(plot_list[[period_index]], list(ggplot() + theme_void()))
    }
    
    # Arrange the plots in a grid
    combined_plot <- grid.arrange(
      grobs = plot_list[[period_index]],
      ncol = 5, nrow = 4  # 5 columns, 4 rows
    )
    
    # Save the plot to file
    ggsave(paste0("Asterix_", data_type, "_", antecedent_periods[period_index], "_days.png"),
           plot = combined_plot, width = 15, height = 12, units = "in")
  }
  
  return(ccf_results)
}

# Example: Step 9: Compute and Plot CCF Results for Antecedent Rainfall
ccf_rainfall_results <- compute_and_plot_ccf(rainfall_shifted_list, logger_ts, antecedent_periods, "Rainfall")

# Example: Step 10: Compute and Plot CCF Results for Antecedent Soil Water Content
ccf_soil_water_results <- compute_and_plot_ccf(soil_water_shifted_list, logger_ts, antecedent_periods, "SWC")

# 1. Install and Load Required Packages
required_packages <- c("readxl", "tidyverse", "cluster", "gridExtra", "ggplot2", 
                       "writexl", "dplyr", "conflicted", "lubridate", "dbscan", "sp", "factoextra")

# Install and load required packages
install_if_needed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load required packages
invisible(lapply(required_packages, install_if_needed))

conflicts_prefer(stats::lag)
conflicts_prefer(stats::filter)


# Set working directory
setwd("C:/Users/rnremie0411/Documents/Rdata")

# 2. Read in the Excel file
#data <- read_excel("C:/Users/rnremie0411/Documents/Rdata/COMBINATIONJANMAY2024_cleanMay16NEW.xlsx")

file_path <- "C:/Users/rnremie0411/Documents/Rdata/COMBINATIONJANMAY2024_cleanMay16NEW.xlsx"

data <- read_excel(file_path)

# Check structure of the data (first few rows, column names)
str(data)
head(data)

# Step 2: Check if the Date format is correct and convert it if needed
data$Date <- as.POSIXct(data$Date, format="%Y-%m-%d %H:%M:%S", tz="UTC") 

# Check for missing dates
if (any(is.na(data$Date))) {
  stop("Date format error: Some dates could not be parsed correctly.")
}

# 4. Handle Missing Values (using column-wise mean replacement)
data[2:21] <- apply(data[2:21], 2, function(x) ifelse(is.na(x) | is.nan(x), mean(x, na.rm = TRUE), x))

# 5. Calculate Average Drip Rate
average_drip_rate <- colMeans(data[2:21], na.rm = TRUE)


# 6. MDS & Clustering Function with Options for Different Methods
perform_mds_clustering <- function(data, k_clusters, method = "kmeans") {
  # Calculate correlation matrix and perform MDS
  correlation_matrix <- cor(data, method = "pearson", use = "complete.obs")
  mds_result <- cmdscale(1 - correlation_matrix, k = 2)
  
  # Perform clustering based on selected method
  if (method == "kmeans") {
    set.seed(123)
    
    # Set 4 clusters for kmeans (enforcing 4 clusters)
    kmeans_result <- kmeans(mds_result, centers = k_clusters, nstart = 25)
    result <- kmeans_result$cluster
   
    # Check if K-means produced the expected number of clusters
    print(table(kmeans_result$cluster))  # Check the distribution of clusters
    
  } 
  
  #perform hierarchical clustering using average linkage
  else if (method == "hierarchical") {
    distance_matrix <- dist(mds_result)
    hierarchical_result <- hclust(distance_matrix, method = "average")
    
#======= save dendrogram as PNG    
    
    # ✅ Save dendrogram to PNG
    #png("hierarchical_dendrogram3.png", width = 800, height = 1000)
    #png("hierarchical_dendrogram3.png", width = 1000, height = 800, res = 1000)
    plot(hierarchical_result,
         main = "Hierarchical Clustering Dendrogram",
         xlab = "",
         sub = "")
    #dev.off()
    #on.exit(dev.off())  # Ensures dev.off() is always called
   
  
    # Plot hierarchical clustering dendrogram
    plot(hierarchical_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")
 
    # Cut tree into 4 clusters for hierarchical clustering
    cut_result <- cutree(hierarchical_result, k = k_clusters)
    result <- cut_result
   
   } else {
     
    stop("Unknown clustering method.")
  }
  
#======= 
  
  # Create summary table
  logger_names <- colnames(data)
  cluster_labels <- paste("Cluster", result)
  summary_table <- data.frame(
    Logger = logger_names,
    Average_Drip_Rate = average_drip_rate,
    Cluster_Group = result,
    Cluster_Label = cluster_labels
  )
  
  return(list(mds_result = mds_result, summary_table = summary_table, cluster_result = result))
}

# 7. Plot MDS with Clusters
plot_mds_clusters <- function(mds_result, cluster_result, logger_names, title) {
  # Check for NULL or empty data before plotting
  if (is.null(mds_result) | is.null(cluster_result)) {
    stop("MDS result or cluster result is NULL, unable to plot.")
  }
  
  mds_df <- data.frame(x = mds_result[, 1], 
                       y = mds_result[, 2], 
                       cluster = factor(cluster_result), 
                       label = logger_names)
  
  # Ensure mds_df has valid numeric data
  if (any(is.na(mds_df$x)) | any(is.na(mds_df$y))) {
    stop("MDS result contains NA values, unable to plot.")
  }
  
  # Create the ggplot object
  mds_plot <- ggplot(mds_df, aes(x = x, y = y, color = cluster)) +
    geom_point(size = 3.5) +
    geom_text(aes(label = label), vjust = -1, size = 6) +
    labs(title = title, x = "MDS Dimension 1", y = "MDS Dimension 2") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(mds_plot)
}


#============= USING CUSTOM COLOURS =================

plot_mds_clusters <- function(mds_result, cluster_result, logger_names, title) {
  # Check for NULL or empty data before plotting
  if (is.null(mds_result) | is.null(cluster_result)) {
    stop("MDS result or cluster result is NULL, unable to plot.")
  }
  
  mds_df <- data.frame(x = mds_result[, 1], 
                       y = mds_result[, 2], 
                       cluster = factor(cluster_result), 
                       label = logger_names)
  
  # Ensure mds_df has valid numeric data
  if (any(is.na(mds_df$x)) | any(is.na(mds_df$y))) {
    stop("MDS result contains NA values, unable to plot.")
  }
  
  # Define a specific color palette for the clusters
  cluster_colors <- c("forestgreen","orange","red","purple")  # Customize as needed
  
  # Create the ggplot object with specified cluster colors
  mds_plot <- ggplot(mds_df, aes(x = x, y = y, color = cluster)) +
    geom_point(size = 3) +
    geom_text(aes(label = label), vjust = -1, size = 5) +
    labs(title = title, x = "MDS Dimension 1", y = "MDS Dimension 2") +
    scale_color_manual(values = cluster_colors) +  # Apply the custom color palette
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(mds_plot)
}


#===========

# 8. Perform Clustering and Plot Results for K-means and Hierarchical
methods <- c("kmeans", "hierarchical") # Available clustering methods
k_clusters <- 4  # Now explicitly set to 4 clusters

# Perform MDS and clustering for each method
results_list <- list()  # Initialize the results list properly
plots_list <- list()   # Initialize the plots list

for (method in methods) {
  cluster_result <- perform_mds_clustering(data[2:21], k_clusters, method)
  results_list[[method]] <- cluster_result
  
  # Check if valid result is returned before plotting
  if (!is.null(cluster_result$mds_result) & !is.null(cluster_result$cluster_result)) {
    mds_plot <- plot_mds_clusters(cluster_result$mds_result, cluster_result$cluster_result, colnames(data)[2:21], paste("MDS with", method, "clustering"))
    plots_list[[method]] <- mds_plot
  } else {
    warning(paste("Clustering for method", method, "produced no valid result."))
  }
}

#== display plots side by side ==
# # Display all plots if available
# if (length(plots_list) > 0) {
#   grid.arrange(grobs = plots_list, ncol = 2)
# } else {
#   warning("No valid plots to display.")
# }

# # ========= NEW SECTION ============
# 
#== display plots individually ==

if (length(plots_list) > 0) {
  for (p in plots_list) {
    print(p)
    # Optional: pause between plots so you can view each one
    Sys.sleep(5)  # Pause for 1 second (remove if not needed)
  }
} else {
  warning("No valid plots to display.")
}


# #== save each plot to png ==

if (length(plots_list) > 0) {
  for (i in seq_along(plots_list)) {
    ggsave(
      filename = paste0("plot_FigS4NEW_120", i, ".png"),
      plot = plots_list[[i]],
      width = 10, height = 8, dpi = 120
    )
  }
}


# #======= END OF SECTION ============

# Save plots (optional)
if ("kmeans" %in% names(plots_list)) {
  ggsave("mds_kmeans_plot.png", plots_list[["kmeans"]], width = 7, height = 6, units = "in")
}
if ("hierarchical" %in% names(plots_list)) {
  ggsave("mds_hierarchical_plot.png", plots_list[["hierarchical"]], width = 7, height = 6, units = "in")
}

# # 9. Save summary tables for each clustering result
if ("kmeans" %in% names(results_list)) {
  write_xlsx(results_list[["kmeans"]]$summary_table, "MDS_Kmeans_cluster_summary_table.xlsx")
}
if ("hierarchical" %in% names(results_list)) {
  write_xlsx(results_list[["hierarchical"]]$summary_table, "MDS_Hierarchical_cluster_summary_table.xlsx")
}

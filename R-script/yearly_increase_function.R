calculate_yearly_increase <- function(data, id_col, year_col, variable_cols) {
  # Make sure data is in the correct order
  data <- data[order(data[[id_col]], data[[year_col]]), ]
  
  # Get unique IDs
  ids <- unique(data[[id_col]])
  
  # Loop through each variable
  for (variable_col in variable_cols) {
    # Create an empty vector to store the results
    inc_vector <- numeric(nrow(data))
    
    # Loop through each ID
    for (id_value in ids) {
      # Subset the data for the current ID
      id_data <- data[data[[id_col]] == id_value, ]
      
      # Sort the data by year
      id_data <- id_data[order(id_data[[year_col]]), ]
      
      # Initialize a vector to store yearly increases
      inc_id <- numeric(length = nrow(id_data))
      
      # Loop through each year starting from the second year
      for (i in 2:nrow(id_data)) {
        # Calculate the yearly increase
        inc_id[i] <- id_data[[variable_col]][i] - id_data[[variable_col]][i - 1]
      }
      
      # Set the first year's increase to NA
      inc_id[1] <- NA
      
      # Assign the results to the correct rows in the vector
      inc_vector[data[[id_col]] == id_value] <- inc_id
    }
    
    # Create a column name for the new variable
    new_col_name <- paste(variable_col, "inc", sep = "_")
    
    # Add the results to the existing data frame
    data[[new_col_name]] <- inc_vector
  }
  
  # Return the modified data frame
  return(data)
}

# Example usage:
# Replace 'your_data_frame' with the actual name of your data frame
#test = c("ld_sub", "rd_wv.ol", "rd_wv.uc", "rd_wv.sc", "rd_wv.ss")
#your_data_frame <- 

booster <- function(data, year_condition, vars, increase, factor) {
  # Convert year_condition to character to avoid issues
  year_condition <- as.character(year_condition)
  
  # Print some debugging information
  #cat("Before update:\n")
  #print(data)
  
  # Condition to subset rows
  condition_rows <- data$y == year_condition
  
  # Print more debugging information
  #cat("Condition rows:\n")
  #print(condition_rows)
  
  #cat("Subset of data:\n")
  #print(data[condition_rows, ])
  
  # Loop through each pair of vars and increase
  for (i in seq_along(vars)) {
    var <- vars[i]
    inc <- increase[i]
    new_column <- paste(var, "X", sep = "")
    
    # Calculate new column value
    new_values <- data[condition_rows, var] + ifelse(data[condition_rows, inc] < 0, 0, data[condition_rows, inc]) * factor
    
    data[condition_rows, new_column] <- new_values
  }
  
  # Print the updated data
  #cat("After update:\n")
  #print(data)
  
  return(data)
}


calculate_yearly_increase_w_prc <- function(data, id_col, year_col, variable_cols) {
  # Make sure data is in the correct order
  data <- data[order(data[[id_col]], data[[year_col]]), ]
  
  # Get unique IDs
  ids <- unique(data[[id_col]])
  
  # Loop through each variable
  for (variable_col in variable_cols) {
    # Create an empty vector to store the results
    inc_vector <- numeric(nrow(data))
    
    # Create an empty vector to store the percentage changes
    prcnt_vector <- numeric(nrow(data))
    
    # Loop through each ID
    for (id_value in ids) {
      # Subset the data for the current ID
      id_data <- data[data[[id_col]] == id_value, ]
      
      # Sort the data by year
      id_data <- id_data[order(id_data[[year_col]]), ]
      
      # Initialize a vector to store yearly increases
      inc_id <- numeric(length = nrow(id_data))
      
      # Loop through each year starting from the second year
      for (i in 2:nrow(id_data)) {
        # Calculate the yearly increase
        inc_id[i] <- id_data[[variable_col]][i] - id_data[[variable_col]][i - 1]
        
        # Calculate the percentage change
        prcnt_id <- (id_data[[variable_col]][i] - id_data[[variable_col]][i - 1]) / id_data[[variable_col]][i - 1] * 100
        
        # Assign the percentage change to the correct rows in the vector
        prcnt_vector[data[[id_col]] == id_value] <- prcnt_id
      }
      
      # Set the first year's increase and percentage change to NA
      inc_id[1] <- NA
      prcnt_vector[data[[id_col]] == id_value][1] <- NA
      
      # Assign the results to the correct rows in the vectors
      inc_vector[data[[id_col]] == id_value] <- inc_id
    }
    
    # Create a column name for the new variable
    new_col_name <- paste(variable_col, "inc", sep = "_")
    
    # Add the yearly increase results to the existing data frame
    data[[new_col_name]] <- inc_vector
    
    # Create a column name for the percentage change variable
    prcnt_col_name <- paste(variable_col, "prc_ch", sep = "_")
    
    # Add the percentage change results to the existing data frame
    data[[prcnt_col_name]] <- prcnt_vector
  }
  
  # Return the modified data frame
  return(data)
}


calculate_yearly_increase_w_prc2 <- function(data, id_col, year_col, variable_cols) {
  # Make sure data is in the correct order
  data <- data[order(data[[id_col]], data[[year_col]]), ]
  
  # Get unique IDs
  ids <- unique(data[[id_col]])
  
  # Loop through each variable
  for (variable_col in variable_cols) {
    # Create an empty vector to store the results
    inc_vector <- numeric(nrow(data))
    prcnt_vector <- numeric(nrow(data))
    
    # Loop through each ID
    for (id_value in ids) {
      # Subset the data for the current ID
      id_data <- data[data[[id_col]] == id_value, ]
      
      # Calculate yearly increase
      yearly_increase <- diff(id_data[[variable_col]], lag = 1, differences = 1)
      
      # Calculate percentage change
      percentage_change <- c(NA, yearly_increase / head(id_data[[variable_col]], -1) * 100)
      
      # Assign results to the correct rows in vectors
      inc_vector[data[[id_col]] == id_value] <- c(NA, yearly_increase)
      prcnt_vector[data[[id_col]] == id_value] <- percentage_change
    }
    
    # Create new column names
    new_col_name <- paste(variable_col, "inc", sep = "_")
    prcnt_col_name <- paste(variable_col, "prc_ch", sep = "_")
    
    # Add results to the existing data frame
    data[[new_col_name]] <- inc_vector
    data[[prcnt_col_name]] <- prcnt_vector
  }
  
  # Return the modified data frame
  return(data)
}



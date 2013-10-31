# Get estimates of the PM10 and PM2.5 ratios using the fractions of TSP: 0.47 and 0.072 
pm10 <- test_data$TSP * 0.47
pm25 <- test_data$TSP * 0.072
testt_data$pm10 <- pm10
test_data$pm25 <- pm25



# Function for hourly percentiles
hourly.pm.stats <- function(data, year, tsp = NULL, pm10 = NULL, pm25 = NULL,
                            estimate.tsp = FALSE, estimate.pm10 = FALSE, estimate.pm25 = FALSE,
                            ratios = c(0.47, 0.072), percentiles = c(100, 99, 98, 95, 90, 75, 50)) {
  
  # Validate data input
  #
  # Are all of the time elements available?
  # In which columns are the time elements provided
  # How to account for different names?
  # Is it possible to detect which columns contain what based on creating time objects and testing
  # for fairly regular increments?
  #
  # Is there at least one PM data source identified?
  
  
  # if(tsp == NULL & pm10 == NULL & pm25 == NULL,
  #       break("At least one PM parameter must be identified in the data frame"), NULL)
  
  
  # Subset data
  data_year <- data[which(data$Year == year), ]
  
  # Determine how large to make the empty data frame
  number_of_percentiles <- length(percentiles)
  
  # Construct the data frame, provide column names
  hourly_percentiles <- as.data.frame(matrix(nrow = (number_of_percentiles), ncol = 6))
  colnames(hourly_percentiles) <- c("year", "tsp", "pm10", "pm25", "percentile", "type")
  
  # Generate vectors of percentiles for each of tsp, pm10, and pm25
  tsp_percentiles <- quantile(data_year$TSP, probs = percentiles/100, na.rm = TRUE)
  pm10_percentiles <- quantile(data_year$pm10, probs = percentiles/100, na.rm = TRUE)
  pm25_percentiles <- quantile(data_year$pm25, probs = percentiles/100, na.rm = TRUE)
  
  # Place data into data frame
  hourly_percentiles$year <- year
  hourly_percentiles$tsp <- tsp_percentiles
  hourly_percentiles$pm10 <- pm10_percentiles
  hourly_percentiles$pm25 <- pm25_percentiles
  hourly_percentiles$percentile <- percentiles
  hourly_percentiles$type <- "hourly"
  
  # Print the hourly stats data frame
  print(hourly_percentiles)
}

# try out the function for each year

hourly.pm.stats(data = revised_dust_data,
                tsp = "TSP", pm10 = "pm10", pm25 = "pm25", year = 2010)

hourly.pm.stats(data = revised_dust_data,
                tsp = "TSP", pm10 = "pm10", pm25 = "pm25", year = 2011)

hourly.pm.stats(data = revised_dust_data,
                tsp = "TSP", pm10 = "pm10", pm25 = "pm25", year = 2012)




# Function for sequential, daily percentiles
daily.seq.pm.stats <- function(data, tsp = NULL, pm10 = NULL, pm25 = NULL,
                               estimate.tsp = FALSE, estimate.pm10 = FALSE, estimate.pm25 = FALSE,
                               year, percentiles = c(100, 99, 98, 95, 90, 75, 50)) {
  #data <- dust_data_2010_2012
  #tsp <- "dust"  #character
  #pm10 <- "pm10" #character
  #pm25 <- "pm25" #character
  #year <- 2012   #numeric
  #percentiles <- c(100, 99, 98, 95, 90, 75, 50)  #numeric vector
  
  # Subset data
  data_daily_year <- data[which(data$year== year), ]
  
  # Determine how large to make the empty data frame
  number_of_percentiles <- length(percentiles)
  
  # Construct the data frame, provide column names
  daily_percentiles <- as.data.frame(matrix(nrow = (number_of_percentiles), ncol = 6))
  colnames(daily_percentiles) <- c("year", "tsp", "pm10", "pm25", "percentile", "type")
  
  # Attach factor of day of year for each record in subset
  
  timestamp <- ISOdatetime(data_daily_year$year,
                           data_daily_year$month,
                           data_daily_year$day,
                           data_daily_year$hour,
                           min = 0, sec = 0, tz = "")
  
  data_daily_year$timestamp <- timestamp
  data_daily_year$yday <- yday(data_daily_year$timestamp)
  
  # Initialize empty vectors for daily averages
  tsp_daily_mean <- as.numeric(matrix(nrow = max(data_daily_year$yday), ncol = 1))
  pm10_daily_mean <- as.numeric(matrix(nrow = max(data_daily_year$yday), ncol = 1)) 
  pm25_daily_mean <-  as.numeric(matrix(nrow = max(data_daily_year$yday), ncol = 1))
  
  # Generate daily averages for each of tsp, pm10, and pm25
  for (i in 1:max(data_daily_year$yday)) {
    data_day <- data_daily_year[which(data_daily_year$yday == i), ]
    tsp_daily_mean[i] <- mean(data_day$dust, na.omit = TRUE)
    pm10_daily_mean[i] <- mean(data_day$pm10, na.omit = TRUE)
    pm25_daily_mean[i] <- mean(data_day$pm25, na.omit = TRUE)
  }
  
  # Generate vectors of percentiles for each of tsp, pm10, and pm25
  tsp_percentiles <- quantile(tsp_daily_mean, probs = percentiles/100, na.rm = TRUE)
  pm10_percentiles <- quantile(pm10_daily_mean, probs = percentiles/100, na.rm = TRUE)
  pm25_percentiles <- quantile(pm25_daily_mean, probs = percentiles/100, na.rm = TRUE)
  
  # Place data into data frame
  daily_percentiles$year <- year
  daily_percentiles$tsp <- tsp_percentiles
  daily_percentiles$pm10 <- pm10_percentiles
  daily_percentiles$pm25 <- pm25_percentiles
  daily_percentiles$percentile <- percentiles
  daily_percentiles$type <- "daily, sequential"
  
  # Print the hourly stats data frame
  print(daily_percentiles)
  rm(pm10, pm25, tsp)
}

daily.seq.pm.stats(data = revised_dust_data,
                   tsp = "dust", pm10 = "pm10", pm25 = "pm25", year = 2010)

daily.seq.pm.stats(data = revised_dust_data,
                   tsp = "dust", pm10 = "pm10", pm25 = "pm25", year = 2011)

daily.seq.pm.stats(data = revised_dust_data,
                   tsp = "dust", pm10 = "pm10", pm25 = "pm25", year = 2012)


# Create function for annual geometric mean

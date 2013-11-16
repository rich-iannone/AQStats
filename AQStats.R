# Load in test data from the BC Air Quality Network
require(lubridate)
df <- read.csv("Abbotsford A Columbia Street-2011-2012-MON.csv", header = TRUE)
df$Date.Time <- parse_date_time(as.character(df$Date.Time), "%m%d%y %H%M")
df$Date.Time <- as.POSIXct(df$Date.Time, tz = "America/Vancouver")


# Rename rows to contain units; remove 1st row with unit labels
for (i in 2:(ncol(df))) {
  colnames(df)[i] <- paste(colnames(df)[i],".",gsub(" ","",as.character(df[1,i])), sep = "")
}
df <- df[-1, ]

# Function for hourly percentiles for particulate matter
hourly.pm.stats <- function(df, year, tsp = NULL, pm10 = NULL, pm25 = NULL,
                            estimate.tsp = FALSE, estimate.pm10 = FALSE, estimate.pm25 = FALSE,
                            ratios = c(0.47, 0.072), percentiles = c(100, 99, 98, 95, 90, 75, 50)) {
  
  year <- 2012
  df <- df
  
  tsp = NULL
  pm10 = "PM10.ug/m3"
  pm25 = "PM25.ug/m3"
  
  ratios = c(0.47, 0.072)
  percentiles = c(100, 99, 98, 95, 90, 75, 50)
  
  
  # If data frame is a vector, make it a data frame
  if (is.vector(df)) {
    df <- data.frame(x = df)
  }

  # Test if data has a PM/TSP column
  PM.col <- ifelse(!is.na(pmatch("PM", colnames(df))), pmatch("PM", colnames(df)), 0)
  TSP.col <- ifelse(!is.na(pmatch("TSP", colnames(df))), pmatch("TSP", colnames(df)), 0)
  pm.col <- ifelse(!is.na(pmatch("pm", colnames(df))), pmatch("pm", colnames(df)), 0)
  tsp.col <- ifelse(!is.na(pmatch("tsp", colnames(df))), pmatch("tsp", colnames(df)), 0)
  PM.cols <- c(PM.col, TSP.col, pm.col, tsp.col)
  any.PM.col <- sum(PM.cols) > 0
  
  # Test if data has a PM10 column
  PM10.col <- ifelse(!is.na(pmatch("PM10", colnames(df))), pmatch("PM10", colnames(df)), 0)
  pm10.col <- ifelse(!is.na(pmatch("pm10", colnames(df))), pmatch("pm10", colnames(df)), 0)
  PM10.cols <- c(PM10.col, pm10.col)
  any.PM10.col <- sum(PM10.cols) > 0
  
  # Test if data has a PM25 column
  PM25.col <- ifelse(!is.na(pmatch("PM25", colnames(df))), pmatch("PM25", colnames(df)), 0)
  pm25.col <- ifelse(!is.na(pmatch("pm25", colnames(df))), pmatch("pm25", colnames(df)), 0)
  PM25.cols <- c(PM25.col, pm25.col)
  any.PM25.col <- sum(PM25.cols) > 0
    
  # Detect if there is a column of class "POSIXct"
  posix_time <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    posix_time[i] <- ifelse(class((df)[,i])[1] == "POSIXct", 1, NA)
  }
  posix_col <- match(1, posix_time)
  
  # Detect if there is a column that contains year data
  year_col <- mat.or.vec(ncol(df),1)
  for (i in 1:(ncol(df))) {
    year_col[i] <- ifelse(mean(as.numeric(df[,3]), na.rm = TRUE) < (year(Sys.time()) + 1) &
                          mean(as.numeric(df[,3]), na.rm = TRUE) > 1950, 1, NA)
  }
  year_col <- match(1, year_col)
  
  if (!is.na(posix_col)) {
      # Create subset of data frame based on POSIX formatted objects and year selected 
    data_year <- df[which(year(df[,posix_col]) == year), ]
  } else if (!is.na(year_col)) {
    data_year <- df[which(year(df[,year_col]) == year), ]
  } else { break("Year information cannot be found in data frame") }
    
  # Determine how large to make the empty data frame
  number_of_percentiles <- length(percentiles)
  
  # Construct the data frame, provide column names
  hourly_percentiles <- as.data.frame(matrix(nrow = (number_of_percentiles), ncol = 6))
  colnames(hourly_percentiles) <- c("year", "percentile", "type", "pm", "pm10", "pm25")
  
  # Generate vectors of percentiles for each of tsp, pm10, and pm25
  if (any.PM.col == TRUE) {
  pm_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM.cols)])),
                             probs = percentiles/100, na.rm = TRUE)
  } else {
  pm_percentiles <- rep(NA, times = length(percentiles))
  }
  
  if (any.PM10.col == TRUE) {
  pm10_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM10.cols)])),
                               probs = percentiles/100, na.rm = TRUE)
  } else {
    pm10_percentiles <- rep(NA, times = length(percentiles))
  }
  
  if (any.PM25.col == TRUE) {
  pm25_percentiles <- quantile(as.numeric(as.character(data_year[,max(PM25.cols)])),
                               probs = percentiles/100, na.rm = TRUE)
  } else {
    pm25_percentiles <- rep(NA, times = length(percentiles))
  }
  
  # Place data into data frame
  hourly_percentiles$year <- year
  hourly_percentiles$percentile <- percentiles
  hourly_percentiles$type <- "hourly"
  hourly_percentiles$pm <- round(pm_percentiles, digits = 2)
  hourly_percentiles$pm10 <- round(pm10_percentiles, digits = 2)
  hourly_percentiles$pm25 <- round(pm25_percentiles, digits = 2)
  
  # Print the hourly stats data frame
  print(hourly_percentiles)
}


# Function for sequential, daily percentiles
daily.seq.pm.stats <- function(df, tsp = NULL, pm10 = NULL, pm25 = NULL,
                               estimate.tsp = FALSE, estimate.pm10 = FALSE, estimate.pm25 = FALSE,
                               year, percentiles = c(100, 99, 98, 95, 90, 75, 50)) {
  #data <- dust_data_2010_2012
  #tsp <- "dust"  #character
  #pm10 <- "pm10" #character
  #pm25 <- "pm25" #character
  #year <- 2012   #numeric
  #percentiles <- c(100, 99, 98, 95, 90, 75, 50)  #numeric vector
  
  # If data frame is a vector, make it a data frame
  if (is.vector(df)) {
    df <- data.frame(x = df)
  }
  
  # Subset data
  data_daily_year <- df[which(df$year== year), ]
  
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

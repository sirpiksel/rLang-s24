#' @title Weather Data: Average Annual Temperature in Germany
#' 
#' @description
#' This dataset contains the average annual temperatures in Germany from 1881 to
#' 2023. The data is provided both as a data frame (`tempDE_df`) and as a time
#' series object (`tempDE_ts`).
#' 
#' @usage tempDE_df
#' tempDE_ts
#' 
#' @format
#' `tempDE_df` is a data frame with 143 entries (rows) and 2 variables (columns)
#' named `Year` and `Temperature`. The `Year` column contains the respective
#' years, while the `Temperature` column gives the average annual temperature in
#' the whole of Germany.
#' 
#' `tempDE_ts` is a time series that shows the average annual temperature in
#' Germany for each year.
#' 
#' @name tempDE
#' 
#' @aliases tempDE_df tempDE_ts
#' 
#' @source [Deutscher Wetterdienst](https://www.dwd.de/)
#' 
#' @examples
#' # Display the first few rows of the data frame
#' head(tempDE_df)
#' 
#' # Plot the temperature
#' base::plot(tempDE_df$Year, tempDE_df$Temperature, type = "p", col = "blue",
#'     main = "Average Annual Temperature in Germany (1881-2023)",
#'     xlab = "Year", ylab = "Temperature (°C)")
#' lines(tempDE_df$Year, tempDE_df$Temperature, col = "red")
#' 
#' @keywords dataset
#' 
Year <- 1881:2023

Temperature <- as.numeric(c(
  7.3, 8.3, 7.9, 8.6, 7.7, 8.0, 7.0, 6.9, 7.4, 7.3, 7.4, 7.5, 7.9, 8.1, 7.3,
  7.6, 7.9, 8.5, 8.1, 8.4, 7.6, 7.2, 8.4, 8.4, 8.0, 8.3, 7.8, 7.5, 7.4, 8.4,
  9.0, 7.9, 8.5, 8.5, 7.9, 8.4, 7.5, 8.5, 7.3, 8.6, 9.0, 7.2, 8.0, 7.5, 8.3,
  8.7, 8.0, 8.3, 7.4, 8.8, 7.6, 8.3, 7.6, 9.5, 8.4, 8.4, 8.6, 8.6, 8.3, 6.6,
  7.2, 7.3, 8.9, 8.3, 9.0, 8.4, 8.5, 9.0, 9.1, 8.6, 8.7, 7.9, 8.9, 7.7, 7.5,
  6.8, 8.6, 8.2, 9.0, 8.4, 8.9, 7.1, 7.1, 8.1, 7.5, 8.5, 8.9, 8.1, 7.8, 7.7,
  8.4, 7.8, 8.2, 8.8, 8.9, 8.5, 8.7, 7.8, 7.7, 7.6, 8.2, 8.9, 9.0, 8.0, 7.4,
  7.9, 7.4, 9.1, 9.5, 9.5, 8.3, 9.4, 8.5, 9.7, 8.9, 7.2, 8.9, 9.1, 9.5, 9.9,
  9.0, 9.6, 9.4, 8.4, 9.0, 9.5, 9.9, 9.5, 9.2, 7.8, 9.6, 9.1, 8.7, 10.3, 9.9,
  9.5, 9.6, 10.5, 10.3, 10.4, 9.2, 10.5, 10.6
))


tempDE_df <- data.frame(Year = Year, Temperature = Temperature)

tempDE_ts <- ts(data = Temperature, Year)

usethis::use_data(tempDE_df, overwrite = TRUE)
usethis::use_data(tempDE_ts, overwrite = TRUE)

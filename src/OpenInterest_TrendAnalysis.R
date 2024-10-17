#持仓量长期趋势分析

# Define the years and the months of interest
years <- 2012:2024
months <- c(3, 6, 9, 12)  # March, June, September, December

# Create a function to get the third Friday of a given month and year
get_third_friday <- function(year, month) {
  # Create a sequence of dates for the specified month
  month_dates <- seq(ymd(paste(year, month, "1", sep = "-")), 
                     ymd(paste(year, month, days_in_month(month), sep = "-")), 
                     by = "day")
  
  # Find and return the third Friday
  month_dates %>%
    as_tibble() %>%
    mutate(day = wday(value, label = TRUE)) %>%  # Get day of the week
    filter(day == "Fri") %>%                      # Filter only Fridays
    slice(3)                                      # Select the third Friday
}

# Create a tibble to store the results
results <- tibble()
# Loop through each year and month to get the third Friday
for (year in years) {
  for (month in months) {
    third_friday <- get_third_friday(year, month)
    results <- results %>%
      bind_rows(tibble(Year = year, Month = month, Third_Friday = third_friday$value))
  }
}
ExpirationD <- results$Third_Friday


VOI<-VOI%>%mutate(Season=sapply(Date, function(d){ind<-which(ExpirationD<=ymd(d));return(ymd(ExpirationD[max(ind)]))}))

#Get the total OI for each season of each year
SeasonalOI <-VOI%>%group_by(Season)%>%
  mutate(TotOpenInterest = ave(OpenInterest, Season, FUN = function(x) mean(x, na.rm = TRUE))) %>%
  ungroup()%>%select(TotOpenInterest, Season)%>%unique()

SeasonalOI_ts<-ts(SeasonalOI$TotOpenInterest, frequency = 4)
stl_decomposition<-stl(SeasonalOI_ts,s.window = "periodic")

# Plot the decomposition
plot(stl_decomposition)
stl_decomposition<-stl_decomposition$time.series%>%as.data.frame()
tsdata=SeasonalOI%>%transmute(Date=as.character(as.Date(Season)), OpenInterest=stl_decomposition$trend)

subset(SPY_daily,Date>="2012-11-01")%>%plot_ly(x=~Date,  name = 'Price', type='candlestick',open=~Open, close=~Close,high=~High, low=~Low)%>%
  add_lines(x=~tsdata$Date, y=~tsdata$OpenInterest, yaxis="y2", name='OpenInterest',type='scatter', mode = 'lines',inherit = F)%>%
  layout(xaxis = list(rangeslider = list(visible = F),showticklabels=FALSE),
         yaxis2=list(overlaying="y",side="right"),
         hovermode = "x unified")%>%config(scrollZoom=TRUE)

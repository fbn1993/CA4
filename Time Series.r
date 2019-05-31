# retrieving the contents of csv file in dataframe
gdp_data <- read.csv("gdp_data.csv")

# Using melt fuction to put GDP of both sectors of Industry and Transportation in single column 
gdp_rs <- melt(gdp_data, id = c("Year"))

# Creating the ggplot to compare the GDP of both the sectors
ggplot(data = gdp_rs, aes(x = Year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))

# Performing t test to check p value
t.test(value ~ variable, data = gdp_rs)

# Creating three ggplots with the first one comparing the GDPs of the Industry 
# and Transportation sectors and the other two showing the density plots of both
# the sectors
p1 <- ggplot(data = gdp_rs, aes(x = variable, y = value)) + geom_boxplot()
p2 <- ggplot(data = gdp_data, aes(Industry)) + geom_density()
p3 <- ggplot(data = gdp_data, aes(Transportation)) + geom_density()
multiplot(p1, p2, p3, cols = 3)

# Defining the time series for the analysis
data_fraction <- (gdp_data$Industry - gdp_data$Transportation)/gdp_data$Transportation
data_timeseries <- ts(data_fraction, frequency = 1, start = gdp_data$Year[1])
autoplot(data_timeseries)

basicStats(data_fraction)

# Plotting acf and pacf
acf(data_timeseries)
pacf(data_timeseries)

# checking regression of time series against a constant value
summary(lm(data_timeseries ~ 1))

# Finding the break points and plotting them
(break_point <- breakpoints(data_timeseries ~ 1))
plot(break_point)
summary(break_point)

# Plotting time series and fitting the lines on break points
plot(data_timeseries)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))
fitted(break_point)[1]
fitted(break_point)[length(data_timeseries)]

# Applying the arima statisitical model and findind its summary and co efficients
(model_arima <- auto.arima(data_timeseries, stepwise = FALSE, trace = TRUE))
summary(model_arima)
coeftest(model_arima)

# Performing residual analysis on the model and LjungBoxTest
checkresiduals(model_arima)
LjungBoxTest(residuals(model_arima), k = 2, lag.max = 20)
sarima(data_timeseries, p = 1, d = 1, q = 1)

# Using Forecast function to predict the values 
h_fut <- 20
plot(forecast(model_arima, h = h_fut, xreg = rep(1, h_fut)))

#################################
#
# This page is use as scratch script
# for gamestop_casse_study.Rmd
#
#################################

# Load packages that will use
#library(ggthemes)  # package for ggplot theme
library(fpp3) # forcast package latest vers (tibble, dplyr, tidyr, lubridate, ggplot2, tsibble, feasts)
library(forecast) # forecasting functions for ts and linear models

# load data frame
balance_sheet <- read_csv("~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/balance_sheet_2015-2024.csv")
income_statement <- read_csv("~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/income_statement_2015-2023.csv")
cash_flow <- read_csv("~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/cash_flow_2015-2024.csv")
# changing data frame structure from wide to long
balance_sheet <- balance_sheet %>% 
  pivot_longer(cols = 2:11, names_to = "year", values_to = "values")
income_statment <- income_statment %>% 
  pivot_longer(cols = 2:11, names_to = "year", values_to = "values")
cash_flow <- cash_flow %>% 
  pivot_longer(cols = 2:11, names_to = "year", values_to = "values")
# changing data frame structure from long to wide
balance_sheet <- balance_sheet %>% 
  pivot_wider(names_from = account, values_from = values)
income_statement <- income_statment %>% 
  pivot_wider(names_from = account, values_from = values)
cash_flow <- cash_flow %>% 
  pivot_wider(names_from = account, values_from = values)
# save data frame from longer to wide
write.csv(balance_sheet, "~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/wide dataframe/balance_sheet.csv", row.names = F)
write.csv(income_statement, "~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/wide dataframe/income_statement.csv", row.names =F)
write.csv(cash_flow, "~/RStudio/RMarkdown/financial_project_01/Gamestop Excel/wide dataframe/cash_flow.csv", row.names = F)
# combine all dataset into one
write.csv(gamestop_fs, "~/RStudio/RMarkdown/financial_project_01/Excel/gamestop/wide dataframe/financial_statements_gamestop.csv", row.names = F)
write.csv(bestbuy_fs, "~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/wide data frame/financial_statement_bestbuy.cav", row.names = F)
# manipulate data frame
liquidity_ratio <- balance_sheet %>% 
  mutate("Current Ratio" = `Total current assets` / `Total current liabilities`,
         "Cash Ratio" = `Cash and cash equivalents`/ `Total current liabilities`,) %>% 
  select(year, `Current Ratio`, `Cash Ratio`)
  

################################################################
# Scrapping table using rvest
# First you need to load Rvest and Dplyr packages
# Why dplyr? because we need magittr function
################################################################

# GameStop revenue annualy from 1999 - 2024
link <- "https://stockanalysis.com/stocks/gme/revenue/"
page <- read_html(link)

gamestop_revenue <- page %>% html_nodes("table") %>% 
  html_table() %>% .[[1]]
# Cleaning dataset
gamestop_revenue <- read_csv("~/RStudio/RMarkdown/financial_project_01/Excel/gamestop/wide dataframe/gamestop_revenue_1999-2024.csv") %>%
  rename("year" = `Fiscal Year End`, "revenue" = Revenue, "growth" = Growth) %>% 
  mutate("year" = dmy(year))
# Save table
write_csv(gamestop_revenue, "~/RStudio/RMarkdown/financial_project_01/Excel/gamestop/wide dataframe/gamestop_revenue_1999-2024.csv")
# GameStop revenue quarterly from 31 Jan 2012 - 30 April 2024
link <- "https://ycharts.com/companies/GME/revenues"
page <- read_html(link)

quarter_revenue <- page %>% html_nodes("table.table") %>% 
  html_table() %>% .[[1]]
quarter_revenue01 <- page %>% html_nodes("table.table") %>% 
  html_table() %>% .[[2]]
# binding two table into one in rows format
quarter_revenue <- quarter_revenue %>% bind_rows(quarter_revenue, quarter_revenue01)
# save table
write_csv(quarter_revenue, "~/RStudio/RMarkdown/financial_project_01/Excel/gamestop/gamestop_quarter_revenue_2012-2024.csv")
# cleaning data frame
quarter_revenue <- read_csv("~/RStudio/RMarkdown/financial_project_01/Excel/gamestop/gamestop_quarter_revenue_2012-2024.csv")
quarter_revenue <- quarter_revenue %>% 
  rename_all(tolower) %>% 
  mutate("date" = dmy(date),
         "value" = value / 1000000) # vallue in million
# remove duplicate
quarter_revenue <- quarter_revenue %>% 
  unique()

# Visualizing dataset
# P.S in here I use ggthemes package for ggplot theme
#library(extrafont) # for changing font in graph
gamestop_revenue %>%
  mutate("revenue" = revenue / 1000000) %>% 
  ggplot(aes(x = year, y = revenue)) +
  geom_line(linewidth = 1.2, alpha = 0.7, color = "#102C57") +
  labs(
    title = "GameStop Revenue From 1999 - 2024",
    x = "",
    y = "Revenue in Billion",
    caption = "Data is gathered from GameStop SEC filling from fiscal end years of 1999 - 2024"
  ) +
  theme_calc()

################################################
# Start time series analysis
################################################

# Declare this as time series data
Y <- ts(quarter_revenue[,2], start = c(2012, 1), end = c(2024, 4), frequency = 4) # WRONG! they create new addition of 2024 Q3 and Q4 from repeating value
# Another way (update) to changing it in tibble format                            # Tried to used unique() but they change it to num data type
quarter_revenue_tibble <- quarter_revenue |>
  mutate(date = yearquarter(date)) |>
  as_tsibble(index = date)

# Time plot
autoplot(Y) +
  ggtitle("GameStop 10-Q Revenue From 2012 - 2024") +
  ylab("Revenue in Billion")

# Data has a strong trend. Investigate transformations.
# Get rid of it first (by making it stationary data)
# so it's easier and more accurate when we conduct
# ARIMA model
DY <- diff(Y)
# Time Plot of differenced data
autoplot(DY) +
  ggtitle("GameStop 10-Q Revenue no-trend graph")

# We already remove trend in our data (they now in trend-stationary condition)
# now it's time to get rid of seasonality in our data

# Plotting data to see if there's seasonal trend
ggseasonplot(DY) +
  ggtitle("Gamestop 10-Q Revenue Seasonal Trend Each Years") +
  ylab("Revenue in Billion")
# Another plot to see seasonal trend in our data
# that ffs2 provide (difference is the didn't lot it each year
# with different line, but they put it into one with average in it)
ggsubseriesplot(DY)

#################################################################################
# Y series has trend and seasonality.
# To remove the trend we take the first difference
# The first difference series still has seasonality
#
# Forecast with various methods
#################################################################################

# Use a benchmark method to forecast.
# Let's use the seasonal naive method as our benchmark.
# y_t = y_(t-s) + e_t
dat <- snaive(DY) # Residual SD: 563.5142
print(summary(dat))
checkresiduals(dat)

# Use ETS method
dat_ets <- ets(Y) # Residual SD: 476.717
print(summary(dat_ets)) # You find residual SD in here on 'Sigma'
checkresiduals(dat_ets)

# ARIMA method
dat_arima <- auto.arima(Y, d = 1, D = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE) # Residual SD: 476.3087
print(summary(dat_arima)) # To find SD in this method, you need to square^2 it first
checkresiduals(dat_arima) # using sqrt() function
sqrt(226870)
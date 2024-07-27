# install and load the remotes and rjava packages
install.packages(c("remotes", "rJava"))
library(remotes) # packages to download tabularpdf
library(rJava) # java programming language for r
library(dplyr) # manipulate data frame
library(tidyr) # tidying data frame
library(readr) # read csv file
# load the package after installing tabularpdf
library(tabulapdf)
# install and load the tabulapdf package (install through github)
install.packages("tabulapdf", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
# load the pdf file
# gamestop pdf
gamestop_balance_sheet <- "GamestopCorp_2015.pdf"
gamestop_2019 <- "GamestopCorp_2019.pdf"
gamestop_2023 <- "GamestopCorp_2023.pdf"
gamestop_2022 <- "GamestopCorp_2022.pdf"
# extract the table
# 2015
balance_sheet_2015 <- extract_tables(gamestop_balance_sheet, pages = 58) %>% as.data.frame()
income_statement_2015 <- extract_tables(gamestop_balance_sheet, pages = 59) %>% as.data.frame()
cash_flow_2015 <- extract_tables(gamestop_balance_sheet, pages = 62) %>% as.data.frame()
# 2019
balance_sheet_2019 <- extract_tables(gamestop_2019, pages = 37) %>% as.data.frame()
income_statement_2019 <- extract_tables(gamestop_2019, pages = 38) %>% as.data.frame()
cash_flow_2019 <- extract_tables(gamestop_2019, pages = 41) %>% as.data.frame()
# 2022
balance_sheet_2022 <- extract_tables(gamestop_2022, pages = 34) %>% as.data.frame()
income_statement_2022 <- extract_tables(gamestop_2022, pages = 35) %>% as.data.frame()
cash_flow_2022 <- extract_tables(gamestop_2022, pages = 37) %>% as.data.frame()
cash_flow_2022_01 <- extract_tables(gamestop_2022, pages = 38) %>% as.data.frame()
# unite cash_flow_2022 table into one
cash_flow_2022 <- bind_rows(cash_flow_2022, cash_flow_2022_01)
# 2023
balance_sheet_2023 <- extract_tables(gamestop_2023, pages = 32) %>% as.data.frame()
income_statement_2023 <- extract_tables(gamestop_2023, pages = 33) %>% as.data.frame()
cash_flow_2023 <- extract_tables(gamestop_2023, pages = 35) %>% as.data.frame()
cash_flow_2023_01 <- extract_tables(gamestop_2023, pages = 36) %>% as.data.frame()
# unite cash_flow_2023 table into one
cash_flow_2023 <- bind_rows(cash_flow_2023, cash_flow_2023_01)

#cleaning the table
balance_sheet_2015 <- balance_sheet_2015 %>% 
  rename("account" = "...1", "2015" = "X2016.2015", "2016" = "...5") %>% 
  select(c("account", "2015", "2016")) %>% 
  drop_na()

income_statement_2015 <- income_statement_2015 %>% 
  rename("account" = "...1", "2016" = "X.In.millions..except.per.share.data.", "2014" = "...4") %>%
  select("account", "2016", "2014") %>% 
  drop_na()

cash_flow_2015 <- cash_flow_2015 %>% 
  rename('account' = '...1', '2016' = 'January.30..2016', '2015' = 'January.31..2015', '2014' = 'February.1..2014') %>% 
  select('account', '2016', '2015', '2014') %>% 
  drop_na()

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

# save dataset to csv
write.csv(balance_sheet_2015, "~/RStudio/RMarkdown/financial_project_01/balance_sheet_2015.csv", row.names = F)
write.csv(balance_sheet_2018, "~/RStudio/RMarkdown/financial_project_01/balance_sheet_2018.csv", row.names = F)
write.csv(balance_sheet_2019, "~/RStudio/RMarkdown/financial_project_01/balance_sheet_2019.csv", row.names = F)
write.csv(balance_sheet_2022, "~/RStudio/RMarkdown/financial_project_01/balance_sheet_2022.csv", row.names = FALSE)
write.csv(balance_sheet_2023, "~/RStudio/RMarkdown/financial_project_01/balance_sheet_2023.csv", row.names = FALSE)
write.csv(income_statement_2015, "~/RStudio/RMarkdown/financial_project_01/income_statement_2015.csv", row.names = FALSE)
write.csv(income_statement_2019, "~/RStudio/RMarkdown/financial_project_01/income_statement_2019.csv", row.names = FALSE)
write.csv(income_statement_2023, "~/RStudio/RMarkdown/financial_project_01/income_statement_2023.csv", row.names = FALSE)
write.csv(cash_flow_2015, "~/RStudio/RMarkdown/financial_project_01/cash_flow_2015.csv", row.names = FALSE)
write.csv(cash_flow_2019, "~/RStudio/RMarkdown/financial_project_01/cash_flow_2019.csv", row.names = FALSE)
write.csv(cash_flow_2022, "~/RStudio/RMarkdown/financial_project_01/cash_flow_2022.csv", row.names = FALSE)
write.csv(cash_flow_2023, "~/RStudio/RMarkdown/financial_project_01/cash_flow_2023.csv", row.names = FALSE)


# Data frame for the competitor company
# which is BestBuy

# load the pdf file
bestbuy2024 <- "best_buy_2024.pdf"
bestbuy2022 <- "best_buy_2022.pdf"
bestbuy2020 <- "best_buy_2020.pdf"
# extract the table
# 2024
balance_sheet_2024 <- extract_tables(bestbuy2024, pages = 39) %>% as.data.frame()
income_statement_2024 <- extract_tables(bestbuy2024, pages = 40) %>% as.data.frame()
cash_flow_2024 <- extract_tables(bestbuy2024, pages = 42) %>% as.data.frame()
# 2022
balance_sheet_2022 <- extract_tables(bestbuy2022, pages = 40) %>% as.data.frame()
income_statement_2022 <- extract_tables(bestbuy2022, pages = 41) %>% as.data.frame()
cash_flow_2022 <- extract_tables(bestbuy2022, pages = 43) %>% as.data.frame()
#2020
balance_sheet_2020 <- extract_tables(bestbuy2020, pages = 40) %>% as.data.frame()
income_statement_2020 <- extract_tables(bestbuy2020, pages = 41) %>% as.data.frame()
cash_flow_2020 <- extract_tables(bestbuy2020, pages = 43) %>% as.data.frame()
# save data frame
write.csv(balance_sheet_bb, "~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/wide data frame/balance_sheet_bb.csv", row.names = F)
write.csv(income_statement_bb, "~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/wide data frame/income_statement_bb.csv", row.names = F)
write.csv(cash_flow_bb, "~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/wide data frame/cash_flow_bb.csv", row.names = F)
# change best buy data frame from long to wide
# load the data frame
balance_sheet_bb <- read_csv("~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/balance_sheet_bb.csv")
income_statement_bb <- read_csv("~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/income_statement_bb.csv")
cash_flow_bb <- read_csv("~/RStudio/RMarkdown/financial_project_01/Excel/bestbuy/cash_flow_bb.csv")
# changing data frame structure
# to longer
balance_sheet_bb <- balance_sheet_bb %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "value")
income_statement_bb <- income_statement_bb %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "value")
cash_flow_bb <- cash_flow_bb %>% 
  pivot_longer(cols = 2:7, names_to = "year", values_to = "values")
# from longer to wider (data type)
balance_sheet_bb <- balance_sheet_bb %>% 
  rename("values" = "value") %>% 
  pivot_wider(names_from = account, values_from = values)
income_statement_bb <- income_statement_bb %>% 
  rename("values" = "value") %>% 
  pivot_wider(names_from = account, values_from = values)
cash_flow_bb <- cash_flow_bb %>% 
  pivot_wider(names_from = accounts, values_from = values)

# needed packages
library(tidyverse)
library(jsonlite)
library(finreportr)
library(rvest)
library(tidyquant)
library(data.table)

# list of S&P 500 companies
myurl = 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'

# converting web table to a dataframe
tickers =
  myurl %>% 
  read_html() %>% 
  html_nodes(
    xpath = '//*[@id="constituents"]'
    ) %>% 
  html_table(fill=TRUE) %>%
  as.data.frame()
 
# data wrangling 
tickers_test = 
  tickers %>% 
  select(
    Symbol,
    Security,
    GICS.Sector,
    GICS.Sub.Industry,
    Date.first.added
  ) %>%
  drop_na(Date.first.added) %>% 
  rename(
    company_name = Security,
    sector = GICS.Sector,
    subsector = GICS.Sub.Industry
  )

tickers_test$revenue = sample(1000000:10000000000, size = nrow(tickers_test), replace = TRUE)

# creating function to fetch stock price on 11/14/22
get_symbols = function(ticker){
  tq_get(
    ticker, 
    get = "stock.prices", 
    complete_cases = TRUE,
    from = "2022-11-14"
    )
}

# getting stock price for all S&P companies
tickers_df = 
  map(tickers_test$Symbol, get_symbols)

# removing a couple of problematic records and coverting list to table
tickers_df_two = 
  tickers_df[-c(66, 82)] %>% 
  rbindlist(fill=TRUE) 
  
# function to fetch financial data from Yahoo finance based on stock ticker
get_stats <- function(symbol) {
  url <- paste0("https://finance.yahoo.com/quote/",symbol,"/key-statistics?p=", symbol)
  df <- url %>%
    read_html() %>%
    html_table(header = FALSE) %>%
    map_df(bind_cols) %>%
    as_tibble()
  
  names(df) <- c("valuation_measures", "value")
  df["stock"] <- symbol
  
  return(df)
}

# function to keep an error from stopping execution 
catch_error <- function(.f, otherwise=NULL) {
  function(...) {
    tryCatch({
      .f(...)  
    }, error = function(e) otherwise(...))
  }
}

# building dataframe with needed data
fin_data = 
  map_df(
    tickers_df_two$symbol,
    catch_error(
      get_stats, 
      otherwise = function(x) 
        tibble(
          valuation_measures = NA_character_, 
          value = NA_character_, 
          stock = x, 
          error = "error in getting data"
          )
      )
    )

# getting only the value i need (revenue)
mark_cap = 
  fin_data %>% 
  filter(
    str_detect(valuation_measures, 'Revenue') 
  ) %>% 
  select(
    stock,
    valuation_measures,
    value
  ) %>% 
  pivot_wider(
    names_from = valuation_measures,
    values_from = value
  ) %>% 
  rename(
    rev = 3
  ) %>% 
  mutate(
    rev = 
      case_when(
        str_detect(rev, 'M') ~ str_replace(rev, 'M', 'e6'),
        str_detect(rev, 'B') ~ str_replace(rev, 'B', 'e9'),
        TRUE ~ rev
      )
  ) %>%
  mutate(
    rev =  as.numeric(rev)
  ) %>% 
  drop_na(rev)

# adding revenue to sector/subsector classifcation
revenue = 
  left_join(
    tickers_test %>% 
      select(
        Symbol,
        company_name,
        sector,
        subsector
        ) %>%
      mutate(
        company_name = str_remove_all(company_name, "'")
      ),
    mark_cap,
    by = c('Symbol' = 'stock'),
    keep = FALSE
  ) %>%
  drop_na(rev) %>%
  mutate(
    sql = paste0("values ('", Symbol,"', '", company_name, "', '", sector, "', '", subsector, "', ", as.character(format(as.numeric(rev), scientific = FALSE)), ") union all")
  )


write.csv(
  revenue, 
  'C:/Users/raymond.lucio/Downloads/revenue.csv')

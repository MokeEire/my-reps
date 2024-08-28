# Stage 1: load functions
source("R/parsing_functions.R")

# Stage 2: load and process data
house_bill_types = list.files(here("data", "BILLSTATUS","117"), pattern = "^h")

house_bill_folders = here("data", "BILLSTATUS", "117", 
                          house_bill_types)

house_bill_files = map(house_bill_folders, list.files, full.names = T) %>% 
  set_names(house_bill_types)

extract_bill_status(house_bill_files$hr[1951])
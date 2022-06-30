library(extrafont)
library(tictoc)
library(furrr)
library(here)
library(reactable)
plan(multisession)
source("R/parsing_functions.R")



action_codes = read_csv(here("data", "action_codes.csv"), col_types = "cc")

# List files

bill_types = list.files(here("data", "BILLSTATUS","117"))

bill_folders = here("data", "BILLSTATUS", "117", bill_types) %>% 
    str_remove_all("\\/OneDrive")

bill_files = map(bill_folders, list.files, full.names = T) %>% 
  set_names(bill_types)


all_files = flatten_chr(bill_files)

tic(str_c("Extract ", sum(map_dbl(bill_files, length)), " bills"))
all_bills = future_map_dfr(all_files, extract_bill_status, log_types = "console")
toc()

tic("Code all bills")
all_bills_coded = mutate(all_bills,
                         actions = future_map(actions, remove_duplicate_actions),
                         actions = future_map(actions, code_actions, action_codes = action_codes))
toc()

actions_unnested = unnest(all_bills_coded, actions)

saveRDS(all_bills, here("data", "cleaned", "BILLSTATUS_117_House.Rds"))
all_bills = readRDS(here("data", "cleaned", "BILLSTATUS_117_House.Rds"))

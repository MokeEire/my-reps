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

saveRDS(all_bills, here("data", "cleaned", "BILLSTATUS_117_House.Rds"))

actions_unnested = select(all_bills, where(negate(is_list)), starts_with("action")) %>% 
  # Unnest actions
  unnest(actions) %>% 
  # Create action timestamp
  mutate(action_ts = make_datetime(year = year(actionDate), 
                                   month = month(actionDate), day = day(actionDate), 
                                   hour = coalesce(hour(actionTime), 0), 
                                   min = coalesce(minute(actionTime), 0), 
                                   sec = coalesce(second(actionTime), 0), 
                                   tz = "US/Eastern"),
         action_type = fct_explicit_na(
           factor(action_type, 
                  levels = c("IntroReferral", "Committee", "Floor", 
                             "Discharge", "President", "BecameLaw"), 
                  ordered = T),
           na_level = "(Missing Action Type)"
         ),
         action_source_name = factor(action_source_name,
                                     levels = c("Library of Congress", "House floor actions", 
                                                "House committee actions", "Senate"),
                                     ordered = T)) %>% 
  
  # Join in action codes
  left_join(action_codes, by = c("actionCode" = "Code")) %>% 
  # Order bills and actions
  arrange(bill_id, action_ts, action_type, action_source_name) %>% 
  # Number actions
  group_by(billType, billNumber) %>% 
  # Number actions
  mutate(action_number = row_number(),
         became_law = ("BecameLaw" %in% action_type), .after = actionTime) %>% 
  ungroup()

actions_unnested_clean = actions_unnested %>% 
  # Remove Intro-H action codes (keep NAs)
  filter(replace_na(actionCode != "Intro-H", T))


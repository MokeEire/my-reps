# Libraries
library(extrafont)
library(tictoc)
library(furrr)
library(here)
library(reactable)
library(reactablefmtr)
plan(multisession)
source("R/parsing_functions.R")


# Load data ---------------------------------------------------------------

# Load cleaned R objects
all_bills = readRDS(here("data", "cleaned", "BILLSTATUS_117_2022-11-10.Rds"))

# Action code mapping table from GovInfo
# https://github.com/usgpo/bill-status/blob/main/BILLSTATUS-XML_User_User-Guide.md#3-action-code-element-possible-values
action_codes = read_csv(here("data", "action_codes.csv"), col_types = "cc")


# Prepare action data -------------------------------------------------------------

# Select ID cols and unnest actions from nested data frames
actions_unnested = select(all_bills, 
                          bill_id, title, bill_type, actions) %>% 
  unnest(actions)

# Join in action code descriptions
actions_joined = actions_unnested %>% 
  left_join(action_codes, by = "action_code")


# Clean Intro/Referral actions --------------------------------------------


intro_actions = actions_joined %>% 
  filter(action_type == "IntroReferral")

intro_action_codes = tribble(
  ~action_code, ~action_code_desc,
  "1000", "Introduced in House",
  "Intro-H", "Introduced in House",
  "10000", "Introduced in Senate",
  "B00100", "Sponsor introductory remarks on measure",
  "H11100", "Referred to Committee",
  "H11210", "Committee granted an extension for further consideration"
)

intro_actions_coded = intro_actions %>%
  # Join action codes
  left_join(intro_action_codes, by = "action_code") %>% 
  # Assign actions to category based on their text when missing action code
  mutate(action_code_desc = case_when(
    !is.na(action_code_desc) ~ action_code_desc,
    str_detect(action_text, "[Rr]eferred to the (Select\\s)?Committee") ~ "Referred to Committee",
    str_detect(action_text, "Received in the Senate") ~ "Received in the Senate"
  ))

intro_actions_dedupe = intro_actions_coded %>% 
  # Remove Intro-H action codes (keep NAs)
  filter(replace_na(action_code != "Intro-H", T))

tic("Code all bills")
all_bills_coded = mutate(all_bills,
                         bill_type_cat = case_when(bill_type %in% c("HR", "S") ~ "Bill",
                                                   bill_type %in% c("HJRES", "SJRES") ~ "Joint Resolution",
                                                   bill_type %in% c("HRES", "SRES") ~ "Simple Resolution",
                                                   bill_type %in% c("HCONRES", "SCONRES") ~ "Concurrent Resolution"),
                         actions = future_map(actions, remove_duplicate_actions),
                         actions = future_map(actions, code_actions, action_codes = action_codes))
toc()


actions_unnested = unnest(all_bills_coded, actions)
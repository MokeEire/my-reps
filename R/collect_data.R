library(extrafont)
library(tictoc)
library(furrr)
library(here)
library(reactable)
plan(multisession)
source("R/parsing_functions.R")

# List files ----
# Files collected from:
# https://www.govinfo.gov/bulkdata/BILLSTATUS/117
# Congress committee membership:
# https://github.com/unitedstates/congress-legislators/commit/5e4d9a0656458646e96c2b378fba640c0e22f8b1?diff=split
# Also here:
# https://clerk.house.gov/xml/lists/MemberData.xml
bill_types = list.files(here("data", "BILLSTATUS","117"))

bill_folders = here("data", "BILLSTATUS", "117", bill_types) |> 
    str_remove_all("\\/OneDrive")

bill_files = map(bill_folders, list.files, full.names = T) |> 
  set_names(bill_types)

all_files = flatten_chr(bill_files)


# Get files from sitemap --------------------------------------------------
# bill_status_sitemaps = read_xml("https://www.govinfo.gov/sitemap/bulkdata/BILLSTATUS/sitemapindex.xml") %>% 
#   # Strip namespace for coherent xpaths
#   xml_ns_strip() %>% 
#   # Find all loc elements which contain urls
#   xml_find_all("/sitemapindex/sitemap/child::loc") %>% 
#   # Convert them to text
#   map_chr(xml_text)

# Select sitemaps for given congress
# sitemaps_117 = str_subset(bill_status_sitemaps, "117")
# 
# (xml_nodes_117 = sitemaps_117 %>% 
#     map(read_xml) %>% 
#     map(xml_ns_strip) %>% 
#     map(xml_find_all, "/urlset/url/child::loc"))
# 
# xml_files_117 = xml_nodes_117 %>% 
#   map(xml_text) %>% 
#   set_names(str_extract(sitemaps_117, "(?<=BILLSTATUS\\/117)[a-z]{1,7}"))
# 
# xml_files_117_sample = map(xml_files_117, sample, size = 10)


# Extract bills from files ------------------------------------------------

# Testing
sample_files = sample(all_files, 500)

tic()
sample_df = future_map(sample_files, extract_bill_status, 
                       log_types = "console", .progress=T) |> 
  list_rbind()
toc()


tic(str_c("Extract ", length(all_files), " bills"))
all_bills = future_map(all_files, extract_bill_status, 
                       log_types = NULL, .progress = T) |> 
  list_rbind() |> 
  select(congress, origin_chamber, bill_id, type, number, title, 
         introduced_date, update_date, 
         latest_action_date = latest_action_action_date, latest_action_text, latest_action_action_time = latest_action_action_time,
         policy_areas, legislative_subjects, constitutional_authority_statement_text, titles, summaries, text_versions,
         sponsors, cosponsors, actions, committees,
         everything())
toc()


# Check data --------------------------------------------------------------

skimr::skim(all_bills)
# Note: length on list variables counts tibble columns

# Save objects ------------------------------------------------------------

saveRDS(all_bills, here("data", "cleaned", paste0("BILLSTATUS_117_", lubridate::today(), ".Rds")))
# saveRDS(actions_unnested, here("data", "cleaned", "BILLSTATUS_117_Actions.Rds"))



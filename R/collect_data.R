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

bill_types = list.files(here("data", "BILLSTATUS","117"))

bill_folders = here("data", "BILLSTATUS", "117", bill_types) %>% 
    str_remove_all("\\/OneDrive")

bill_files = map(bill_folders, list.files, full.names = T) %>% 
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
# map_dfr(bill_files$hres, extract_bill_status, log_types = "console")
tic(str_c("Extract ", length(all_files), " bills"))
all_bills = future_map_dfr(all_files, extract_bill_status, log_types = NULL)
toc()



# Save objects ------------------------------------------------------------

saveRDS(all_bills, here("data", "cleaned", paste0("BILLSTATUS_117_", lubridate::today(), ".Rds")))
# saveRDS(actions_unnested, here("data", "cleaned", "BILLSTATUS_117_Actions.Rds"))

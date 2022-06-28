library(tidyverse)
library(rjson)
library(jsonlite)
library(RCurl)
library(lubridate)
library(tidyjson)
library(XML)
library(xml2)
library(log4r)
library(mokeR)


# Logging functions -------------------------------------------------------

log_layout = function(level, ...){
  function(level,
           ..., bill_type, bill_num){
    msg = paste0(..., collapse = "")
    
    sprintf("%s | Bill #%s | %s | %s\n", bill_type, bill_num,
            log4r:::fmt_current_time("%Y-%m-%d %H:%M:%S"),
            msg)
  }
}

create_logger = function(log_threshold = "INFO", 
                         directory = here::here("logs"), 
                         log_types = c("file", "console")){
  
  log_appenders = list()
  if("file" %in% log_types){
    log_file = here::here(here::here("logs"), 
                          paste0("log-", 
                                 format(lubridate::now(), 
                                        "%Y-%m-%d--%H-%M-%S"), 
                                 ".txt"))
    
    file.create(log_file)
    
    log_appenders = append(
      log_appenders, 
      log4r::file_appender(log_file, append = T, layout = log_layout()))
  }
  
  if("console" %in% log_types){
    
    log_appenders = append(log_appenders, 
                           log4r::console_appender(layout = log_layout()))
  }
  
  log4r::logger(
    threshold = log_threshold,
    appenders = log_appenders
  )
}

log_info = function(logger, ...){
  if(logger$threshold > log4r:::INFO)
    return(invisible(NULL))
  for (appender in logger$appenders){
    appender(level="INFO", ...)
  }
}

log_debug = function(logger, ...){
  if(logger$threshold > log4r:::DEBUG)
    return(invisible(NULL))
  for (appender in logger$appenders){
    appender(level="DEBUG", ...)
  }
}

read_log = function(log_file){
  if(missing(log_file)){
    logs = file.info(list.files(here("logs"), full.names = T))
    log_file = row.names(logs[which.max(logs$ctime), ])
  }
  read_delim(log_file, delim = " | ", 
             col_names = c("bill_type", "bill_num", "time", "action"), 
             col_types = "ccTc") %>% 
    group_by(bill_type, bill_num) %>% 
    mutate(parse_time_s = time_length(max(time) - min(time), unit = "second")) %>% 
    ungroup() %>% 
    arrange(bill_type, bill_num, time)
}


# API functions -----------------------------------------------------------


getPackages = function(packageId, summary = F, xml = T){
  summary_url = paste0("https://api.govinfo.gov/packages/", packageId, 
                       "/summary",
                       "?api_key=", apiGovKey)
  # encode the URL with characters for each space.
  summary_json = fromJSON(URLencode(summary_url)) %>% 
    flatten_dfc()
  
  if(summary){
    return(summary_json)
  }
  
  if(xml){
    summary_json$xmlLink
  }
  
  
}


get_package_xml = function(packageId){
  
  summary_url = paste0("https://api.govinfo.gov/packages/", packageId, 
                       "/summary",
                       "?api_key=", apiGovKey)
  
  # encode the URL with characters for each space.
  fromJSON(URLencode(summary_url)) %>% 
    pluck("download", "xmlLink")
  
}

getPublished = function(dateIssuedStartDate, dateIssuedEndDate,
                        startingRecord = 0, numRecords = 20,
                        collections, congress,
                        docClass){
  # If end date is provided, format it
  if(missing(dateIssuedEndDate)){
    end_date = ""
  } else {
    end_date = paste0("/", format_date_api(dateIssuedEndDate))
  }
  
  # Construct URL to request
  url = paste0(
    # Root
    "https://api.govinfo.gov/published/",
    # Start and end dates
    format_date_api(dateIssuedStartDate), end_date,
    # Record indexing
    "?offset=", startingRecord, "&pageSize=", numRecords, 
    # Collection
    "&collection=",collections,
    if_else(missing(congress), "", paste0("&congress=", congress)),
    if_else(missing(docClass), "", paste0("&docClass=", docClass)),
    # API key
    "&api_key=", apiGovKey
  )
  
  
  # Request URL encoded for any spaces
  request = fromJSON(URLencode(url))
  # Assign the packages dataframe
  packages = request$packages
  
  # Request the next page of results while a next page exists
  while(!is_null(request$nextPage)){
    next_page = URLencode(request$nextPage)
    request = fromJSON(next_page)
    
    # Requests tend to fail as we approach 10k results
    if(nrow(packages)> 9500){
      browser()
    }
    # Append new results to packages
    packages = bind_rows(packages, request$packages)
    
    # A tiny bit of sleep seems to reduce API errors
    Sys.sleep(.1)
  }
  
  return(packages)
}


# Helper functions --------------------------------------------------------



silent_convert = function(df, ...){
  suppressWarnings(suppressMessages(readr::type_convert(df, ...)))
}

flatten_rename = function(list_to_flatten, 
                          name_prefix = "prefix"){
  rename_with(flatten_dfc(list_to_flatten), ~str_c(name_prefix, "_", .))
}

format_date_api = function(date){
  stopifnot(is.Date(date) || is.timepoint(date))
  format(date, "%Y-%m-%dT%H:%M:%SZ")
}


# XML parsing functions ---------------------------------------------------



parse_committee = function(committee){
  committee_tibbled = committee %>% 
    modify_at("activities", map_dfr, ~flatten_rename(.x, "committee_activity")) %>% 
    modify_at("subcommittees", function(subcommittee){
      map_dfr(subcommittee, parse_subcommittee)
    })
  
  committee_df = discard(committee_tibbled, is_tibble) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c("committee_", .)) %>% 
    mutate(committee_activities = list(committee_tibbled$activities),
           subcommittee_activities = list(committee_tibbled$subcommittees))
  
  return(committee_df)
}

parse_subcommittee = function(subcommittee){
  modify_at(subcommittee, "activities", map_dfr, ~flatten_rename(.x, "activity")) %>% 
    flatten_dfc() %>% 
    rename_with(~str_c("subcommittee_", .))
}

parse_votes = function(recorded_votes){
  modify_at(recorded_votes, "activities", map_dfr, ~flatten_rename(.x, "activity")) %>% 
    flatten_dfc() %>% 
    rename_with(~str_c("subcommittee_", .))
}

parse_vote_roll = function(vote, logger, bill_type, bill_num){
  
  tryCatch(
    {
      vote_xml = read_xml(vote, options = "RECOVER")
      vote_data = xml_find_all(vote_xml, "vote-data")
      vote_data = xml_find_all(vote_xml, "vote-data")
      
      vote_list = as_list(vote_data)
      
      flatten_dfr(vote_list) %>% 
        unnest(everything())
    },
    error=function(cond) {
      log_info(logger, 
               bill_type = bill_type,
               bill_num = bill_num, 
               "ERROR: Vote roll could not be parsed")
      # Choose a return value in case of error
      return(tibble())
    }
  )
  
  
}

parse_action = function(action){
  action %>% 
    modify_at("sourceSystem", ~flatten_rename(.x, "source")) %>% 
    modify_at("committees", function(committee){
      map_dfr(committee, ~flatten_rename(.x, "committee"))
    }) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c("action_", .), .cols = -starts_with("action"))
}

parse_amendment = function(amendment){
  amendment %>% 
    map_at("actions", function(actions){
      actions %>% 
        modify_at("actions", map_dfr, parse_action) %>% 
        modify_at("actionTypeCounts", flatten_dfc) %>% 
        modify_at("actionByCounts", flatten_dfc)
    }) %>% flatten_dfc()
}

parse_sponsor = function(sponsor, role = "sponsor"){
  sponsor %>% 
    modify_at("identifiers", ~flatten_rename(.x, "identifiers")) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c(role, "_", .), .cols = -starts_with(role))
}

xml_nonempty_nodes = function(xml_node){
  xml_children(xml_node)[xml_length(xml_children(xml_node)) == 0] %>% 
    keep(~(xml_text(.) != ""))
}

# attribute_tibble_templates = list(
#   actions = tibble(
#     actionDate = col_date(), 
#     actionTime = col_time(),
#     action_committee_systemCode = col_character(), 
#     action_committee_name = col_character(), 
#     action_source_code = col_character(),
#     action_source_name = col_character(),
#     action_text = col_character(), 
#     action_type = col_character(), 
#     actionCode = col_character()
#   )
# )

attribute_col_types = list(
  actions = cols(actionDate = col_date(), 
                 actionTime = col_time(),
                 action_committee_systemCode = col_character(), 
                 action_committee_name = col_character(), 
                 action_source_code = col_character(),
                 action_source_name = col_character(),
                 action_text = col_character(), 
                 action_type = col_character(), 
                 actionCode = col_character()),
  cosponsors = cols(cosponsor_bioguideId = col_character(), 
                    cosponsor_fullName = col_character(), 
                    cosponsor_firstName = col_character(), 
                    cosponsor_lastName = col_character(), 
                    cosponsor_party = col_character(), 
                    cosponsor_state = col_character(), 
                    cosponsor_identifiers_lisID = col_character(),
                    cosponsor_identifiers_bioguideId = col_character(),
                    cosponsor_district = col_character(),
                    cosponsor_sponsorshipDate = col_date(),
                    cosponsor_sponsorshipWithdrawnDate = col_date(),
                    cosponsor_isOriginalCosponsor = col_character(),
                    cosponsor_middleName = col_character())
)

count_attr_colnames = function(xml_file, attribute = "actions"){
  bill_xml = read_xml(xml_file) %>% 
    xml_child("bill")
  
  if(attribute == "actions"){
    as_list(xml_find_all("actions/item")) %>% 
      map_dfr(parse_action) %>% 
      colnames()
  }
}

#' Remove duplicate actions
#'
#' @param actions_df 
#'
#' @return
#' @export
#'
#' @examples
remove_duplicate_actions = function(actions_df){
  actions_df %>% 
    # Remove Intro-H action codes (keep NAs), replace_na keeps missing actionCodes
    filter(replace_na(actionCode != "Intro-H", T))
}

#' Number actions
#' 
#' Create action number to represent chronological order. Currently this is determined by sorting in the following order:
#' 
#' - Timestamp
#' - Action Type (in the order IntroReferral, Committee, Floor, Discharge, President, BecameLaw)
#' - Action Source (in the order Library of Congress, House floor actions, House committee actions, Senate)
#'
#' @param actions_df 
#'
#' @return
#' @export
#'
#' @examples
number_actions = function(actions_df){
  actions_ordered = actions_df %>% 
    # Order bills and actions
    arrange(action_ts, action_type, action_source_name) %>% 
    # Number actions
    mutate(action_number = row_number()) %>% 
    ungroup()
  
  # message(select(actions_ordered, action_number, action_ts, action_type, action_text))
  return(actions_ordered)
}



#' Code actions
#' 
#' Code action type and action source as factors, combine action date and action time
#'
#' @param actions_df 
#' 
#' 
#'
#' @return
#' @export
#'
#' @examples
code_actions = function(actions_df, action_codes){
  # TODO: 
  cols <- c(actionTime = NA_POSIXct_)
  
  # Ensure actionTime is present
  add_column(actions_df, !!!cols[setdiff(names(cols), names(actions_df))]) %>% 
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
    number_actions() %>% 
    # Create boolean for whether bill became law
    mutate(became_law = ("BecameLaw" %in% action_type), .after = actionTime) %>% 
    ungroup()
}

extract_bill_status = function(xml_file, 
                               nested_attributes = c("committees", "votes", "actions", "sponsors", "cosponsors"),
                               get_votes = T,
                               col_specs = attribute_col_types,
                               log_threshold = "INFO",
                               log_types = c("file", "console")){
  # Create logger
  logger = create_logger(log_threshold = log_threshold, 
                         log_types = log_types)
  
  bill_xml = xml_child(read_xml(xml_file), "bill")
  
  singletons = xml_nonempty_nodes(bill_xml)
  # browser()
  # Singletons strewn together
  bill_df = flatten_dfc(
    setNames(
      map(singletons, xml_text), 
      xml_name(singletons))
    
  )

  # browser()
  # Extract non-singular base attributes ----
  ## Policy area
  policy_areas = as_list(xml_find_all(bill_xml, "policyArea/name")) %>% 
    map_chr(flatten_chr)
  ## Subjects
  bill_subjects = as_list(xml_find_all(bill_xml, "subjects/billSubjects/legislativeSubjects/item/name")) %>% 
    map_chr(flatten_chr)
  ## Summaries
  bill_summaries = as_list(xml_find_all(bill_xml, "summaries/billSummaries/item")) %>% 
    map_dfr(flatten_dfc)
  ## Titles
  bill_titles = as_list(xml_find_all(bill_xml, "titles/item")) %>% 
    map_dfr(flatten_dfc)
  ## Text versions
  bill_text_versions = as_list(xml_find_all(bill_xml, "textVersions/item")) %>% 
    map(map_at, "formats", map_dfr, flatten_dfc) %>% 
    map_dfr(flatten_dfc)
  ## Latest action
  latest_action = as_list(xml_find_all(bill_xml, "latestAction")) %>% 
    map(flatten_dfc) %>% 
    map_dfr(~rename_with(.x, ~str_c("latestAction_", .)))
  
  # bill_df = bill_df %>% 
  #   mutate(policy_areas = list(policy_areas),
  #          legislative_subjects = list(bill_subjects),
  #          bill_summaries = list(bill_summaries),
  #          bill_titles = list(bill_titles),
  #          bill_text_versions = list(bill_text_versions)) %>% 
  #   bind_cols(latest_action)
  bill_df$policy_areas = list(policy_areas)
  bill_df$legislative_subjects = list(bill_subjects)
  bill_df$bill_summaries = list(bill_summaries)
  bill_df$bill_titles = list(bill_titles)
  bill_df$bill_text_versions = list(bill_text_versions)
  
  bill_df = bind_cols(bill_df, latest_action)

  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Reading XML")

  xpaths = xml_children(bill_xml) %>% 
      map_chr(xml_path)
  
  bill_nodesets = map(xpaths, ~ xml_find_all(bill_xml, xpath = .x)) %>% 
    set_names(map_chr(., xml_name))
  
  # browser()
  # Committees ---------
  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing committees")
  
  bill_committees = xml_find_all(bill_nodesets[["committees"]], "billCommittees")
  
  if("committees" %in% nested_attributes && xml_length(bill_committees)>0){

    committees = xml_find_all(bill_committees, "item")
    # Coerce nodes to list
    committees_list = as_list(committees)
    
    committees_df = map_dfr(committees_list, parse_committee)
    
    bill_df$committees = list(committees_df)
    
  } else {
    bill_df$committees = list(tibble())
  }
  
  # Votes ---------
  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing votes")
  
  votes_node = bill_nodesets[["recordedVotes"]]
  if("votes" %in% nested_attributes && xml_length(votes_node)>0 && get_votes){
    
    bill_votes = xml_find_all(votes_node, "recordedVote")
    # Coerce nodes to list
    votes_list = as_list(bill_votes)

    votes_df = map_dfr(votes_list, flatten_dfc)

    # Add Vote tallies
    vote_rolls_df = votes_df %>% 
      mutate(vote_roll = map(url, parse_vote_roll, 
                             logger = logger, 
                             bill_type = bill_df$billType,
                             bill_num = bill_df$billNumber),
             roll_found = map_lgl(vote_roll, ~(nrow(.) > 0)))
    
    bill_df$votes = list(vote_rolls_df)
  } else {
    bill_df$votes = list(tibble())
  }
  
  # Actions ---------
  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing actions")
  actions_node = bill_nodesets[["actions"]]
  if("actions" %in% nested_attributes && xml_length(actions_node)>0){
    bill_actions = xml_find_all(actions_node, "item")
    
    bill_action_counts = as_list(xml_find_all(actions_node, "./*[not(self::item)]")) %>% 
      map_dfc(flatten_dfc) %>% 
      rename_with(.cols = everything(), ~str_c("actions_", .)) %>% 
      pivot_longer(everything(), names_to = "action", names_prefix = "actions_", values_to = "count")
    
    # Coerce nodes to list
    actions_df = as_list(bill_actions) %>% 
      map_dfr(parse_action) %>% 
      type_convert(col_types = col_specs$actions)
    
    bill_df$actions = list(actions_df)
    
    bill_df$action_counts = list(type_convert(bill_action_counts,
                                              col_types = cols(action = col_character(), count = col_integer())))
  } else {
    bill_df$actions = list(tibble())
  }
  
  # Amendments ---------
  # log_debug(logger, 
  #          bill_type = bill_df$billType,
  #          bill_num = bill_df$billNumber,
  #          "Parsing amendments")
  # amendments_node = bill_nodesets[["amendments"]]
  # if(xml_length(amendments_node)>0){
  #   browser()
  #   bill_amendments = xml_find_all(amendments_node, "amendment")
  #   # Coerce nodes to list
  #   amendments_df = map(bill_amendments, as_list) %>% 
  #     map_dfr(parse_action)
  #   
  #   bill_df$amendments = list(silent_convert(amendments_df))
  # } else {
  #   bill_df$amendments = list(tibble())
  # }
  
  # Sponsors ---------
  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing sponsors")
  
  bill_sponsors = xml_find_all(bill_nodesets[["sponsors"]], "item")
  if("sponsors" %in% nested_attributes && xml_length(bill_sponsors)>0){

    # Coerce nodes to list
    sponsors_df = map(bill_sponsors, as_list) %>% 
      map_dfr(parse_sponsor)
    
    bill_df$sponsors = list(sponsors_df)
  } else {
    bill_df$sponsors = list(tibble())
  }
  
  
  # Cosponsors ---------
  log_debug(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing cosponsors")
  cosponsors_node = bill_nodesets[["cosponsors"]]
  if("cosponsors" %in% nested_attributes && xml_length(cosponsors_node)>0){
    
    bill_cosponsors = xml_find_all(cosponsors_node, "item")
    # Coerce nodes to list
    cosponsors_df = map(bill_cosponsors, as_list) %>% 
      map_dfr(parse_sponsor, role = "cosponsor")
    
    bill_df$cosponsors = list(type_convert(cosponsors_df, col_types = col_specs$cosponsors))
  } else {
    bill_df$cosponsors = list(tibble())
  }
  
  finished_df = as_tibble(bill_df) %>% 
    # Combine bill type and number to create an ID
    unite(bill_id, billType, billNumber, sep = "-", remove = F)
  
  log_info(logger, 
            bill_type = bill_df$billType,
            bill_num = bill_df$billNumber,
            "Complete")
  
  mutate(finished_df,
         across(ends_with("Date"), as_datetime),
         actions = map(actions, mutate, 
                       action_type = factor(action_type, 
                                            levels = c("IntroReferral", "Committee", "Floor", 
                                                       "Discharge", "President", "BecameLaw"), 
                                            ordered = T))
         )
}

trunc_columns = function(df){
  df %>% 
    mutate(across(where(is_character), str_trunc, width = 32))
}

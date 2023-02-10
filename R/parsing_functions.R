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
library(janitor)


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

get_published = function(dateIssuedStartDate, dateIssuedEndDate,
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
    # format_date_api(dateIssuedStartDate), 
    dateIssuedStartDate,
    end_date,
    # Record indexing
    "?offset=", startingRecord, "&pageSize=", numRecords, 
    # Collection
    "&collection=",collections,
    ifelse(missing(congress), "", paste0("&congress=", congress)),
    ifelse(missing(docClass), "", paste0("&docClass=", docClass)),
    # API key
    "&api_key=", apiGovKey
  )
  
  
  # Request URL encoded for any spaces
  request = fromJSON(URLencode(url))
  # Assign the packages dataframe
  packages = request$packages
  
  
  
  # Request the next page of results while a next page exists
  # while(!is_null(request$nextPage)){
  while(nrow(packages)<= request$count){
    summary_links = URLencode(paste0(packages$packageLink, "&api_key=", apiGovKey))
    map(summary_links, fromJSON)
    browser()
    next_page_url = paste0(request$nextPage, "&api_key=", apiGovKey)
    next_page = URLencode(next_page_url)
    request = fromJSON(next_page)
    # request = tryCatch(fromJSON(next_page), 
    #                    error = function(e)Sys.sleep(30),
    #                    finally = fromJSON(next_page))
    # browser()
    # Requests tend to fail as we approach 10k results
    # if(nrow(packages)> 9500){
    #   browser()
    # }
    # Append new results to packages
    packages = bind_rows(packages, request$packages)
    
    # A tiny bit of sleep seems to reduce API errors
    Sys.sleep(.5)
  }
  
  return(packages)
}


# Helper functions --------------------------------------------------------


list_flatten_rename = function(list_to_flatten, 
                              name_prefix = "prefix"){
  rename_with(
    .data = as_tibble(list_flatten(list_to_flatten)), 
    .fn = ~str_c(name_prefix, "_", .),
    # Exclude columns which already start with the prefix
    .cols = -starts_with(name_prefix)
  )
}

format_date_api = function(date){
  stopifnot(is.Date(date) || is.timepoint(date))
  format(date, "%Y-%m-%dT%H:%M:%SZ")
}


# XML parsing functions ---------------------------------------------------

# Using the user guide to understand the XML structure
# https://github.com/usgpo/bill-status/blob/main/BILLSTATUS-XML_User_User-Guide.md

#' Parse committee
#'
#' @param committee committee_element
#' 
#' Flatten committee activities, and assign subcommittees to their own tibble
#'
#' @return
#' @export
#'
#' @examples
parse_committee = function(committee){
  # Flatten committee activity data into tibble
  committee_tibbled = map_at(committee, "activities", map, list_flatten) |> 
    map_at("activities", map, as_tibble) |> 
    map_at("activities", list_rbind) |> 
    # Flatten subcommittee data within committee
    map_at("subcommittees", function(subcommittee){
      map(subcommittee, parse_subcommittee) |> 
        list_rbind()
    })
  
  # Remove tibble to flatten then recombine
  committee_df = discard(committee_tibbled, is_tibble) |> 
    list_flatten() |> 
    as_tibble() |> 
    mutate(activities = list(committee_tibbled$activities),
           subcommittees = list(committee_tibbled$subcommittees))
  
  return(committee_df)
}

#' Parse subcommittee
#'
#' @param subcommittee subcommittee element
#' 
#' Flatten subcommittee activities to tibble
#'
#' @return
#' @export
#'
#' @examples
parse_subcommittee = function(subcommittee){
  # List of lists
  # > systemCode
  # > name
  # > activities
  #   > item
  #     > name
  #     > date
  
  # Flatten subcommittee data into tibble
  subcommittee_tibbled = map_at(subcommittee, "activities", map, list_flatten, name_spec = "{outer}") |> 
    map_at("activities", map, as_tibble) |> 
    map_at("activities", list_rbind)
  
  # Remove tibble to flatten then recombine
  discard(subcommittee_tibbled, is_tibble) |> 
    list_flatten() |> 
    as_tibble() |> 
    mutate(activities = list(subcommittee_tibbled$activities))
  
}


#' Parse vote roll
#'
#' @param vote vote_element
#' @param logger logger object
#' @param bill_type bill type
#' @param bill_num bill number
#' 
#' Attempt to parse vote XML files, return empty tibble when reading XML fails
#'
#' @return
#' @export
#'
#' @examples
parse_vote_roll = function(vote, chamber){
  
  clean_url = str_replace(vote, "Votes", "evs")
  
  vote_xml = read_xml(clean_url)
  
  if(!exists("vote_xml")){
    return(tibble())
  }
  # House Votes
  if(chamber == "House"){
    # Extract legislator vote data from XML
    legislators_list = as_list(xml_find_all(vote_xml, "vote-data/recorded-vote"))
    
    # Flatten votes into tibble
    legislator_vote_df = legislators_list |> 
      # Modify one level deeper using map_at to target legislator elements
      map(map_at, "legislator", attributes) |> 
      map(list_flatten) |> 
      map(as_tibble) |> 
      list_rbind() |> 
      janitor::clean_names()
    
    # Vote metadata
    vote_singular_nodes = xml_find_all(vote_xml, "vote-metadata/*[count(./*) = 0]")
    
    vote_df = as_list(vote_singular_nodes) |> 
      # as_list() doesn't retain element names so we set names ourselves
      setNames(xml_name(vote_singular_nodes)) |> 
      list_flatten() |> 
      as_tibble() |> 
      janitor::clean_names() |> 
      # Remove duplicated columns
      select(-any_of(c("congress", "chamber", "session", "legis_num")))
    
    # Party vote totals
    # Extract from XML
    vote_totals_by_party = xml_find_all(vote_xml, "vote-metadata/vote-totals/totals-by-party")
    
    
    party_vote_totals_df = as_list(vote_totals_by_party) |> 
      # Flatten each party list
      map(list_flatten) |> 
      # Convert each list to a tibble
      map(as_tibble) |> 
      # Combine as rows
      list_rbind() |> 
      janitor::clean_names() |> 
      type_convert(col_types = cols(
        party = col_character(),
        yea_total = col_double(),
        nay_total = col_double(),
        present_total = col_double(),
        not_voting_total = col_double()
      ))
    
    vote_df |> 
      rename_with(~str_remove(., "^vote_")) |> 
      mutate(legislator_votes = list(legislator_vote_df),
             party_votes = list(party_vote_totals_df)) |> 
      janitor::clean_names()
    
  } else {
    # Singular nodes
    vote_singular_nodes = xml_find_all(vote_xml, "*[count(./*) = 0]")
    
    singular_df = as_list(vote_singular_nodes) |> 
      set_names(xml_name(vote_singular_nodes)) |> 
      list_flatten() |> 
      as_tibble() |> 
      select(-congress, -session, -congress_year)
    
    # Document
    # vote_document = xml_child(vote_xml, "document") |> 
    #   as_list() |> 
    #   flatten_dfc()
    
    # Amendment
    vote_amendment = xml_child(vote_xml, "amendment") |> 
      as_list() |> 
      list_flatten() |> 
      as_tibble()
    
    # Vote count
    vote_count = xml_child(vote_xml, "count") |> 
      as_list() |> 
      list_flatten() |> 
      as_tibble()
    # Tie breaker
    vote_tie_breaker = xml_child(vote_xml, "tie_breaker") |> 
      as_list() |> 
      list_flatten() |> 
      as_tibble()
    
    # Vote Members
    vote_members = xml_child(vote_xml, "members") |> 
      as_list() |> 
      map(list_flatten) |> 
      map(as_tibble) |> 
      list_rbind()
    
    vote_df = list(
      singular_df, vote_amendment, vote_count, vote_tie_breaker
    ) |> 
      # Remove elements with no data
      # keep(~(nrow(.) > 0)) |> 
      compact() |> 
      list_cbind()
    
    vote_df |> 
      rename_with(~str_remove(., "^vote_")) |> 
      mutate(legislator_votes = list(vote_members)) |> 
      janitor::clean_names()
  }

  
  
  
}

#' Parse action element
#' 
#' @description
#' `parse_action` parses an action XML node from a BILLSTATUS XML file into a dataframe row.  
#'
#' @param action action XML node
#' 
#' @details 
#' To produce a single row tibble, this function separates votes and 
#' committees from each action for two reasons. 1) they can contain multiple child elements
#' and 2) they may not be present.
#' The remaining action elements are flattened into a tibble. Committees and votes are
#' added as list columns when they are present. 
#'
#' @returns 1-row tibble
#' @export
#'
#' @examples
parse_action = function(action){

  # Separate votes and parse the vote roll
  votes = keep_at(action, "recordedVotes")
  committees = keep_at(action, "committees")
  
  # Remove votes
  # Issue: actions which have 2 committee items are parsed as two separate actions
  #       The XML files are inconsistent in having separate action items and separate committee sub-items when
  #       an action is related to two committees
  # Solution: Treat each committee referral as separate action
  actions_df = discard_at(action, c("recordedVotes", "committees")) |> 
    # Flatten source columns, calendar number, and committee data
    map_at(c("sourceSystem", "calendarNumber"), list_flatten) |> 
    # Flatten the list into a df
    list_flatten() |> 
    as_tibble()
  
  # Parse committees
  if(length(committees)>0){
    committees_df = committees |> 
      # Flatten list of committee elements
      map(map, list_flatten) |> 
      # Convert to tibble
      map(map, as_tibble) |> 
      # Combine into rows
      list_flatten() |> 
      list_rbind()
    # Add in committees
    actions_df = mutate(actions_df, committees = list(committees_df))
  }  
  
  # Parse votes
  if(length(votes)>0){
    votes_df = as_tibble(list_flatten(votes$recordedVotes$recordedVote)) |> 
      mutate(vote = map2(url, chamber, parse_vote_roll))
    
    # Add in vote
    actions_df = mutate(actions_df, vote_record = list(votes_df))
  }  
  actions_df
}

parse_amendment = function(amendment){
  map_at(amendment, "actions", function(actions){
      actions %>% 
        modify_at("actions", map_dfr, parse_action) %>% 
        modify_at("actionTypeCounts", flatten_dfc) %>% 
        modify_at("actionByCounts", flatten_dfc)
    }) %>% flatten_dfc()
}

parse_sponsor = function(sponsor){
  map_at(sponsor, "identifiers", list_flatten) |> 
    list_flatten() |> 
    as_tibble()
}

#' XML singular nodes
#'
#' @param xml_node 
#' 
#' Find the XML nodes with no children, removing those which are empty
#'
#' @return
#' @export
#'
#' @examples
xml_singular_nodes = function(xml_node){
  
  # Return child nodes of current node
  child_nodes = xml_children(xml_node)
  
  # Select child nodes with 0 children
  zero_length_child_nodes = child_nodes[xml_length(child_nodes) == 0]
  
  # Keep the nodes which are not empty strings
  keep(zero_length_child_nodes, ~(xml_text(.) != ""))
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
                 sourceSystem_code = col_character(),
                 sourceSystem_name = col_character(),
                 text = col_character(), 
                 type = col_character(), 
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



extract_bill_status = function(xml_file, 
                               nested_attributes = c("committees", "votes", "actions", "sponsors", "cosponsors"),
                               get_votes = T,
                               col_specs = attribute_col_types,
                               log_threshold = "INFO",
                               log_types = c("console")){
  # When using Bill Type and Bill Number columns
  # Before 117th congress: billType and billNumber
  # 117th & After: type and number
  # https://github.com/usgpo/bulk-data/issues/113#issuecomment-1366754669
  # Additional changes:
  # https://github.com/usgpo/bill-status/issues/200#issue-1333965229
  
  # Create logger
  logger = create_logger(log_threshold = log_threshold, 
                         log_types = log_types)
  
  # Read in XML
  xml_raw = read_xml(xml_file)
  bill_xml = xml_child(xml_raw, "bill")
  
  # Isolate nodes with no children
  singletons = xml_find_all(bill_xml, "//bill/*[count(./*) = 0 and not(string-length(.) = 0)]")
  
  # Combine these nodes into dataframe
  bill_df = as_tibble(
    setNames(
      map(singletons, xml_text), 
      xml_name(singletons))
  )

  # Extract other bill-level attributes ----
  ## Policy area
  policy_areas = map_chr(as_list(xml_find_all(bill_xml, "policyArea/name")),
                         list_simplify)
  ## Subjects
  bill_subjects = map_chr(as_list(xml_find_all(bill_xml, "subjects/legislativeSubjects/item/name")),
                          list_simplify)
  ## Summaries
  bill_summaries = map(as_list(xml_find_all(bill_xml, "summaries/summary")), list_flatten) |> 
    map(as_tibble) |> 
    list_rbind()
  
  ## Titles
  bill_titles = map(as_list(xml_find_all(bill_xml, "titles/item")), list_flatten) |>
    map(as_tibble) |> 
    list_rbind()
  
  ## Text versions
  bill_text_versions = map(as_list(xml_find_all(bill_xml, "textVersions/item")),
                           map_at, "formats", list_flatten) |> 
    map(list_flatten) |> 
    map(list_flatten) |> 
    map(as_tibble) |> 
    list_rbind()
  
  #TODO: Parse bill text version format data
  ## Latest action
  latest_action = as_list(xml_find_all(bill_xml, "latestAction")) |> 
    list_flatten() |> 
    list_flatten() |> 
    as_tibble() |> 
    rename_with(~str_c("latestAction_", .))
  
  # bill_df = bill_df |> 
  #   mutate(policy_areas = list(policy_areas),
  #          legislative_subjects = list(bill_subjects),
  #          bill_summaries = list(bill_summaries),
  #          bill_titles = list(bill_titles),
  #          bill_text_versions = list(bill_text_versions)) |> 
  #   bind_cols(latest_action)
  # Combine bill-level attributes
  bill_df$policy_areas = list(policy_areas)
  bill_df$legislative_subjects = list(bill_subjects)
  bill_df$summaries = list(bill_summaries)
  bill_df$titles = list(bill_titles)
  bill_df$text_versions = list(bill_text_versions)
  
  
  bill_df = bind_cols(bill_df, latest_action) |> 
    # While files are not fully reprocessed, remove bill prefix
    rename_with(~str_remove(., "^bill"))

  log_debug(logger, 
           bill_type = bill_df$type,
           bill_num = bill_df$number,
           "Reading XML")


  # Committees ---------
  log_debug(logger, 
           bill_type = bill_df$type,
           bill_num = bill_df$number,
           "Parsing committees")
  
  committees = xml_find_all(bill_xml, "//bill/committees/item")
  
  if("committees" %in% nested_attributes && length(committees)>0){

    # Coerce nodes to list
    committees_list = as_list(committees)
    
    committees_df = janitor::clean_names( list_rbind(map(committees_list, parse_committee)) )
    
    bill_df$committees = list(committees_df)
    
  } else {
    bill_df$committees = list(tibble())
  }
  
  # Votes ---------
  # log_debug(logger, 
  #          bill_type = bill_df$type,
  #          bill_num = bill_df$number,
  #          "Parsing votes")
  # 
  # votes_node = xml_find_all(bill_xml, "//bill/recordedVotes/recordedVote")
  # 
  # if("votes" %in% nested_attributes && length(votes_node)>0 && get_votes){
  #   
  #   
  #   # Coerce nodes to list
  #   votes_list = as_list(votes_node)
  # 
  #   votes_df = select(map_dfr(votes_list, flatten_dfc), -any_of(c("congress")))
  # 
  #   # Add Vote tallies
  #   vote_rolls_df = mutate(votes_df,
  #                          vote_roll = map2(url, chamber, parse_vote_roll, 
  #                            logger = logger, 
  #                            bill_type = bill_df$type,
  #                            bill_num = bill_df$number),
  #            roll_found = map_lgl(vote_roll, ~(nrow(.) > 0))) |> 
  #     janitor::clean_names()
  #   
  #   bill_df$house_votes = list(filter(vote_rolls_df, chamber == "House"))
  #   bill_df$senate_votes = list(filter(vote_rolls_df, chamber == "Senate"))
  # } else {
  #   bill_df$house_votes = list(tibble())
  #   bill_df$senate_votes = list(tibble())
  # }
  
  # Actions ---------
  log_debug(logger, 
           bill_type = bill_df$type,
           bill_num = bill_df$number,
           "Parsing actions")
  
  bill_actions = xml_find_all(bill_xml, "//bill/actions/item")
  
  if("actions" %in% nested_attributes && length(bill_actions)>0){

    # Action counts not found in Senate bill 3271
    # bill_action_counts = as_list(xml_find_all(bill_xml, "//bill/actions/*[not(self::item)]")) |> 
    #   map_dfc(flatten_dfc) |> 
    #   rename_with(.cols = everything(), ~str_c("actions_", .)) |> 
    #   pivot_longer(everything(), names_to = "action", names_prefix = "actions_", values_to = "count")
    
    # Coerce nodes to list
    actions_df = as_list(bill_actions) |> 
      map(parse_action) |> 
      list_rbind() |> 
      type_convert(col_types = col_specs$actions) |> 
      janitor::clean_names()
    
    bill_df$actions = list(actions_df)
    
    # bill_df$action_counts = list(type_convert(bill_action_counts,
    #                                           col_types = cols(action = col_character(), count = col_integer())))
  } else {
    bill_df$actions = list(tibble())
  }
  
  # Amendments ---------
  # log_debug(logger, 
  #          bill_type = bill_df$type,
  #          bill_num = bill_df$number,
  #          "Parsing amendments")
  # amendments_node = bill_nodesets[["amendments"]]
  # if(xml_length(amendments_node)>0){
  #   browser()
  #   bill_amendments = xml_find_all(amendments_node, "amendment")
  #   # Coerce nodes to list
  #   amendments_df = map(bill_amendments, as_list) |> 
  #     map_dfr(parse_action)
  #   
  #   bill_df$amendments = list(amendments_df)
  # } else {
  #   bill_df$amendments = list(tibble())
  # }
  
  # Sponsors ---------
  log_debug(logger, 
           bill_type = bill_df$type,
           bill_num = bill_df$number,
           "Parsing sponsors")
  
  bill_sponsors = xml_find_all(bill_xml, "//bill/sponsors/item")
  
  if("sponsors" %in% nested_attributes && length(bill_sponsors)>0){

    # Coerce nodes to list
    sponsors_df = as_list(bill_sponsors) |> 
      map(parse_sponsor) |> 
      list_rbind() |> 
      janitor::clean_names()
    
    bill_df$sponsors = list(sponsors_df)
  } else {
    bill_df$sponsors = list(tibble())
  }
  
  
  # Cosponsors ---------
  log_debug(logger, 
           bill_type = bill_df$type,
           bill_num = bill_df$number,
           "Parsing cosponsors")
  
  bill_cosponsors = xml_find_all(bill_xml, "//bill/cosponsors/item")
  
  if("cosponsors" %in% nested_attributes && length(bill_cosponsors)>0){
    
    # Coerce nodes to list
    cosponsors_df = as_list(bill_cosponsors) |> 
      map(parse_sponsor) |> 
      list_rbind() |> 
      janitor::clean_names()
    
    bill_df$cosponsors = list(type_convert(cosponsors_df, col_types = col_specs$cosponsors))
  } else {
    bill_df$cosponsors = list(tibble())
  }
  finished_df = as_tibble(bill_df) |> 
    janitor::clean_names() |> 
    # Combine bill type and number to create an ID
    unite(bill_id, type, number, sep = "-", remove = F)
  
  log_info(logger, 
            bill_type = bill_df$type,
            bill_num = bill_df$number,
            "Complete")
  
  mutate(finished_df,
         across(ends_with("date"), as_datetime)
         )
}

trunc_columns = function(df){
  df %>% 
    mutate(across(where(is_character), str_trunc, width = 32))
}


# Cleaning functions ------------------------------------------------------

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
    filter(replace_na(action_code != "Intro-H", T))
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
code_actions = function(actions_df, 
                        action_codes,
                        action_order = c("IntroReferral", "Committee", "Floor", 
                                         "Discharge", "President", "BecameLaw")){
  # TODO: 
  cols <- c(action_time = NA_POSIXct_)
  
  # Ensure actionTime is present
  actions_new_cols = add_column(actions_df, !!!cols[setdiff(names(cols), names(actions_df))]) %>% 
    # Create action timestamp
    mutate(action_ts = make_datetime(year = year(action_date), 
                                     month = month(action_date), 
                                     day = day(action_date), 
                                     hour = coalesce(hour(action_time), 0), 
                                     min = coalesce(minute(action_time), 0), 
                                     sec = coalesce(second(action_time), 0), 
                                     tz = "US/Eastern"),
           action_type_fct = fct_explicit_na(
             factor(action_type, 
                    levels = action_order, 
                    ordered = T),
             na_level = "(Missing Action Type)"
           ),
           action_source_name = factor(action_source_name,
                                       levels = c("Library of Congress", "House floor actions", 
                                                  "House committee actions", "Senate"),
                                       ordered = T))
  
  # Join in action codes
  left_join(actions_new_cols, 
            action_codes, by = "action_code") %>% 
    number_actions() %>% 
    # Create boolean for whether bill became law
    mutate(became_law = ("BecameLaw" %in% action_type), .after = action_time) %>% 
    ungroup() %>% 
    select(-action_type_fct, -action_time)
}

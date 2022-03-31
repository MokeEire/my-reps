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

log_layout = function(level, ...){
  function(level,
           ..., bill_type, bill_num){
    msg = paste0(..., collapse = "")
    
    sprintf("%s | Bill #%s | %s | %s\n", bill_type, bill_num,
            log4r:::fmt_current_time("%Y-%m-%d %H:%M:%S"),
            msg)
  }
}

create_logger = function(directory = here::here("logs")){
  log_file = here::here(here::here("logs"), paste0("log-", format(lubridate::now(), "%Y-%m-%d--%H-%M-%S"), ".txt"))
  file.create(log_file)
  
  log4r::logger(
    appenders = list(
      log4r::file_appender(log_file, append = T, layout = log_layout()),
      log4r::console_appender(layout = log_layout())
    )
  )
}

log_info = function(logger, ...){
  if(logger$threshold > log4r:::INFO)
    return(invisible(NULL))
  for (appender in logger$appenders){
    appender(level="INFO", ...)
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
    read_xml(summary_json$xmlLink)
  }
  
  
}

getPublished = function(dateIssuedStartDate, dateIssuedEndDate,
                        startingRecord = 0, numRecords = 20,
                        collections, congress,
                        docClass){
  # If end date is provided, format it
  if(missing(dateIssuedEndDate)){
    end_date = ""
  } else {
    end_date = paste0("/", format(dateIssuedEndDate, "%Y-%m-%dT%H:%M:%SZ"))
  }
  
  # Construct URL
  url = paste0(
    # Root
    "https://api.govinfo.gov/published/",
    # Start and end dates
    format(dateIssuedStartDate, "%Y-%m-%dT%H:%M:%SZ"), end_date,
    # Record indexing
    "?offset=", startingRecord, "&pageSize=", numRecords, 
    # Collection
    "&collection=",collections,
    if_else(missing(congress), "", paste0("&congress=", congress)),
    if_else(missing(docClass), "", paste0("&docClass=", docClass)),
    # API key
    "&api_key=", apiGovKey
  )
  # encode the URL with characters for each space.
  full_url <- URLencode(url)
  
  request = fromJSON(full_url)
  packages = request$packages
  while(!is_null(request$nextPage)){
    next_page = URLencode(request$nextPage)
    request = fromJSON(next_page)
    # if(nrow(packages)> 9500){
    #   browser()
    # }
    packages = bind_rows(packages, request$packages)
    Sys.sleep(2)
  }
  packages
}

silent_convert = function(df, ...){
  suppressMessages(readr::type_convert(df, ...))
}

parse_committee = function(committee){
  committee_tibbled = committee %>% 
    modify_at("activities", map_dfr, ~rename_with(flatten_dfc(.x), ~str_c("committee_activity_", .))) %>% 
    modify_at("subcommittees", function(subcommittee){
      map_dfr(subcommittee, parse_subcommittee)
    })
  
  committee_df = keep(committee_tibbled, negate(is_tibble)) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c("committee_", .)) %>% 
    mutate(committee_activities = list(committee_tibbled$activities),
           subcommittee_activities = list(committee_tibbled$subcommittees))
  
  return(committee_df)
}

parse_subcommittee = function(subcommittee){
  modify_at(subcommittee, "activities", map_dfr, ~rename_with(flatten_dfc(.x), ~str_c("activity_", .))) %>% 
    flatten_dfc() %>% 
    rename_with(~str_c("subcommittee_", .))
}

parse_votes = function(recorded_votes){
  modify_at(recorded_votes, "activities", map_dfr, ~rename_with(flatten_dfc(.x), ~str_c("activity_", .))) %>% 
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
  # 
  action %>% 
    modify_at("sourceSystem", ~rename_with(flatten_dfc(.x), ~str_c("sourceSystem_", .))) %>% 
    modify_at("committees", function(committee){
      map_dfr(committee, ~rename_with(flatten_dfc(.x), ~str_c("committee_", .)))
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
    modify_at("identifiers", ~rename_with(flatten_dfc(.x), ~str_c("identifiers_", .))) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c(role, "_", .), .cols = -starts_with(role))
}

extract_bill_status = function(xml_file, 
                               base_attributes = c("congress", "title", "billType", "billNumber", "originChamber", "policyArea", 
                                                   "introducedDate","createDate", "updateDate"),
                               nested_attributes = c("committees", "votes", "actions", "sponsors", "cosponsors")){
  logger = create_logger()
  
  bill_xml = read_xml(xml_file) %>% 
    xml_child("bill")
  # message("Bill #: ",xml_find_all(bill_xml, "billNumber") %>% xml_text())
  
  base_attr_list = map(base_attributes, ~xml_find_all(bill_xml, xpath = .))
  
  bill_df = set_names(base_attr_list, map_chr(base_attr_list, xml_name)) %>% 
    map_dfc(xml_text)
  
  latest_action = xml_find_all(bill_xml, "latestAction") %>% 
    as_list() %>% 
    map(flatten_dfc) %>% 
    map_dfr(~rename_with(.x, ~str_c("latestAction_", .)))
  
  subjects_list = xml_find_all(bill_xml, "subjects/billSubjects/legislativeSubjects/item") %>% 
    xml_text()
  
  subjects = tibble(subjects = list(subjects_list))
  
  bill_df = bind_cols(bill_df, latest_action, subjects)
  
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Reading XML")

  xpaths = bill_xml %>% 
    xml_children() %>% 
      map_chr(xml_path)
  
  bill_nodesets = map(xpaths, ~ xml_find_all(bill_xml, xpath = .x)) %>% 
    set_names(map_chr(., xml_name))
  
  # browser()
  # Committees ---------
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing committees")
  
  bill_committees = xml_find_all(bill_nodesets[["committees"]], "billCommittees")
  
  if("committees" %in% nested_attributes && xml_length(bill_committees)>0){

    committees = xml_find_all(bill_committees, "item")
    # Coerce nodes to list
    committees_list = map(committees, as_list)
    
    committees_df = map_dfr(committees_list, parse_committee)
    
    bill_df$committees = list(silent_convert(committees_df))
    
  } else {
    bill_df$committees = list(tibble())
  }
  
  # Votes ---------
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing votes")
  
  votes_node = bill_nodesets[["recordedVotes"]]
  if("votes" %in% nested_attributes && xml_length(votes_node)>0){
    
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
    
    bill_df$votes = list(silent_convert(vote_rolls_df))
  } else {
    bill_df$votes = list(tibble())
  }
  
  # Actions ---------
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing actions")
  actions_node = bill_nodesets[["actions"]]
  if("actions" %in% nested_attributes && xml_length(actions_node)>0){
    bill_actions = xml_find_all(actions_node, "item")
    # Coerce nodes to list
    actions_df = map(bill_actions, as_list) %>% 
      map_dfr(parse_action)

    bill_df$actions = list(silent_convert(actions_df))
  } else {
    bill_df$actions = list(tibble())
  }
  
  # Amendments ---------
  # log_info(logger, 
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
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing sponsors")
  
  bill_sponsors = xml_find_all(bill_nodesets[["sponsors"]], "item")
  if("sponsors" %in% nested_attributes && xml_length(bill_sponsors)>0){

    # Coerce nodes to list
    sponsors_df = map(bill_sponsors, as_list) %>% 
      map_dfr(parse_sponsor)
    
    bill_df$sponsors = list(silent_convert(sponsors_df))
  } else {
    bill_df$sponsors = list(tibble())
  }
  
  
  # Cosponsors ---------
  log_info(logger, 
           bill_type = bill_df$billType,
           bill_num = bill_df$billNumber,
           "Parsing cosponsors")
  cosponsors_node = bill_nodesets[["cosponsors"]]
  if("cosponsors" %in% nested_attributes && xml_length(cosponsors_node)>0){
    
    bill_cosponsors = xml_find_all(cosponsors_node, "item")
    # Coerce nodes to list
    cosponsors_df = map(bill_cosponsors, as_list) %>% 
      map_dfr(parse_sponsor, role = "cosponsor")
    
    bill_df$cosponsors = list(silent_convert(cosponsors_df))
  } else {
    bill_df$cosponsors = list(tibble())
  }
  
  as_tibble(bill_df) %>% 
    silent_convert()
}

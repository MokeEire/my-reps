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
                        collections){
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
    # API key
    "&api_key=", apiGovKey
  )
  # encode the URL with characters for each space.
  full_url <- URLencode(url)
  fromJSON(full_url)
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

parse_vote_roll = function(vote){
  
  vote_xml = read_xml(vote)
  
  vote_data = xml_find_all(vote_xml, "vote-data")
  
  vote_list = as_list(vote_data)
  
  flatten_dfr(vote_list) %>% 
    unnest(everything())
}

parse_action = function(action){
  action %>% 
    modify_at("sourceSystem", ~rename_with(flatten_dfc(.x), ~str_c("sourceSystem_", .))) %>% 
    modify_at("committees", function(committee){
      map_dfr(committee, ~rename_with(flatten_dfc(.x), ~str_c("committee_", .)))
    }) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c("action_", .), .cols = -starts_with("action"))
}

parse_sponsor = function(sponsor, role = "sponsor"){
  sponsor %>% 
    modify_at("identifiers", ~rename_with(flatten_dfc(.x), ~str_c("identifiers_", .))) %>% 
    flatten_dfc() %>% 
    rename_with(.fn = ~str_c(role, "_", .), .cols = -starts_with(role))
}

extract_bill_status = function(xml_file, 
                               base_attributes = c("billNumber", "title", "billType", "originChamber", 
                                                   "introducedDate", "congress","createDate", "updateDate")){
  
  bill_xml = read_xml(xml_file) %>% 
    xml_child("bill")
  
  # message("Bill #: ",xml_find_all(bill_xml, "billNumber") %>% xml_text())
  
  base_attr_list = map(base_attributes, ~xml_find_all(bill_xml, xpath = .))
  
  bill_df = set_names(base_attr_list, map_chr(base_attr_list, xml_name)) %>% 
    map_dfc(xml_text)

  xpaths = bill_xml %>% 
    xml_children() %>% 
      map_chr(xml_path)
  
  bill_nodesets = map(xpaths, ~ xml_find_all(bill_xml, xpath = .x)) %>% 
    set_names(map_chr(., xml_name))
  
  # browser()

  bill_committees = xml_find_all(bill_nodesets[["committees"]], "billCommittees")
  
  if(xml_length(bill_committees)>0){

    committees = xml_find_all(bill_committees, "item")
    # Coerce nodes to list
    committees_list = map(committees, as_list)
    
    committees_df = map_dfr(committees_list, parse_committee)
    
    bill_df$committees = list(silent_convert(committees_df))
    
  } else {
    bill_df$committees = list(tibble())
  }
  
  
  if(xml_length(bill_nodesets[["recordedVotes"]])>0){
    
    bill_votes = xml_find_all(bill_nodesets[["recordedVotes"]], "recordedVote")
    # Coerce nodes to list
    votes_list = as_list(bill_votes)

    votes_df = map_dfr(votes_list, flatten_dfc)

    # Add Vote tallies
    vote_rolls_df = votes_df %>% 
      mutate(vote_roll = map(url, parse_vote_roll))
    
    bill_df$votes = list(silent_convert(vote_rolls_df))
  } else {
    bill_df$votes = list(tibble())
  }
  
  
  if(xml_length(bill_nodesets[["actions"]])>0){
    bill_actions = xml_find_all(bill_nodesets[["actions"]], "item")
    # Coerce nodes to list
    actions_df = map(bill_actions, as_list) %>% 
      map_dfr(parse_action)

    bill_df$actions = list(silent_convert(actions_df))
  } else {
    bill_df$actions = list(tibble())
  }
  
  bill_sponsors = xml_find_all(bill_nodesets[["sponsors"]], "item")
  if(xml_length(bill_sponsors)>0){

    # Coerce nodes to list
    sponsors_df = map(bill_sponsors, as_list) %>% 
      map_dfr(parse_sponsor)
    
    bill_df$sponsors = list(silent_convert(sponsors_df))
  } else {
    bill_df$sponsors = list(tibble())
  }
  
  
  # Cosponsors
  if(xml_length(bill_nodesets[["cosponsors"]])>0){
    
    bill_cosponsors = xml_find_all(bill_nodesets[["cosponsors"]], "item")
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

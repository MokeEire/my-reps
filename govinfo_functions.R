getPackages = function(packageId, summary = F, xml = F){
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

read_bill_xml = function(
  xml_link
  ){
  bill_xml = read_xml(xml_link)
  
  bill_item = xml_child(bill_xml, "bill")
  
  xml_df = xmlToDataFrame(
    xmlTreeParse(bill_xml, useInternalNodes = T), 
    nodes = getNodeSet(bill_xml, "//bill")) %>% 
      select(billNumber, title, createDate:congress, policyArea)
  
  # Extract xpaths
  xpaths = bill_item %>% 
    xml_children() %>% 
      map_chr(xml_path)
  
  bill_nodesets = map(xpaths, ~ xml_find_all(bill_item, xpath = .x)) %>% 
    set_names(map_chr(., xml_name))
  
  nested_attributes = bill_nodesets %>% 
    discard(~(xml_length(.) == 0))
  
  # Committees
  # List of bill committees
  bill_committees = nested_attributes[["committees"]] %>% 
    xml_find_all("billCommittees/item")
  
  committees_list = bill_committees %>% 
    # retrieve each committee's xpath
    map_chr(xml_path) %>% 
    # Find the nodes based on each xpath
    map(~xml_find_all(sample_bill, .x)) %>% 
    # Coerce nodes to list
    map(as_list)
  
  committees_dfs = committees_list %>% 
    map_depth(2, function(committee){
      committee %>% 
        modify_at("activities", map, ~rename_with(flatten_dfc(.x), ~str_c("activity_", .))) %>% 
        modify_at("subcommittees", function(subcommittee){
          modify(subcommittee, function(item){
            modify_at(item, "systemCode", flatten_chr) %>% 
              modify_at("name", flatten_chr) %>% 
              modify_at("activities", map, ~rename_with(flatten_dfc(.x), ~str_c("activity_", .)))
          }) %>% 
            map(~rename_with(flatten_dfc(.x), ~str_c("subcommittee_", .)))
        }) %>% 
        flatten_dfc() %>% 
        rename_with(.fn = ~str_c("committee_", .), .cols = -starts_with("subcommittee")) %>% 
        select(starts_with("committee"), starts_with("subcommittee"))
    }) %>% 
    flatten_dfr()
  
  # Actions
  # List of bill committees
  bill_actions = nested_attributes[["actions"]] %>% 
      xml_find_all("item")
  
  actions_df = bill_actions %>% 
    # Coerce nodes to list
    map(as_list) %>% 
    map_dfr(function(action){
      action %>% 
        modify_at("sourceSystem", ~rename_with(flatten_dfc(.x), ~str_c("sourceSystem_", .))) %>% 
        modify_at("committees", function(committee){
          modify(committee, function(item){
            modify_at(item, "systemCode", flatten_chr) %>% 
              modify_at("name", flatten_chr)
          }) %>% 
            map(~rename_with(flatten_dfc(.x), ~str_c("committee_", .)))
        }) %>% 
        flatten_dfc() %>% 
        rename_with(.fn = ~str_c("action_", .), .cols = -starts_with("action"))
    })
  
  # Sponsors
  sponsors_df = nested_attributes[["sponsors"]] %>% 
    xml_find_all("item") %>% 
    # Coerce nodes to list
    map(as_list) %>% 
    map_dfr(function(action){
      action %>% 
        modify_at("identifiers", ~rename_with(flatten_dfc(.x), ~str_c("identifiers_", .))) %>% 
        flatten_dfc() %>% 
        rename_with(.fn = ~str_c("sponsor_", .), .cols = -starts_with("sponsor"))
    })
  
  cosponsors_df = nested_attributes[["cosponsors"]] %>% 
    xml_find_all("item") %>% 
      # Coerce nodes to list
      map(as_list) %>% 
      map_dfr(function(cosponsor){
        cosponsor %>% 
          modify_at("identifiers", ~rename_with(flatten_dfc(.x), ~str_c("identifiers_", .))) %>% 
          flatten_dfc() %>% 
          rename_with(.fn = ~str_c("cosponsor_", .), .cols = -starts_with("cosponsor"))
      })
}
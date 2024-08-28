actions_table = function(id, 
                         action_data,
                         bill_data){
  actions <- action_data[action_data$bill_id == bill_data$bill_id[id],
                         c("action_type", "actionDate", "actionTime", 
                           "action_text", "actionCode","action_source_name",
                           #"action_source_code", 
                           "action_committee_name"
                           #, "action_committee_systemCode"
                         )]
  htmltools::div(
    style = "padding: 1rem",
    reactable(actions, 
              columns = list(
                actionDate = colDef(
                  name = "Action Date"
                ),
                action_text = colDef(
                  name = "Action Text", 
                  minWidth = 320
                ),
                action_type = colDef(
                  name = "Action Type"
                ),
                actionTime = colDef(
                  name = "Action Time"
                ),
                actionCode = colDef(
                  name = "Action Code"
                ),
                # action_source_code = colDef(
                #   name = "Source Code"
                #   ),
                action_source_name = colDef(
                  name = "Source Name"
                ),
                # action_committee_systemCode = colDef(
                #   name = "Source Code"
                #   ),
                action_committee_name = colDef(
                  name = "Committee"
                )
              ),
              # Assign Column Groups
              # columnGroups = list(
              #   colGroup(name = "Action Source", 
              #            columns = c("action_source_code", "action_source_name")),
              #   colGroup(name = "Action Committee", 
              #            columns = c("action_committee_systemCode",
              #                        "action_committee_name"))
              # ), 
              theme = moke_rt(), compact = T, pagination = F)
  )
}
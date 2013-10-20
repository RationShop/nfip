# read FEMA NFIP policies from (Policy & Claim Statistics for Flood Insurance)
# http://www.fema.gov/policy-claim-statistics-flood-insurance/policy-claim-statistics-flood-insurance/policy-claim-13

Fn_Analyze_Policies <- function(raw_policies_file = "nfip_policies_raw.txt", 
                                output_policies_file = "formatted_policies_all.txt", 
                                output_policies_aggregated_file = "formatted_policies_county.txt") {  

  if(file.exists("formatted_policies_county.txt")) {
    cat("reading policies by county file dated - ", 
        as.character(file.info("formatted_policies_county.txt")$mtime), 
        "\n")
    out_policies_agg <- read.delim("formatted_policies_county.txt", as.is = TRUE)
    
    return (out_policies_agg)
    
  } else {  
    require(stringr)
    
    # read data on policies
    # data from http://bsa.nfipstat.fema.gov/reports/1011.htm
    cat("reading raw data file dated - ", 
        as.character(file.info("nfip_policies_raw.txt")$mtime), 
        "\n")
    
    data_policies <- readLines(raw_policies_file)
    
    # not formatted for easy use in R
    # all the info related to counties has a fixed width of 116
    line_chars <- sapply(data_policies, FUN = nchar, USE.NAMES = FALSE)
    line_ids   <- seq(1:length(data_policies))
    data_info  <- data.frame(line_chars = line_chars, 
                             line_ids = line_ids)
    table(data_info$line_chars)
    # extract county data
    data_info <- subset(data_info, line_chars == 116)
    policy_data <- data_policies[data_info$line_ids]
    
    # lines where state totals are given
    state_breaks <- grep(" Total for ", policy_data)
    state_breaks <- c(0, state_breaks)
    
    # identify and clean up data for each state
    out_policies <- NULL # all policies for each state
    
    for (eachState in 1:(length(state_breaks) - 1)) {
      policy_state <- policy_data[(state_breaks[eachState] + 1): 
                                    (state_breaks[eachState + 1] - 1)]
      # remove irrelvant info
      junk_lines   <- grep("  -----------", policy_state)
      policy_state <- policy_state[-junk_lines]
      junk_lines   <- grep("  County Name", policy_state)
      policy_state <- policy_state[-junk_lines]
      
      # get county name (1-34), community name(35-67), policies in force(68-75), 
      #insurance in force(76-98), written premium in force(99-116)
      cnty_name <- substr(policy_state, 1, 34)
      comm_name <- substr(policy_state, 35, 67)
      pol_force <- substr(policy_state, 68, 75)
      ins_force <- substr(policy_state, 76, 98)
      wri_force <- substr(policy_state, 99, 116)
      
      # clean up and reformat each of the above variables
      
      # fill in the blanks for county names  
      cnty_name_miss <- grep(paste(rep(c(" "), 34), collapse = ""), cnty_name)
      for (eachPt in cnty_name_miss) {
        if (eachPt > 1) {
          cnty_name[eachPt] <- cnty_name[eachPt -1]
        } else {
          cnty_name[eachPt] <- NA
        }
      }  
      cnty_name <- str_trim(cnty_name)
      
      # remove whitespaces, commas, convert to numeric
      Fn_Remove_Space_Comma <- function(x) {
        x <- gsub(",", "", str_trim(x))
        x <- as.numeric(x)
        return (x)
      }
      pol_force <- Fn_Remove_Space_Comma(pol_force)
      ins_force <- Fn_Remove_Space_Comma(ins_force)
      wri_force <- Fn_Remove_Space_Comma(wri_force)
      
      # add name of state, from record after the last record of the state
      # record has "Total for xxxxxxxxxxx"
      state_name <- policy_data[state_breaks[eachState + 1]]
      state_name <- str_trim(substr(state_name, 1, 34))
      state_name <- gsub("Total for ", "", state_name)
      
      # data frame with all of the state's data
      out_state <- data.frame(state = state_name,
                              county = cnty_name,
                              community = comm_name,
                              policies = pol_force,
                              insurance = ins_force,
                              premium = wri_force)
      # update output
      out_policies <- rbind(out_policies, out_state)    
    }
    
    # write data
    # convert state names to upper case, consistent with claims data
    out_policies$state <- toupper(out_policies$state)
    out_policies <- out_policies[order(out_policies$state), ]
    write.table(out_policies,
                file = output_policies_file,
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                na = "")
    
    # aggregate state data by county
    # consider only the 50 states; remove NAs
    out_policies <- na.omit(out_policies)
    out_policies <- subset(out_policies, 
                           state %in% toupper(c(state.name, "District Columbia")))
    out_policies <- droplevels(out_policies)
    out_policies_agg <- aggregate(cbind(policies, insurance, premium) ~ state * county, 
                                  data = out_policies, 
                                  FUN = sum)
    
    # write data
    out_policies_agg <- out_policies_agg[order(out_policies_agg$state), ]
    write.table(out_policies_agg,
                file = output_policies_aggregated_file,
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                na = "")
    
    return (out_policies_agg)
  }
}

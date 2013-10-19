# read FEMA NFIP policies and claims stats from 
# (Policy & Claim Statistics for Flood Insurance)
# http://www.fema.gov/policy-claim-statistics-flood-insurance/policy-claim-statistics-flood-insurance/policy-claim-13

require(stringr)

# read data on policies
# data from http://bsa.nfipstat.fema.gov/reports/1011.htm
data_policies <- readLines("nfip_policies.txt")

# read data on claims
# data from http://bsa.nfipstat.fema.gov/reports/1040.htm - claims

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
out_policies <- NULL # data frame to store policies by county for each state
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
  
  # remove whitespaces and commas
  Fn_Remove_Space_Comma <- function(x) {
    x <- str_trim(x)
    x <- gsub(",", "", x)
    x <- as.numeric(x)
    return (x)
  }
  pol_force <- Fn_Remove_Space_Comma(pol_force)
  ins_force <- Fn_Remove_Space_Comma(ins_force)
  wri_force <- Fn_Remove_Space_Comma(wri_force)
  
  # add name of state, from record after the last record of the state
  # record has "Total for xxxxx"
  state_name <- policy_data[state_breaks[eachState + 1]]
  state_name <- str_trim(substr(state_name, 1, 34))
  state_name <- unlist(strsplit(state_name, " "))[3]
  
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


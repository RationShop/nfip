# read FEMA NFIP claims stats from (Policy & Claim Statistics for Flood Insurance)
# http://www.fema.gov/policy-claim-statistics-flood-insurance/policy-claim-statistics-flood-insurance/policy-claim-13

Fn_Analyze_Claims <- function(raw_claims_file = "nfip_claims_raw.txt", 
                              output_claims_file = "formatted_claims_all.txt", 
                              output_claims_aggregated_file = "formatted_claims_county.txt") {

  
  if(file.exists("formatted_claims_county.txt")) {
    cat("reading claims by county file dated - ", 
        as.character(file.info("formatted_claims_county.txt")$mtime), 
        "\n")
    out_claims_agg <- read.delim("formatted_claims_county.txt", as.is = TRUE)
    
    return (out_claims_agg)
    
  } else {

    require(stringr)
    
    # read data on claims
    # data from http://bsa.nfipstat.fema.gov/reports/1040.htm - claims
    cat("reading raw data file dated - ", 
        as.character(file.info("nfip_claims_raw.txt")$mtime), 
        "\n")
    
    data_claims <- readLines(raw_claims_file)
    
    # not formatted for easy use in R
    # all the info related to counties has a fixed width of 128
    line_chars <- sapply(data_claims, FUN = nchar, USE.NAMES = FALSE)
    line_ids   <- seq(1:length(data_claims))
    data_info  <- data.frame(line_chars = line_chars, 
                             line_ids = line_ids)
    table(data_info$line_chars)
    # extract county data
    data_info <- subset(data_info, line_chars == 128)
    claims_data <- data_claims[data_info$line_ids]
    
    # lines where state totals are given
    state_breaks <- grep(" TOTAL FOR ", claims_data)
    state_breaks <- c(0, state_breaks)
    
    # identify and clean up data for each state
    out_claims <- NULL # all claims for each state
    
    for (eachState in 1:(length(state_breaks) - 1)) {
      claims_state <- claims_data[(state_breaks[eachState] + 1): 
                                  (state_breaks[eachState + 1] - 1)]
      # remove irrelvant info
      junk_lines   <- grep("       -------------------------", claims_state)
      claims_state <- claims_state[-junk_lines]
      
      # get county name (1-34), community name(35-66), total losses(67-78), 
      # closed losses(79-89), open losses(90-100), cwop losses(101-111), 
      # total payments (112-128)
      cnty_name <- substr(claims_state, 1, 34)
      comm_name <- substr(claims_state, 35, 66)
      total_loss  <- substr(claims_state, 67, 78)
      closed_loss <- substr(claims_state, 79, 89)
      open_loss   <- substr(claims_state, 90, 100)
      cwop_loss   <- substr(claims_state, 101, 111)
      total_pay   <- substr(claims_state, 112, 128)  
      
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
      total_loss  <- Fn_Remove_Space_Comma(total_loss)
      closed_loss <- Fn_Remove_Space_Comma(closed_loss)
      open_loss   <- Fn_Remove_Space_Comma(open_loss)
      cwop_loss   <- Fn_Remove_Space_Comma(cwop_loss)
      total_pay   <- Fn_Remove_Space_Comma(total_pay)
      
      # add name of state, from record after the last record of the state
      # record has "Total for xxxxxxxxxxx"
      state_name <- claims_data[state_breaks[eachState + 1]]
      state_name <- str_trim(substr(state_name, 1, 34))
      state_name <- gsub("TOTAL FOR ", "", state_name)
    
      # data frame with all of the state's data
      out_state <- data.frame(state = state_name,
                              county = cnty_name,
                              community = comm_name,
                              total_loss = total_loss,
                              closed_loss = closed_loss,
                              open_loss = open_loss,
                              cwop_loss = cwop_loss,
                              total_pay = total_pay)
      # update output
      out_claims <- rbind(out_claims, out_state)    
    }
    
    # write data
    out_claims <- out_claims[order(out_claims$state), ]
    write.table(out_claims,
                file = output_claims_file,
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                na = "")
    
    # aggregate data by county for each state
    # consider only the 50 states; remove NAs
    out_claims <- na.omit(out_claims)
    out_claims <- subset(out_claims, 
                         state %in% toupper(c(state.name, "District Columbia")))
    out_claims <- droplevels(out_claims)
    out_claims_agg <- aggregate(cbind(total_loss, closed_loss, open_loss, cwop_loss, 
                                      total_pay) ~ state * county,                                   
                                  data = out_claims, 
                                  FUN = sum)
    
    # write data
    out_claims_agg <- out_claims_agg[order(out_claims_agg$state), ]
    write.table(out_claims_agg,
                file = output_claims_aggregated_file,
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                na = "")
    
    
    return (out_claims_agg)
  }
}

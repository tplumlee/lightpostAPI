# function for checking whether date args are properly formatted
valid_date <- function(str_date) {
  str_date <- ifelse(
    nchar(str_date)==7, 
    paste0(str_date, "-01"),
    str_date
  )

  valid <- !is.na(
    as.Date(
      str_date,
      format = "%Y-%m-%d",
      optional = TRUE
    )
  )

  return(valid)
}

# initialize rate_limit with first request of new session or after 1 second wait period
get_rate_limit <- function() {
  resp <- lightcastapi_status()

  the$rate_limit["remaining"] <- httr2::resp_header(resp, "ratelimit-remaining") |> 
    as.numeric()

  the$rate_limit["time"] <- Sys.time()
}

# update rate_limit based on elapsed time since last request
update_rate_limit <- function(resp = NULL) {
  # allow function to update values based on current state & elapsed time OR new request
  if (!is.null(resp)) {
    the$rate_limit["remaining"] <- httr2::resp_header(resp, "ratelimit-remaining") |> 
      as.numeric()
  }
  
  remaining <- the$rate_limit["remaining"]

  if (remaining==9){ 
    # 9==new start to a rate "burndown" cycle (after one status request to (re-)initialize)
    the$rate_limit["time"] <- Sys.time()

  } else if(remaining < 9 & remaining > 2) {
    # Do nothing, continue with requests
  } else {
    # if remaining =< 2, wait until 1 sec "burndown" cycle is over (unless cycle is already over), then reset 

    elapsed <- difftime(Sys.time(), as.POSIXct(the$rate_limit["time"]), units = "secs") |> 
      as.numeric()

    if (elapsed < 1) {
      Sys.sleep(1.1-elapsed)
      get_rate_limit()
    } else {
      get_rate_limit()
    }

  }  
}
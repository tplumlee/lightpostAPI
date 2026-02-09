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
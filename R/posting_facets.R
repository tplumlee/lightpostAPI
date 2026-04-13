#' Request Faceted Job Postings Data from Lightcast's "Rankings" API Endpoints 
#' 
#' @description This function allows the user to request job postings data that
#'  is faceted (or disaggregated) by either a single variable or by two (i.e., 
#'  "nested" facets). This includes aggregated measures as well as timeseries 
#'  measures. The user must specify a time period and posting status to query
#'  (e.g., all postings that were newly posted during the specified time frame
#'  vs. those that were active at some point during the time frame), variable(s)
#'  to use in faceting, preferences for how facets are ranked, and any filters.
#' 
#' @param ... Named arguments indicating a
#'  filter name and appropriate values to filter on. For example 
#'  `soc5 = c("11-1011", "11-1021")` or `msa = "47900"`. For a listing of 
#'  available filters and required values/data types, see \[BUILT-IN DATA 
#'  NOT YET IMPLEMENTED\]. 
#' @param status Query job postings based on their status. Defaults to `"active"`.
#'  Without any arguments provided to `start` or `end` (the default) this queries
#'  currently active postings (as of the `latest_day` of available). If `start` 
#'  and `end` are specified, this queries postings active at any point during 
#'  that period. Can also take values of `"posted"` or `"expired"` (in which 
#'  case`start` and `end` values must also be declared).
#' @param start,end Start and end dates of the period to be queried. Must both
#'  be specified and be formatted as strings in the "YYYY-MM" format. For example,
#'  `start = "2025-01"` and `end = "2025-12"` would return results for postings
#'  active, posted, or expired (depending on the value of `status`) in the 
#'  2025 calendar year.
#' @param facet A variable to use in faceting (i.e., diaggregating) the job
#'  postings data being queried. This will produce an aggregate measure, 
#'  specified by `rank_by` (e.g., total unique postings), for each category
#'  of the faceting variable. Categories will be returned in descending
#'  order of the `rank_by` metric up to the number of categories/items
#'  specified by `rank_limit`.
#' @param rank_by A string indicating which metric to use when ranking `facet` 
#'  categories. Defaults to `"unique_postings"`. For a listing of available 
#'  metrics, see \[BUILT-IN DATA NOT YET IMPLEMENTED\].
#' @param rank_limit An integer between `0` and `1000` specifying the number of
#'  facet categories to return, based on `rank_by` metric. Defaults to `10`. 
#'  All facet categories can be returned by specifying `0`, except when 
#'  faceting by skills or certifications.
#' @param min_postings Requires faceted categories to have at least this many 
#'  unique postings depending on the filters applied. Defaults to `1` for all
#'  `rank_by` metrics except `"significance"`, which defaults to `3`.
#' @param metrics Any additonal metrics to aggregate for the faceted categories.
#'  For a listing of available metrics, see \[BUILT-IN DATA NOT YET IMPLEMENTED\].
#' @param nested_facet A variable to use in faceting/disaggregating each of the
#'  `facet` variable's categories. For example, if `"msa"` is provided as an
#'  argument for `facet`, providing `"soc5"` as an argument would return 
#'  total unique postings for each SOC occupation within each MSA.
#' @param nested_rank_by A string indicating which metric to use when ranking 
#'  `nested_facet` categories. Defaults to `"unique_postings"`. For a listing 
#'  of available metrics, see \[BUILT-IN DATA NOT YET IMPLEMENTED\].
#' @param nested_rank_limit An integer between `0` and `100` specifying the number of
#'  facet categories to return, based on `nested_rank_by` metric. Defaults to `10`. 
#'  All facet categories can be returned by specifying `0`, except when 
#'  faceting by skills or certifications.
#' @param nested_min_postings Requires faceted categories to have at least this many 
#'  unique postings depending on the filters applied. Defaults to `1` for all
#'  `nested_rank_by` metrics except `"significance"`, which defaults to `3`.
#' 
#' @return A tibble containing job posting data measures.
#' 
#' @examples
#' # COMING SOON
#' 
#' @export

# TODO: implement include and exclude parameters
posting_facets <- function(..., status = "active", start = NULL, end = NULL,
                           facet, rank_by = "unique_postings", rank_limit = 10,
                           min_postings = 1, metrics = NULL, nested_facet = NULL, 
                           nested_rank_by = "unique_postings", nested_rank_limit = 10, 
                           nested_min_postings = 1) {
  
  # SET UP/TEST FOR RATE LIMITING -----------------------------------------------------------------
  if (!exists("rate_limit", envir = the)) {
    get_rate_limit()
  } else {
    while (the$rate_limit["remaining"] <= 2) {
      update_rate_limit()
    }
  }

  # CHECK/GATHER ARGUMENTS ------------------------------------------------------------------------
  ## when
  if (is.null(start) && is.null(end)) {
    when <- unbox(status)
  } else if (xor(is.null(start), is.null(end))) { # if one or the other (but not both) are missing
    stop("If specifying a timeframe, both start and end dates must be provided.")
  } else if (!(valid_date(start) && valid_date(end))){ # if one or the other or both are invalid...
    stop("Start and end dates must be formatted as 'YYYY-MM'")
  } else {
    when <- list(
      start = unbox(start), 
      end = unbox(end),
      type = unbox(status)
    )
  }

  ## filters
  filters <- rlang::list2(..., when = when)
  ## Apply unbox() to singleton values

  ## rank
  rank <- list(
    by = unbox(rank_by),
    limit = unbox(rank_limit),
    min_unique_postings = unbox(min_postings)
  )

  if (!is.null(metrics)) {
    rank$extra_metrics <- metrics
  }

  ## nested rank
  if (is.null(nested_facet)) {

    nested_rank <- NULL

  } else {

    nested_rank <- list(
      by = unbox(nested_rank_by),
      limit = unbox(nested_rank_limit),
      min_unique_postings = unbox(nested_min_postings)
    )
    
    if (!is.null(metrics)) {
      nested_rank$extra_metrics <- metrics
    }

  }

  # TODO: IMPLEMENT VALUE/TYPE CHECKING

  # COMPOSE PAYLOAD -------------------------------------------------------------------------------
  payload <- list(
    filter = filters,
    rank = rank
  )

  if (!is.null(nested_rank)) {

    payload$nested_rank <- nested_rank

  }

  # COMPOSE REQUEST -------------------------------------------------------------------------------
  if (is.null(nested_rank)) {

    req <- httr2::request(BASE_URL) |> 
      httr2::req_template(
        "/rankings/{rankingFacet}", 
        rankingFacet = facet
      )
    
  } else if (!is.null(nested_rank)) {

    req <- httr2::request(BASE_URL) |> 
      httr2::req_template(
        "/rankings/{rankingFacet}/rankings/{nestedRankingFacet}", 
        rankingFacet = facet,
        nestedRankingFacet = nested_facet
      )
    
  }

  resp <- req |> 
    httr2::req_oauth_client_credentials(client = the$client) |> 
    httr2::req_headers(
      Accept = "application/json",
      "Content-Type" = "application/json"
    ) |> 
    httr2::req_body_json(payload, auto_unbox = FALSE) |> # turn off auto-unbox to preserve single-item arrays
    httr2::req_perform()

  update_rate_limit(resp)
  
  # ERROR HANDLING (?) ----------------------------------------------------------------------------

  # DATA CLEANING AND RETURN ----------------------------------------------------------------------
  buckets <- httr2::resp_body_json(resp)$data$ranking$buckets
  total_postings <- httr2::resp_body_json(resp)$data$totals$unique_postings

  unbucket <- function(bucket, nested = !is.null(nested_rank)) {
    unbucketed <- dplyr::as_tibble(bucket)

    if (nested) {
      unbucketed <- unbucketed[1,]

      if (length(unbucketed$ranking[[1]]) > 0) {
        unbucketed <- unbucketed |> 
          tidyr::unnest_longer("ranking") |>
          tidyr::unnest_wider("ranking", names_sep = "_")
      } else {
        unbucketed <- unbucketed |> 
          dplyr::mutate(
            ranking = NULL, 
            ranking_name = NA, 
            ranking_unique_postings = NA
          )
      }
    }
    
    return(unbucketed)
  }

  # TODO: refactor to make more flexible for additional metrics
  old_names  <- c(
      "name", 
      "unique_postings"
    )
  
  names(old_names)  <- c(
      facet, 
      paste0(facet, "_postings")
    )

  if (!is.null(nested_rank)) {
    add_names <- c("ranking_name", "ranking_unique_postings")
    names(add_names) <- c(
      nested_facet, 
      paste0(nested_facet, "_postings")
    )

    old_names <- append(
      old_names,
      add_names,
      after = 1
    )
  }

  if (length(buckets)==0) {
    
    clean_data <- NULL # if no data is returned by the API, return a NULL value

  } else {

  clean_data <- buckets |> 
    purrr::map(unbucket) |> 
    purrr::list_rbind() |> 
    dplyr::rename(tidyr::all_of(old_names))
    
  }

  return(clean_data)
}
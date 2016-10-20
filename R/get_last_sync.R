#' Get date and time of last sync to fitbit.com
#'
#' Get last sync from fitbit using cookie returned from login function
#' @param cookie Cookie returned after login, specifically the "u" cookie
#' @keywords sync
#' @export
#' @return A list of three character vectors
#'  \item{last_sync}{Character vector that looks like POSIXct}
#'  \item{profile_url}{The url of your FitBit profile}
#'  \item{display_name}{Your public FitBit display name}
#' @examples
#' \dontrun{
#' get_last_sync(cookie)
#' }
#' get_last_sync

get_last_sync <- function(cookie){
  if(!is.character(cookie)){stop("cookie must be a character string")}
  
  url <- "https://www.fitbit.com/ajaxapi"
  request <- paste0('{"template":"/ajaxTemplate.jsp",
          "serviceCalls":[{"name":"leaderboardAjaxService","method":"data"}]}')
  csrfToken <- stringr::str_extract(cookie,
        "[A-Z0-9]{8}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[A-Z0-9]{4}\\-[0-9A-Z]{12}")
  body <- list(request=request, csrfToken = csrfToken)
  response <- httr::POST(url, body=body, httr::config(cookie=cookie))
  dat_string <- methods::as(response, "character")
  dat_list <- jsonlite::fromJSON(dat_string)
  
  # Date and time of last sync in UTC.
  UTC_last_sync <- dat_list[["rankDisplayList"]][["syncTime"]]
  if (!is.null(UTC_last_sync)){  # if not null, convert it to our timezone.
    tz <- Sys.timezone()
    if(is.null(tz)){tz <- format(Sys.time(),"%Z")}
    last_sync <- as.POSIXct(UTC_last_sync, format="%Y-%m-%dT%H:%M:%S",tz='UTC')
    attributes(last_sync)$tzone <- tz  # Convert to our timezone.
  } else{  # if no sync time, return NA.
    last_sync <- NA
  }
  dat_list[["rankDisplayList"]][["profileUrl"]]
  profile_url <- paste0('https://www.fitbit.com',
                        dat_list[["rankDisplayList"]][["profileUrl"]])
  display_name <- dat_list[["rankDisplayList"]][["displayName"]]
  return(list(last_sync, profile_url, display_name) )
}

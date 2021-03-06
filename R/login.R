#' Login to fitbit.com and get cookie
#'
#' Get the login cookie after login at www.fitbit.com
#' @param email Email address used to login to fitbit.com
#' @param password Password used to login to fitbit.com
#' @param rememberMe Value for rememberMe during login, default is FALSE, but changing to TRUE may help with login issues
#' @keywords login
#' @export
#' @return A string containing the cookie that is returned after login at www.fitbit.com
#' @examples
#' \dontrun{
#' cookie <- login(email="corynissen<at>gmail.com", password="mypasswordhere")
#' }
#' login
login <- function(email, password, rememberMe=FALSE){
  if(!is.character(email)){stop("email must be a character string")}
  if(!is.character(password)){stop("password must be a character string")}

  rememberMe <- ifelse(rememberMe, "true", "false")

  url <- "https://www.fitbit.com/login"
  headers <- list("Host" = "www.fitbit.com",
                  "Connection" = "keep-alive",
                  "Content-Length" = "278",
                  "Cache-Control" = "max-age=0",
                  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
                  "Origin" =  "https://www.fitbit.com",
                  "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.99 Safari/537.36",
                  "Content-Type" =  "application/x-www-form-urlencoded",
                  "Referer" = "https://www.fitbit.com/login",
                  "Accept-Encoding" = "gzip, deflate",
                  "Accept-Language" = "en-US,en;q=0.8")
  body <- list("email"=email, "password"=password, "rememberMe"=rememberMe,
               "login"="Log In")

  a <- httr::POST(url, headers=headers, body=body)
  cookie <- a$cookies$u
  if(is.null(cookie)){
    all_cookies <- a$cookies
    cookie <- all_cookies[grep("^u$", all_cookies$name, ignore.case=F),"value"]
    if(is.null(cookie)){
      stop("login failed")
    }
  }

  return(cookie)
}

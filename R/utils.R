fetch_json <- function() {
  url <- "https://raw.githubusercontent.com/tonyfujs/data360-lkups/main/wb_hlo_lkups.json"
  res <- httr::GET(url)
  json_content <- httr::content(res, "text")
  json_data <- jsonlite::fromJSON(json_content)
  return(json_data)
}

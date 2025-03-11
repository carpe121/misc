#' fxn to pull data from REDCap
#' @param token character(1) API token
#' @param redcap_ver character string version of REDCap
#' @param rep_id numeric name of REDCap report
#' @import REDCapR
#' @import tidyverse
#' @export

##== NOTE ==##
# if using pull_red prints a similar error: 
## Warning: Error in $<-.data.frame: replacement has 0 rows, data has 7
# try changing the REDCap version first

#== Example ==#
# pull_red('[your valid token here]', rep_id="#######")

pull_rc = function(token, redcap_ver="v14.5.36", rep_id){
    uri <- paste('https://redcap.partners.org/redcap/redcap_', redcap_ver, '/API/', sep='')
    rep_id <- as.numeric(rep_id)
    redcap_report(redcap_uri = uri, token = token, report_id = rep_id)$data |> 
        mutate_all(~str_replace_na(., ""))
}

#' PTO CALCULATOR
#' calculates estimated accrued pto by date
#' @param start_pto integer pto hours at Sys.Date()
#' @param end_date date "YYYY-MM-DD" by which pto is calculated
#' @param taken_pto integer pto HOURS taken between Sys.Date and end_date (other than holidays)
#' @import lubridate
#' @return numeric (1)
#' @export

pto_calc <- function(start_pto, end_date, taken_pto){
	spto <- dhours(as.numeric(start_pto))
	edat <- as.Date(end_date)
	tpto <- dhours(as.numeric(taken_pto))

	accru_pto <- dhours(as.numeric(difftime(today(), edat, units="weeks"))*(-1)*4.62)
	tot_pto <- spto + accru_pto

	x <- seq(today(), edat, "days")
	holidays <- as.Date(c("2023-12-25", "2024-01-01", 
	                      "2024-01-15", "2024-02-19", "2024-05-27", 
	                      "2024-06-19", "2024-07-04", "2024-09-02", 
	                      "2024-10-14", "2024-11-28", "2024-12-25"))
	holiday_pto <- dhours((length(x[which(x %in% holidays)]))*8)

	pto_hrs <- (as.numeric(tot_pto - holiday_pto - tpto))/3600
	pto_hrs
}

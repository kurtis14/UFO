latest.sightings <- function(area = 'BC', return.rows = 5){
	url <- paste0('http://www.nuforc.org/webreports/ndxl', area, '.html')
	table <- XML::readHTMLTable(url)
	df <- data.frame(table)
	names(df) <- c('date_and_time','city','state','shape','duration','summary','posted')
	return(head(df,return.rows))
}
latest.sightings(area='BC')

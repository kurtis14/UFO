#' Is it a bird? Is it a plane? It's a UFO!
#' 
#' This function returns the latest sightings of Unidentified Flying Objects 
#' in the area (i.e., state, province, territory) specified. 
#' @param area A two-letter term in uppercase that specifies the area to search
#' @param return.rows The number of most recent sightings to return
#' @keywords misc
#' @export
#' @examples
#' latest.sightings()
#' latest.sightings(area='BC')
#' latest.sightings(area='NC',10)
#' latest.sightings(area='MA',3)

latest.sightings <- function(area = 'BC', return.rows = 5, ...){
	url <- paste0('http://www.nuforc.org/webreports/ndxl', area, '.html')
	table <- XML::readHTMLTable(url)
	df <- data.frame(table)
	names(df) <- c('date_and_time','city','state','shape','duration','summary','posted')
	return(head(df,return.rows))
}

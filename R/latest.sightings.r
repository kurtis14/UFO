#' Find latest UFO sightings
#' 
#' This function returns the most recent sightings of Unidentified Flying Objects 
#' in the area (i.e., state, province, territory) specified. 
#' @param area Two uppercase letters that specifies the area to search
#' @param return.rows The number of most recent sightings to return
#' @keywords misc
#' @export
#' @examples
#' latest.sightings()
#' latest.sightings(area='BC')
#' latest.sightings(area='NC', return.rows = 10)
#' latest.sightings(area='MA', 3)

latest.sightings <- function(area = 'BC', return.rows = 5){
	
	url <- paste0('http://www.nuforc.org/webreports/ndxl', area, '.html')
	table <- XML::readHTMLTable(url)
	df <- data.frame(table)
	names(df) <- c('date.time','city','state','shape','duration','summary','posted')
	
	df$date.time <- as.character(df$date.time)
	df[,8:9] <- data.frame(stringr::str_split_fixed(df$date.time, ' ', 2))
	names(df) <- c('date.time','city','state','shape','duration','summary','posted','date','time')
	
	library(dplyr)
	
	df <- df %>%
		select(date,time,posted,state,city,shape,duration,summary)
	
	return(head(df,return.rows))
}


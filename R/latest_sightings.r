#' Find latest UFO sightings
#' 
#' This function returns the most recent sightings of Unidentified Flying Objects 
#' in the area (i.e., state, province, territory) specified. 
#' @param area Two letters that specifies the area to search (case is not important)
#' @param return.rows The number of most recent sightings to return
#' @keywords misc
#' @export
#' @examples
#' latest.sightings()
#' latest.sightings(area='BC')
#' latest.sightings(area='NC', return.rows = 10)
#' latest.sightings(area='MA', 3)

latest.sightings <- function(area = 'BC', return.rows = 5){
	
	# Check that return.rows only takes numeric values and return an error if not ------
	assertthat::assert_that(is.numeric(return.rows))
	
	# Load data from website ------------------
	url <- paste0('http://www.nuforc.org/webreports/ndxl', area, '.html')
	table <- XML::readHTMLTable(url)
	df <- data.frame(table)
	names(df) <- c('date.time','city','state','shape','duration','summary','posted')
	
	# Split combined date-time variable into separate columns -----------
	df$date.time <- as.character(df$date.time)
	df[,8:9] <- data.frame(stringr::str_split_fixed(df$date.time, ' ', 2))
	names(df) <- c('date.time','city','state','shape','duration','summary','posted','date','time')
	
	library(dplyr)
	df <- df %>%
		select(date,time,posted,state,city,shape,duration,summary)
	
	# Change letters of all levels of shape to lowercase ----------
	levels(df$shape) <- tolower(levels(df$shape))
	
	# Return top n rows of dataset --------------
	return(head(df,return.rows))
}


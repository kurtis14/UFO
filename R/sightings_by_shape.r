#' Visualize/plot UFO sightings by shape
#' 
#' This function returns number of reported sightings of UFOs 
#' in the area (i.e., state, province, territory) grouped by shape, in either
#' a table or barplot. 
#' @param area Two letters that specifies the area to search (case is not important)
#' @param print The type of object to return, either a ggplot or table
#' @keywords misc
#' @export
#' @examples
#' sightings.by.shape(print = 'plot')
#' sightings.by.shape(area = 'NY', print = 'plot')
#' sightings.by.shape(area = 'WA', print = 'table')

sightings.by.shape <- function(area = 'BC', print = 'plot'){
	
	# Load data from website -----------------
	url <- paste0('http://www.nuforc.org/webreports/ndxl', area, '.html')
	table <- XML::readHTMLTable(url)
	df <- data.frame(table)
	names(df) <- c('date.time','city','state','shape','duration','summary','posted')
	
	# Split combined date-time variable into separate columns --------------
	df$date.time <- as.character(df$date.time)
	df[,8:9] <- data.frame(stringr::str_split_fixed(df$date.time, ' ', 2))
	names(df) <- c('date.time','city','state','shape','duration','summary','posted','date','time')
	
	library(dplyr)
	
	df <- df %>%
		select(date,time,posted,state,city,shape,duration,summary)
	
	# Revalue blank levels in shape to unspecified ----------
	df2 <- filter(df, shape=='')
	
	df2$shape <- as.character(df2$shape)
	df2$shape <- gsub(pattern = '.*',replacement = 'Unspecified',x=df2$shape)
	df2$shape <- factor(df2$shape)
	
	df <- filter(df, shape != '') %>%
		droplevels
	
	# Merge two datasets ----------------
	df <- rbind(df, df2)
	
	# Change letters of all levels of shape to lowercase ----------
	levels(df$shape) <- tolower(levels(df$shape))
	
	# Create output table for frequency of reported shapes of UFO's ---------
	table <- data.frame(table(df$shape))
	names(table) <- c('Shape','Freq')
	table <- table %>%
		mutate(Shape = reorder(Shape, Freq)) %>%
		arrange(desc(Freq))
	
	# Create barplot of shape frequency -----------------
	plot <- ggplot(table, aes(x=Shape,y=Freq)) + geom_bar(stat='identity') + theme_bw() +
		theme(axis.title.y = element_text(face='bold',size=20),
					axis.text.y = element_text(size=12)) +
		theme(axis.title.x = element_text(face="bold", size=20),
					axis.text.x  = element_text(angle=75, vjust=0.6, size=12)) +
		xlab('Shape of reported UFO') + ylab('Frequency')
	
	# Conditional statement for which object to return ----------------
	if(print == 'plot') {return(plot)}
	if(print == 'table') {return(table)}
	if(print != 'plot' | print != 'table') {
		return("The print argument only takes plot or table as input")}
}

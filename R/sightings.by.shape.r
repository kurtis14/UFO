#' Visualize/plot UFO sightings by shape
#' 
#' This function returns reported sightings of UFOs 
#' in the area (i.e., state, province, territory) specified by shape, in either
#' a table or barplot. 
#' @param area Two uppercase letters that specifies the area to search
#' @param print The type of object to return, either a ggplot or table
#' @keywords misc
#' @export
#' @examples
#' sightings.by.shape()
#' sightings.by.shape(area = 'NY', print = 'plot')
#' sightings.by.shape(area = 'WA', print = 'table')

sightings.by.shape <- function(area = 'BC', print = 'plot'){
	
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
	
	df2 <- filter(df, shape=='')
	
	df2$shape <- as.character(df2$shape)
	df2$shape <- gsub(pattern = '.*',replacement = 'Unspecified',x=df2$shape)
	df2$shape <- factor(df2$shape)
	
	df <- filter(df, shape != '') %>%
		droplevels
	
	df <- rbind(df, df2)
	
	table <- data.frame(table(df$shape))
	
	names(table) <- c('Shape','Freq')
	
	table <- table %>%
		mutate(Shape = reorder(Shape, Freq)) %>%
		arrange(desc(Freq))
	
	plot <- ggplot(table, aes(x=Shape,y=Freq)) + geom_bar(stat='identity') + theme_bw() +
		theme(axis.title.y = element_text(face='bold',size=20),
					axis.text.y = element_text(size=12)) +
		theme(axis.title.x = element_text(face="bold", size=20),
					axis.text.x  = element_text(angle=75, vjust=0.6, size=12)) +
		xlab('Shape of reported UFO') + ylab('Frequency')
	
	if(print == 'plot') {return(plot)}
	if(print == 'table') {return(table)}
}

#' Get the technical data of your favorite NEUMANN!
#'
#' This function returns a table which contains the technical data
#' of your chosen NEUMANN microphone.
#'
#' You are thinking buying a NEUMANN mic, but really want to read
#' the technical specification before you do that. However, you are
#' tired of going to the official website and switch between webpages
#' trying to get the technical data of different models.
#' Here is your solution!
#'
#' @param model.name This is the model name of your chosen mic.
#' Letters and numbers only. No special characters
#' allowed.
#' @return data.frame This table contains the technical data of your
#' chosen mic.
#' @keywords misc
#' @note case in \code{model.name} is ignored
#' @export
#' @examples
#' gMicData("KU100")
#' gMicData("tlm49")

gMicData <- function(model.name = "u87"){
	stopifnot(is.character(model.name))
	## exclude the whitespace and convert the input string to lowercase
	model_name <- tolower(gsub(" ", "", model.name))
	## create the url
	url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", model_name, "_data")
	## read the contents from url
	content <- readLines(url)
	## extract information that has a certain pattern
	myPattern  <-  '<td class="text">(.*)</td>'
	datalines  <-  grep(myPattern,content,value=TRUE)
	## get rid of html syntax
	datalines_clean <- gsub(myPattern, "\\1", datalines)
	datalines_clean2 <- gsub("<sup>(.*)</sup>","", datalines_clean)
	## create a table
	result <- as.data.frame(matrix(datalines_clean2,ncol=2,byrow=TRUE))
	colnames(result) <- c("Parameter", "Value")
	## return the resultant data.frame
  return(result)
}

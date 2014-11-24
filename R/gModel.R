#' Get the list of NEUMANN!
#'
#' This function returns a character vector which holds the model name
#' of all NEUMANN microphones.
#'
#' Please tell me all the microphone models that NEUMANN has right now!
#'
#' @return character This character vector contains the individual model
#' names of all current NEUMANN microphone.
#' @keywords misc
#' @export gModel
#' @examples
#' gModel()

gModel <- function(){
	## create the url
	url <- "https://www.neumann.com/?id=current_microphones&lang=en"
	##url2 <- RCurl::getURL(url, ssl.verifypeer = FALSE)
	## read the contents from url
	content <- readLines(url)
	## extract information that has a certain pattern
	myPattern  <-  '<option id="opt_(.*)_description">'
	datalines  <-  grep(myPattern,content,value=TRUE)
	## get rid of the html syntax
	list <- strsplit(datalines, "\"")
	member <- .getMember(list, 2)
	allMic <- gsub("opt_(.*)", "\\1", member)
	## return all mic
	return(allMic)
}

## helper funtion: get a specific member of each vector in a list
.getMember <- function(list, member){
	Member <- list[[1]][member]
	for(m in 2:length(list)){
		Member <- c(Member, list[[m]][member])
	}
	return(Member)
}





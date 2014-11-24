#' Get the description of a NEUMANN mic!
#'
#' This function returns a general description of your chosen mic!
#'
#' Please tell me something about my favorite mic!
#'
#' @param model.name This is the model name of your chosen mic.
#' Letters and numbers only. No special characters
#' allowed.
#' @return character This character vector contains the general description
#' of your chosen mic.
#' @note case in \code{model.name} is ignored
#' @keywords misc
#' @export
#' @examples
#' gDescription("KU100")
#' gDescription("tlm49")

gDescription <- function(model.name = "u87"){
	stopifnot(is.character(model.name))
	## convert the input string to lowercase
	model_name <- tolower(gsub(" ", "", model.name))
	## create the url
	url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", model_name, "_description")
	## read the contents from url
	content <- readLines(url)
	## extract information that has a certain pattern
	myPattern  <-  '<div class="textBlock">'
	datalines  <-  content[grep(myPattern,content)+1]
	generalDes <- datalines[1]
	result <- gsub("<br/>", "", generalDes)
	result2 <- gsub("\t", "", result)
	result3 <- gsub("/b", "", result2)

	return(result3)
}



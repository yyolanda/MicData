#' Get the description of a NEUMANN mic!
#'
#' This function returns a general description of your chosen mic!
#'
#' Please tell me something about my favorite mic!
#'
#' @param model.name This is the model name of your chosen mic. Please use
#' one of the model names that results from calling function "gModel".
#'
#' @return character This character vector contains the general description.
#' of your chosen mic.
#' @note Case in \code{model.name} is ignored.
#' @keywords misc
#' @export gDescription
#' @examples
#' gDescription("KU100")
#' gDescription("tlm49")


gDescription <- function(model.name = "u87"){
	assertthat::assert_that(is.character(model.name))
	## convert the input string to lowercase
	model_name <- tolower(gsub(" ", "", model.name))
  ## creat the url and special case handling
	if(grepl('^km[0-9]{3}$', model_name)|grepl("^skm[0-9]{3}$", model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "km100", "_description")
	}
	else if (grepl('^km[0-9]{3}a$', model_name)|grepl('km_a', model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "km_a", "_description")
	}
	else if (grepl('^km[0-9]{3}d$', model_name)|grepl('kmd', model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "kmd", "_description")
	}
	else{
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", model_name, "_description")
	}
	## read the contents from url
	content <- readLines(url)
	## extract information that has a certain pattern
	myPattern  <-  '<div class="textBlock">'
	mySecPattern <- '<div class="headline">'
	if(grepl(mySecPattern, content[grep(myPattern, content)+1][1])){
		myDataLines <- content[grep(myPattern, content)+2]
	}
	else{
		myDataLines  <-  content[grep(myPattern,content)+1]
	}

	generalDes <- myDataLines[1]
	## remove all the html tag, and other unwanted characters
	result <- gsub("<.*?>", "", generalDes)
	result2 <- gsub("\t", "", result)
	result3 <- gsub("/b", "", result2)

	return(result3)
}


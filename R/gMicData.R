#' Get the technical data of your favorite NEUMANN!
#'
#' This function returns a table or a list or tables which contains the
#' technical data of your chosen NEUMANN microphone.
#'
#' You are thinking buying a NEUMANN mic, but really want to read
#' the technical specification before you do that. However, you are
#' tired of going to the official website and switch between webpages
#' trying to get the technical data of different models.
#' Here is your solution!
#'
#' @param model.name This is the model name of your chosen mic. Please use
#' one of the model names that results from calling function "gModel". Note
#' that the technical data is not available for some of the mic models.
#' @return list or data.frame The tables contain the technical data of your
#' chosen mic.
#' @keywords misc
#' @note case in \code{model.name} is ignored
#' @export gMicData
#' @examples
#' gMicData("KU100")
#' gMicData("tlm49")


gMicData <- function(model.name = "u87"){
	assertthat::assert_that(is.character(model.name))
	## exclude the whitespace and convert the input string to lowercase
	model_name <- tolower(gsub(" ", "", model.name))
	## create the url and special case handling
	if(grepl('^km[0-9]{3}$', model_name)|grepl("^skm[0-9]{3}$", model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "km100", "_data")
	}
	else if (grepl('^km[0-9]{3}a$', model_name)|grepl('km_a', model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "km_a", "_data")
	}
	else if (grepl('^km[0-9]{3}d$', model_name)|grepl('kmd', model_name)){
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", "kmd", "_data")
	}
	else{
		url <- paste0("http://www.neumann.com/?lang=en&id=current_microphones&cid=", model_name, "_data")
	}
	## read the contents from url
	content <- readLines(url)
	## extract information that has a certain pattern
	myPattern  <-  '<td class="text">(.*)</td>'
	datalines  <-  grep(myPattern,content,value=TRUE)
	## get rid of html tags
	datalines_clean <- gsub(myPattern, "\\1", datalines)
	datalines_clean2 <- gsub("<.*?>","", datalines_clean)
	## create a table
	if(url == "http://www.neumann.com/?lang=en&id=current_microphones&cid=km100_data"){
		result_1 <- as.data.frame(matrix(datalines_clean2[1:80],ncol=4,byrow=TRUE))
		colnames(result_1) <- c("Parameter", "Value_1", "Value_2", "Value_3")
		result_2 <- as.data.frame(matrix(datalines_clean2[81:140],ncol=3,byrow=TRUE))
		colnames(result_2) <- c("Parameter", "Value_1", "Value_2")
		result_3 <- as.data.frame(matrix(datalines_clean2[141:200],ncol=3,byrow=TRUE))
		colnames(result_3) <- c("Parameter", "Value_1", "Value_2")
		result <- list(result_1, result_2, result_3)
	}
	else if(url == "http://www.neumann.com/?lang=en&id=current_microphones&cid=km_a_data"){
		result_1 <- as.data.frame(matrix(datalines_clean2[1:80],ncol=4,byrow=TRUE))
		colnames(result_1) <- c("Parameter", "Value_1", "Value_2", "Value_3")
		result_2 <- as.data.frame(matrix(datalines_clean2[81:160],ncol=4,byrow=TRUE))
		colnames(result_2) <- c("Parameter", "Value_1", "Value_2", "Value_3")
		result_3 <- as.data.frame(matrix(datalines_clean2[161:220],ncol=3,byrow=TRUE))
		colnames(result_3) <- c("Parameter", "Value_1", "Value_2")
		result <- list(result_1, result_2, result_3)
	}
	else if(url == "http://www.neumann.com/?lang=en&id=current_microphones&cid=kmd_data"){
		result_1 <- as.data.frame(matrix(datalines_clean2[1:76],ncol=4,byrow=TRUE))
		colnames(result_1) <- c("Parameter", "Value_1", "Value_2", "Value_3")
		result_2 <- as.data.frame(matrix(datalines_clean2[77:152],ncol=4,byrow=TRUE))
		colnames(result_2) <- c("Parameter", "Value_1", "Value_2", "Value_3")
		result_3 <- as.data.frame(matrix(datalines_clean2[153:209],ncol=3,byrow=TRUE))
		colnames(result_3) <- c("Parameter", "Value_1", "Value_2")
		result <- list(result_1, result_2, result_3)
	}
	else{
		result <- as.data.frame(matrix(datalines_clean2,ncol=2,byrow=TRUE))
		colnames(result) <- c("Parameter", "Value")
	}
	## return the resultant list or data.frame
  return(result)
}


#' Format univariate regression output neatly
#'
#' This function formats the key pieces of a univariate regression for a 
#' lay audience. It does not, and is not intended, to replace robust 
#' regression output that would be valuable for a more advanced 
#' audience/analyst.
#' @param dep Specifies the dependent variable name. Should be a string.
#' @param ind Specifies the independent variable name. Should be a string.
#' @param data Specifies the data frame from which the dep and ind variables come.
#' @keywords regression
#' @export
#' @examples
#' reg_output()
reg_output <- function(dep, ind, data) {
    dep <- match(dep, names(data))
    tr <- match(ind, names(data))
    res <- summary(lm(data[, dep] ~ data[, tr], data = data))$coef
    sig <- ifelse(res[, 4] < .001, "***", ifelse(res[, 4] < .01, "**", 
        ifelse(res[, 4] < .05, "*", ifelse(res[, 4] < .1, ".", ""))))
    ans <- paste(round(res[, 1], 2), " (", round(res[, 2], 2), ") ", sig, 
        sep = "")
    return(ans)
}

#' Format Excel data from a URL for input into R
#' 
#' This is a helper function to assist in reading in Excel data from a URL.
#' There is an active PR for hadley/readxl (#77) to deal with this issue, 
#' but it hasn't been merged yet. So, this hack was written to be able to 
#' correctly read in Excel data from the web.
#' @param file The Excel (.xls or .xlsx) file to be read in from a URL. 
#' @keywords Excel
#' @export
#' @examples
#' get_excel_url()
get_excel_url <- function(file) {
    ext <- tools::file_ext(file)
    tmp <- tempfile(fileext = paste0('.', ext))
    download.file(file, destfile = tmp, mode = 'wb')
    return(tmp)
}

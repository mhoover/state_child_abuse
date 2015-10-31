# functions to help with state abuse data prep and analysis

# specific regression (lm) formatting
reg_output <- function(dep, treat, data) {
    dep <- match(dep, names(data))
    tr <- match(treat, names(data))
    res <- summary(lm(data[, dep] ~ data[, tr], data = data))$coef
    sig <- ifelse(res[, 4] < .001, "***", ifelse(res[, 4] < .01, "**", 
        ifelse(res[, 4] < .05, "*", ifelse(res[, 4] < .1, ".", ""))))
    ans <- paste(round(res[, 1], 2), " (", round(res[, 2], 2), ") ", sig, 
        sep = "")
    return(ans)
}

# helper function to read in excel data from the internet
# there is an active pr out for hadley/readxl (#77) to deal with accessing
# url excel data. until merged, hack below is used.
get_excel_url <- function(file) {
    ext <- tools::file_ext(file)
    tmp <- tempfile(fileext = paste0('.', ext))
    download.file(file, destfile = tmp, mode = 'wb')
    return(tmp)
}

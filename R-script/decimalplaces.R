decimalplaces <- function(x) {
        if ((x %% 1) != 0) {
                nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
        } else {
                return(0)
        }
}
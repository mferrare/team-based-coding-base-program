createPath = function(string) {
    base_path = getwd()
    path = paste(base_path, string, sep="")
    return(path)
}
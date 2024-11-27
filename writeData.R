
writeData = function(data) {
    #check if directory exists, if not create
    base_path = getwd()
    data_folder_path = paste(base_path, "/data", sep="")
    data_file_path = paste(data_folder_path, "data.csv", sep="")

    if(dir.exists(data_folder_path)){
        #if file exists then delete
        if(file.exists(data_file_path)){
            file.remove(data_file_path)
        }
        #then write new data file
        write.csv(data, data_file_path)
    } else {
        dir.create(data_folder_path)
        write.csv(data, data_file_path)
    }
}
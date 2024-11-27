
writeData = function(data) {
    #check if directory exists, if not create
    data_folder_path = createPath("/data")
    data_file_path = createPath("/data/data.csv")

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
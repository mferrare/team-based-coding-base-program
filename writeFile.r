writeFile = function(filePath) {

    # Create the directory structure
    dir_path <- dirname(filePath)  # Extract the directory part of the file path
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

    # Create the file
    file.create(filePath)

    # Confirm that the file was created
    if (file.exists(filePath)) {
        cat("File created successfully:", filePath, "\n")
    } else {
        cat("Failed to create file:", filePath, "\n")
    }
}
#' Wrap All Image Files in Target Folder in Markdown Syntax for Use in Markdown Documents
#'
#' This function takes all the PNG, SVG, JPG, and JPEG image files found in target folder and wraps them in markdown
#' image syntax for easy inclusion in bookdown or other markdown formats.
#'
#' @param target_folder Character string specifying the path to the folder containing image files.
#' @param image_extensions Character vector of image file extensions to process.
#'   Default is c("png", "svg").
#' @param alt_text Character string for alternative text. Default is empty string.
#' @param print_output Logical. If TRUE (default), prints the wrapped syntax to console.
#'   If FALSE, returns the wrapped syntax as a character vector.
#' @param relative_path Logical. If TRUE (default), uses relative paths. If FALSE,
#'   uses absolute paths.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, markdown syntax will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a comment header in the file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#'
#' @export
wrap_all_image_files_folder_in_markdown <- function(target_folder,
                                         image_extensions = c("png", "svg", "jpg", "jpeg"),
                                         alt_text = "",
                                         print_output = TRUE,
                                         relative_path = TRUE,
                                         save_to_file = NULL,
                                         append = FALSE,
                                         add_section_header = NULL) {

  # Check if folder exists
  if (!dir.exists(target_folder)) {
    stop("Target folder does not exist: ", target_folder)
  }

  # Ensure folder path ends with "/"
  if (!endsWith(target_folder, "/")) {
    target_folder <- paste0(target_folder, "/")
  }

  # Get all files in the folder
  all_files <- list.files(target_folder, full.names = relative_path)

  if (length(all_files) == 0) {
    message("No files found in folder: ", target_folder)
    return(character(0))
  }

  # Create pattern for image extensions (case insensitive)
  extensions_pattern <- paste0("\\.(", paste(image_extensions, collapse = "|"), ")$")

  # Filter for image files
  image_files <- all_files[grepl(extensions_pattern, all_files, ignore.case = TRUE)]

  if (length(image_files) == 0) {
    message("No image files found with extensions: ", paste(image_extensions, collapse = ", "))
    return(character(0))
  }

  # Create markdown syntax for each image
  if (relative_path) {
    # Use the folder path plus filename for relative paths
    filenames <- basename(image_files)
    markdown_wrapped <- paste0("![", alt_text, "](", target_folder, filenames, ")")
  } else {
    # Use full path for absolute paths
    markdown_wrapped <- paste0("![", alt_text, "](", image_files, ")")
  }

  # Sort for consistent output
  markdown_wrapped <- sort(markdown_wrapped)

  # Handle file output if requested
  if (!is.null(save_to_file)) {
    # Prepare content for file
    file_content <- character(0)

    # Add section header if specified
    if (!is.null(add_section_header)) {
      file_content <- c(file_content, paste0("# ", add_section_header), "")
    }

    # Add the markdown syntax
    file_content <- c(file_content, markdown_wrapped)

    # Add blank line at end
    file_content <- c(file_content, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content, collapse = "\n"), file = save_to_file)
    }

    message("Markdown syntax saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("Markdown image syntax for", length(image_files), "files:\n")
    cat(paste(markdown_wrapped, collapse = "\n"))
    cat("\n\nReady to copy and paste into your bookdown manuscript!\n")
    invisible(markdown_wrapped)
  } else {
    return(markdown_wrapped)
  }
}


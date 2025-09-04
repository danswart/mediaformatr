#' Wrap All GIF Files in Target Folder in HTML IMG Syntax for Use in Markdown Documents
#'
#' This function takes all GIF files found in target folder and wraps them in HTML img syntax with width/height
#' attributes for easy inclusion in markdown documents that support HTML.
#'
#' @param target_folder Character string specifying the path to the folder containing GIF files.
#' @param gif_extensions Character vector of GIF file extensions to process.
#'   Default is c("gif").
#' @param width Character string for width attribute. Default is "100%".
#' @param height Character string for height attribute. Default is "120%".
#' @param alt_text Character string for alternative text. Default uses filename.
#' @param print_output Logical. If TRUE (default), prints the wrapped syntax to console.
#'   If FALSE, returns the wrapped syntax as a character vector.
#' @param relative_path Logical. If TRUE (default), uses relative paths. If FALSE,
#'   uses absolute paths.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, HTML syntax will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a comment header in the file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#'
#' @export
wrap_all_gifs_in_folder_in_html <- function(target_folder,
                                   gif_extensions = c("gif"),
                                   width = "100%",
                                   height = "120%",
                                   alt_text = NULL,
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

  # Create pattern for gif extensions (case insensitive)
  extensions_pattern <- paste0("\\.(", paste(gif_extensions, collapse = "|"), ")$")

  # Filter for gif files
  gif_files <- all_files[grepl(extensions_pattern, all_files, ignore.case = TRUE)]

  if (length(gif_files) == 0) {
    message("No GIF files found with extensions: ", paste(gif_extensions, collapse = ", "))
    return(character(0))
  }

  # Create HTML img syntax for each gif
  if (relative_path) {
    # Use the folder path plus filename for relative paths
    filenames <- basename(gif_files)
    file_paths <- paste0(target_folder, filenames)
    alt_texts <- if (is.null(alt_text)) tools::file_path_sans_ext(filenames) else rep(alt_text, length(filenames))
    html_wrapped <- paste0('<img src="', file_paths, '" width="', width, '" height="', height, '" alt="', alt_texts, '">')
  } else {
    # Use full path for absolute paths
    alt_texts <- if (is.null(alt_text)) tools::file_path_sans_ext(basename(gif_files)) else rep(alt_text, length(gif_files))
    html_wrapped <- paste0('<img src="', gif_files, '" width="', width, '" height="', height, '" alt="', alt_texts, '">')
  }

  # Sort for consistent output
  html_wrapped <- sort(html_wrapped)

  # Handle file output if requested
  if (!is.null(save_to_file)) {
    # Prepare content for file
    file_content <- character(0)

    # Add section header if specified
    if (!is.null(add_section_header)) {
      file_content <- c(file_content, paste0("# ", add_section_header), "")
    }

    # Add the HTML syntax
    file_content <- c(file_content, html_wrapped)

    # Add blank line at end
    file_content <- c(file_content, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content, collapse = "\n"), file = save_to_file)
    }

    message("HTML syntax saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("HTML img syntax for", length(gif_files), "GIF files:\n")
    cat(paste(html_wrapped, collapse = "\n"))
    cat("\n\nReady to copy and paste into your markdown document!\n")
    invisible(html_wrapped)
  } else {
    return(html_wrapped)
  }
}

#' Wrap All Video Files in Target Folder in HTML Video Tag Syntax for Use in Markdown Documents
#'
#' This function takes all MP4 and MOV video files in target folder and wraps them in HTML video tag
#' syntax for easy inclusion in markdown documents that support HTML.
#'
#' @param target_folder Character string specifying the path to the folder containing video files.
#' @param video_extensions Character vector of video file extensions to process.
#'   Default is c("mp4", "mov").
#' @param width Character string for video width. Default is "320".
#' @param height Character string for video height. Default is "240".
#' @param controls Logical. If TRUE (default), shows video controls. If FALSE, no controls.
#' @param autoplay Logical. If TRUE, video autoplays. If FALSE (default), manual play.
#' @param muted Logical. If TRUE, video starts muted. If FALSE (default), unmuted.
#' @param loop Logical. If TRUE, video loops. If FALSE (default), plays once.
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
wrap_all_video_files_in_folder_in_html <- function(target_folder,
                                     video_extensions = c("mp4", "mov"),
                                     width = "100%",
                                     height = "auto",
                                     controls = TRUE,
                                     autoplay = FALSE,
                                     muted = FALSE,
                                     loop = FALSE,
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

  # Create pattern for video extensions (case insensitive)
  extensions_pattern <- paste0("\\.(", paste(video_extensions, collapse = "|"), ")$")

  # Filter for video files
  video_files <- all_files[grepl(extensions_pattern, all_files, ignore.case = TRUE)]

  if (length(video_files) == 0) {
    message("No video files found with extensions: ", paste(video_extensions, collapse = ", "))
    return(character(0))
  }

  # Create HTML video tag syntax for each video
  if (relative_path) {
    # Use the folder path plus filename for relative paths
    filenames <- basename(video_files)
    file_paths <- paste0(target_folder, filenames)
  } else {
    # Use full path for absolute paths
    file_paths <- video_files
  }

  # Build video attributes
  video_attrs <- character(0)
  video_attrs <- c(video_attrs, paste0('width="', width, '"'))
  video_attrs <- c(video_attrs, paste0('height="', height, '"'))
  if (controls) video_attrs <- c(video_attrs, 'controls')
  if (autoplay) video_attrs <- c(video_attrs, 'autoplay')
  if (muted) video_attrs <- c(video_attrs, 'muted')
  if (loop) video_attrs <- c(video_attrs, 'loop')

  # Create video tags with source elements
  html_wrapped <- character(length(file_paths))
  for (i in seq_along(file_paths)) {
    file_ext <- tools::file_ext(file_paths[i])
    video_type <- if (tolower(file_ext) == "mp4") "video/mp4" else "video/quicktime"

    attrs_string <- paste(video_attrs, collapse = " ")
    html_wrapped[i] <- paste0(
      '<video ', attrs_string, '>',
      '<source src="', file_paths[i], '" type="', video_type, '">',
      'Your browser does not support the video tag.',
      '</video>'
    )
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
    cat("HTML video tag syntax for", length(video_files), "video files:\n")
    cat(paste(html_wrapped, collapse = "\n"))
    cat("\n\nReady to copy and paste into your markdown document!\n")
    invisible(html_wrapped)
  } else {
    return(html_wrapped)
  }
}


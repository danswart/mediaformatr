#' List All Image File References Found in Target File
#'
#' This function reads a target file and extracts all image file references (PNG, SVG, JPG, JPEG, BMP)
#' found within the content. The image file paths are returned exactly as they appear in the source file,
#' with no modifications or transformations applied. Results can be displayed in the console and
#' optionally saved to a file.
#'
#' @param target_file Character string specifying the path to the file to be analyzed.
#'   File can be any text-based format (Rmd, md, qmd, txt, etc.).
#' @param image_extensions Character vector of image file extensions to search for.
#'   Default is c("png", "svg", "jpg", "jpeg", "bmp"). Extensions are matched case-insensitively.
#' @param print_output Logical. If TRUE (default), prints the found image references to console.
#'   If FALSE, suppresses console output.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, the list of image references will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the output file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param show_line_numbers Logical. If TRUE, shows the line number where each image
#'   reference was found. If FALSE (default), shows only the image paths.
#'
#' @details
#' The function searches through each line of the target file using regular expressions
#' to identify image file references. It looks for any text pattern that ends with the
#' specified image extensions, regardless of the surrounding syntax (markdown, HTML, etc.).
#'
#' Duplicate image references are preserved in the output to show the actual frequency
#' of usage in the source file. If you need unique references only, apply \code{unique()}
#' to the returned vector.
#'
#' @return Character vector containing all image file references found in the target file,
#'   in the order they were encountered. Returns character(0) if no images are found.
#'
#' @examples
#' \dontrun{
#' # Basic usage - list images found in a file
#' list_image_calls_in_target_file("sample-slides.qmd")
#'
#' # Show line numbers where images are found
#' list_image_calls_in_target_file("sample-slides.qmd", show_line_numbers = TRUE)
#'
#' # Save results to file with custom header
#' list_image_calls_in_target_file("sample-slides.qmd",
#'                                  save_to_file = "found-images.txt",
#'                                  add_section_header = "Images in Presentation")
#'
#' # Search for specific image types only
#' list_image_calls_in_target_file("document.md",
#'                                  image_extensions = c("png", "jpg"))
#'
#' # Get results without console output
#' image_refs <- list_image_calls_in_target_file("sample-slides.qmd",
#'                                                print_output = FALSE)
#'
#' # Get unique image references only
#' unique_images <- unique(list_image_calls_in_target_file("slides.qmd",
#'                                                         print_output = FALSE))
#' }
#'
#' @seealso
#' \code{\link{list_gif_calls_in_target_file}} for GIF-specific analysis
#'
#'
#' @export
list_image_calls_in_target_file <- function(target_file,
                                             image_extensions = c("png", "svg", "jpg", "jpeg", "bmp"),
                                             print_output = TRUE,
                                             save_to_file = NULL,
                                             append = FALSE,
                                             add_section_header = NULL,
                                             show_line_numbers = FALSE) {

  # Check if file exists
  if (!file.exists(target_file)) {
    stop("Target file does not exist: ", target_file)
  }

  # Check if file is readable
  if (!file.access(target_file, mode = 4) == 0) {
    stop("Target file is not readable: ", target_file)
  }

  # Read the file content
  file_content <- readLines(target_file, warn = FALSE)

  if (length(file_content) == 0) {
    message("Target file is empty: ", target_file)
    return(character(0))
  }

  # Validate image_extensions parameter
  if (!is.character(image_extensions) || length(image_extensions) == 0) {
    stop("image_extensions must be a non-empty character vector")
  }

  # Create pattern for image extensions (case insensitive)
  extensions_pattern <- paste0("\\b\\S+\\.(", paste(image_extensions, collapse = "|"), ")\\b")

  # Extract all image file references from the content
  all_matches <- character(0)
  line_numbers <- integer(0)

  for (i in seq_along(file_content)) {
    line <- file_content[i]
    matches <- regmatches(line, gregexpr(extensions_pattern, line, ignore.case = TRUE))[[1]]
    if (length(matches) > 0) {
      all_matches <- c(all_matches, matches)
      line_numbers <- c(line_numbers, rep(i, length(matches)))
    }
  }

  if (length(all_matches) == 0) {
    message("No image files found in '", basename(target_file), "' with extensions: ",
            paste(image_extensions, collapse = ", "))
    return(character(0))
  }

  # Prepare output format
  if (show_line_numbers) {
    output_lines <- paste0("Line ", line_numbers, ": ", all_matches)
  } else {
    output_lines <- all_matches
  }

  # Handle file output if requested
  if (!is.null(save_to_file)) {
    # Prepare content for file
    file_content_out <- character(0)

    # Add section header if specified
    if (!is.null(add_section_header)) {
      file_content_out <- c(file_content_out,
                           paste0("# ", add_section_header),
                           paste0("Source file: ", target_file),
                           paste0("Date: ", Sys.Date()),
                           paste0("Total images found: ", length(all_matches)),
                           "")
    }

    # Add the image references
    file_content_out <- c(file_content_out, output_lines)

    # Add blank line at end
    file_content_out <- c(file_content_out, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    message("Image references saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("Found", length(all_matches), "image references in:", basename(target_file), "\n")
    if (length(unique(all_matches)) < length(all_matches)) {
      cat("(", length(unique(all_matches)), "unique images)\n")
    }
    cat("\nImage references:\n")
    cat(paste(output_lines, collapse = "\n"))
    cat("\n")
    invisible(all_matches)
  } else {
    return(all_matches)
  }
}

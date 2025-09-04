#' List All GIF File References Found in Target File
#'
#' This function reads a target file and extracts all GIF file references found within the content.
#' The GIF file paths are returned exactly as they appear in the source file, with no modifications
#' or transformations applied. Results can be displayed in the console and optionally saved to a file.
#' This function is specifically designed to inventory GIF usage in presentations, documentation,
#' and other markdown-based content.
#'
#' @param target_file Character string specifying the path to the file to be analyzed.
#'   File can be any text-based format (Rmd, md, qmd, txt, html, etc.).
#' @param print_output Logical. If TRUE (default), prints the found GIF references to console.
#'   If FALSE, suppresses console output.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, the list of GIF references will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the output file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param show_line_numbers Logical. If TRUE, shows the line number where each GIF
#'   reference was found. If FALSE (default), shows only the GIF paths.
#' @param case_sensitive Logical. If TRUE, searches for ".gif" extension with exact case.
#'   If FALSE (default), matches ".gif", ".GIF", ".Gif", etc.
#'
#' @details
#' The function searches through each line of the target file using regular expressions
#' to identify GIF file references. It looks for any text pattern that ends with the
#' .gif extension, regardless of the surrounding syntax (markdown, HTML, file paths, etc.).
#'
#' Duplicate GIF references are preserved in the output to show the actual frequency
#' of usage in the source file. This is particularly useful for presentations where
#' the same GIF might be referenced multiple times. If you need unique references only,
#' apply \code{unique()} to the returned vector.
#'
#' The function is particularly useful for:
#' \itemize{
#'   \item Inventorying animated content in presentations
#'   \item Checking for broken GIF links before publishing
#'   \item Analyzing media usage patterns in documentation
#'   \item Preparing media asset lists for deployment
#' }
#'
#' @return Character vector containing all GIF file references found in the target file,
#'   in the order they were encountered. Returns character(0) if no GIFs are found.
#'
#' @examples
#' \dontrun{
#' # Basic usage - list GIFs found in a presentation file
#' list_gif_calls_in_target_file("sample-slides.qmd")
#'
#' # Show line numbers where GIFs are found
#' list_gif_calls_in_target_file("presentation.rmd", show_line_numbers = TRUE)
#'
#' # Save results to file with custom header
#' list_gif_calls_in_target_file("slides.qmd",
#'                                save_to_file = "gif-inventory.txt",
#'                                add_section_header = "Animated Content Inventory")
#'
#' # Case-sensitive search for exact ".gif" extension
#' list_gif_calls_in_target_file("document.md", case_sensitive = TRUE)
#'
#' # Get results without console output for programmatic use
#' gif_refs <- list_gif_calls_in_target_file("sample-slides.qmd",
#'                                            print_output = FALSE)
#'
#' # Get unique GIF references only
#' unique_gifs <- unique(list_gif_calls_in_target_file("slides.qmd",
#'                                                     print_output = FALSE))
#'
#' # Check if specific GIFs are used
#' all_gifs <- list_gif_calls_in_target_file("presentation.qmd", print_output = FALSE)
#' large_gifs <- grep("large|big", all_gifs, value = TRUE)
#' }
#'
#' @seealso
#' \code{\link{list_image_calls_in_target_file}} for listing all image references
#'
#' @export
list_gif_calls_in_target_file <- function(target_file,
                                           print_output = TRUE,
                                           save_to_file = NULL,
                                           append = FALSE,
                                           add_section_header = NULL,
                                           show_line_numbers = FALSE,
                                           case_sensitive = FALSE) {

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

  # Create pattern for GIF files (case sensitive or insensitive)
  if (case_sensitive) {
    gif_pattern <- "\\b\\S+\\.gif\\b"
  } else {
    gif_pattern <- "\\b\\S+\\.gif\\b"
  }

  # Extract all GIF file references from the content
  all_matches <- character(0)
  line_numbers <- integer(0)

  for (i in seq_along(file_content)) {
    line <- file_content[i]
    if (case_sensitive) {
      matches <- regmatches(line, gregexpr(gif_pattern, line))[[1]]
    } else {
      matches <- regmatches(line, gregexpr(gif_pattern, line, ignore.case = TRUE))[[1]]
    }

    if (length(matches) > 0) {
      all_matches <- c(all_matches, matches)
      line_numbers <- c(line_numbers, rep(i, length(matches)))
    }
  }

  if (length(all_matches) == 0) {
    message("No GIF files found in '", basename(target_file), "'")
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
                           paste0("Total GIFs found: ", length(all_matches)),
                           paste0("Unique GIFs: ", length(unique(all_matches))),
                           paste0("Case sensitive search: ", case_sensitive),
                           "")
    }

    # Add the GIF references
    file_content_out <- c(file_content_out, output_lines)

    # Add blank line at end
    file_content_out <- c(file_content_out, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    message("GIF references saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("Found", length(all_matches), "GIF references in:", basename(target_file), "\n")
    if (length(unique(all_matches)) < length(all_matches)) {
      cat("(", length(unique(all_matches)), "unique GIFs)\n")
    }
    cat("\nGIF references:\n")
    cat(paste(output_lines, collapse = "\n"))
    cat("\n")
    invisible(all_matches)
  } else {
    return(all_matches)
  }
}

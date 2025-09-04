#' List All Duplicate Media File References Found in Target File
#'
#' This function reads a target file and identifies duplicate media file references, helping to
#' optimize content by revealing repeated usage of the same media assets. The function searches
#' for various media types (images, videos, audio) and reports which files are referenced
#' multiple times. This is particularly useful for presentations, documentation, and web content
#' where duplicate media references can impact performance and maintenance.
#'
#' @param target_file Character string specifying the path to the file to be analyzed.
#'   File can be any text-based format (Rmd, md, qmd, txt, html, etc.).
#' @param media_extensions Character vector of media file extensions to search for.
#'   Default is c("gif", "mp4", "mov", "png", "jpeg", "jpg", "svg", "webp", "avi", "wmv", "mp3", "wav").
#'   Extensions are matched case-insensitively.
#' @param print_output Logical. If TRUE (default), prints the found duplicate media references to console.
#'   If FALSE, suppresses console output.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, the list of duplicate media references will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the output file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param show_line_numbers Logical. If TRUE, shows the line numbers where each duplicate
#'   media reference was found. If FALSE (default), shows only the media paths.
#' @param show_usage_count Logical. If TRUE (default), shows how many times each duplicate
#'   media file is referenced. If FALSE, shows only the file paths.
#' @param minimum_duplicates Integer. Minimum number of duplicates required to be reported.
#'   Default is 2 (files referenced at least twice). Set to 3 or higher to focus on heavily duplicated content.
#'
#' @details
#' The function searches through each line of the target file using regular expressions
#' to identify media file references across various formats including markdown syntax,
#' HTML tags, and plain file paths. It then identifies which media files appear multiple
#' times in the document.
#'
#' This analysis is particularly valuable for:
#' \itemize{
#'   \item Optimizing presentation file sizes by identifying redundant media
#'   \item Cleaning up documentation with repeated images
#'   \item Performance analysis of web content
#'   \item Media asset management and organization
#'   \item Identifying frequently used branding elements
#' }
#'
#' The function handles various reference formats including:
#' \itemize{
#'   \item Markdown: \code{![alt](path/image.png)}
#'   \item HTML: \code{<img src="path/image.png">}
#'   \item Direct paths: \code{path/video.mp4}
#'   \item URLs: \code{https://example.com/media.gif}
#' }
#'
#' @return Character vector containing media file references that appear multiple times
#'   in the target file. Returns character(0) if no duplicates are found.
#'   When \code{print_output = FALSE}, returns a named character vector where names
#'   indicate the usage count.
#'
#' @examples
#' \dontrun{
#' # Basic usage - find duplicate media in a presentation
#' list_duplicate_media_calls_in_target_file("sample-slides.qmd")
#'
#' # Show line numbers and usage counts
#' list_duplicate_media_calls_in_target_file("presentation.rmd",
#'                                      show_line_numbers = TRUE,
#'                                      show_usage_count = TRUE)
#'
#' # Focus on heavily duplicated content (3+ references)
#' list_duplicate_media_calls_in_target_file("slides.qmd", minimum_duplicates = 3)
#'
#' # Search specific media types only
#' list_duplicate_media_calls_in_target_file("document.md",
#'                                      media_extensions = c("png", "jpg", "gif"))
#'
#' # Save detailed report to file
#' list_duplicate_media_calls_in_target_file("slides.qmd",
#'                                      save_to_file = "duplicate-media-report.txt",
#'                                      add_section_header = "Media Optimization Report",
#'                                      show_usage_count = TRUE)
#'
#' # Get results for programmatic analysis
#' duplicates <- list_duplicate_media_calls_in_target_file("presentation.qmd",
#'                                                   print_output = FALSE)
#'
#' # Analyze duplicate patterns
#' heavy_duplicates <- list_duplicate_media_calls_in_target_file("slides.qmd",
#'                                                         minimum_duplicates = 4,
#'                                                         print_output = FALSE)
#'
#' # Check for specific file types with high duplication
#' gif_duplicates <- list_duplicate_media_calls_in_target_file("presentation.qmd",
#'                                                       media_extensions = "gif",
#'                                                       print_output = FALSE)
#' }
#'
#' @seealso
#' \code{\link{list_image_calls_in_target_file}} for listing all image references,
#' \code{\link{list_gif_calls_in_target_file}} for GIF-specific analysis
#'
#' @export
list_duplicate_media_calls_in_target_file <- function(target_file,
                                                 media_extensions = c("gif", "mp4", "mov", "png", "jpeg", "jpg",
                                                                      "svg", "webp", "avi", "wmv", "mp3", "wav"),
                                                 print_output = TRUE,
                                                 save_to_file = NULL,
                                                 append = FALSE,
                                                 add_section_header = NULL,
                                                 show_line_numbers = FALSE,
                                                 show_usage_count = TRUE,
                                                 minimum_duplicates = 2) {

  # Check if file exists
  if (!file.exists(target_file)) {
    stop("Target file does not exist: ", target_file)
  }

  # Check if file is readable
  if (!file.access(target_file, mode = 4) == 0) {
    stop("Target file is not readable: ", target_file)
  }

  # Validate parameters
  if (!is.character(media_extensions) || length(media_extensions) == 0) {
    stop("media_extensions must be a non-empty character vector")
  }

  if (!is.numeric(minimum_duplicates) || minimum_duplicates < 2) {
    stop("minimum_duplicates must be a numeric value of 2 or greater")
  }

  # Read the file content
  file_content <- readLines(target_file, warn = FALSE)

  if (length(file_content) == 0) {
    message("Target file is empty: ", target_file)
    return(character(0))
  }

  # Create pattern for media files (case insensitive)
  extensions_pattern <- paste0("([\\w/\\.-]+\\.(", paste(media_extensions, collapse = "|"), "))")

  # Extract all media file references from the content
  all_matches <- character(0)
  line_numbers <- integer(0)

  for (i in seq_along(file_content)) {
    line <- file_content[i]
    matches <- regmatches(line, gregexpr(extensions_pattern, line, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(matches) > 0) {
      all_matches <- c(all_matches, matches)
      line_numbers <- c(line_numbers, rep(i, length(matches)))
    }
  }

  if (length(all_matches) == 0) {
    message("No media files found in '", basename(target_file), "' with extensions: ",
            paste(media_extensions, collapse = ", "))
    return(character(0))
  }

  # Count occurrences of each media file
  media_counts <- table(all_matches)

  # Find duplicates based on minimum_duplicates threshold
  duplicate_media <- names(media_counts)[media_counts >= minimum_duplicates]

  if (length(duplicate_media) == 0) {
    message("No duplicate media files found in '", basename(target_file),
            "' (minimum duplicates: ", minimum_duplicates, ")")
    return(character(0))
  }

  # Prepare detailed output with line numbers and counts
  detailed_output <- character(0)
  for (media_file in duplicate_media) {
    count <- media_counts[media_file]

    if (show_line_numbers) {
      # Find all line numbers for this media file
      media_lines <- line_numbers[all_matches == media_file]
      line_info <- paste("Lines:", paste(media_lines, collapse = ", "))
    } else {
      line_info <- ""
    }

    if (show_usage_count && show_line_numbers) {
      output_line <- paste0(media_file, " (", count, " times) - ", line_info)
    } else if (show_usage_count) {
      output_line <- paste0(media_file, " (", count, " times)")
    } else if (show_line_numbers) {
      output_line <- paste0(media_file, " - ", line_info)
    } else {
      output_line <- media_file
    }

    detailed_output <- c(detailed_output, output_line)
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
                           paste0("Analysis date: ", Sys.Date()),
                           paste0("Minimum duplicates threshold: ", minimum_duplicates),
                           paste0("Media types searched: ", paste(media_extensions, collapse = ", ")),
                           paste0("Total media references: ", length(all_matches)),
                           paste0("Unique media files: ", length(unique(all_matches))),
                           paste0("Duplicate media files: ", length(duplicate_media)),
                           "")
    }

    # Add summary statistics
    file_content_out <- c(file_content_out,
                         "## Duplicate Media Files:",
                         "")

    # Add the duplicate media references
    file_content_out <- c(file_content_out, detailed_output)

    # Add blank line at end
    file_content_out <- c(file_content_out, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    message("Duplicate media analysis saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("Found", length(duplicate_media), "duplicate media files in:", basename(target_file), "\n")
    cat("(", length(all_matches), "total media references,", length(unique(all_matches)), "unique files )\n")
    cat("Minimum duplicates threshold:", minimum_duplicates, "\n\n")
    cat("Duplicate media files:\n")
    cat(paste(detailed_output, collapse = "\n"))
    cat("\n")

    # Create named vector for return
    result <- media_counts[duplicate_media]
    names(result) <- duplicate_media
    invisible(result)
  } else {
    # Return named vector with usage counts
    result <- media_counts[duplicate_media]
    names(result) <- duplicate_media
    return(result)
  }
}

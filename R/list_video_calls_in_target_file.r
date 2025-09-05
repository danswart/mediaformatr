#' List All Video File References Found in Target File
#'
#' This function reads a target file and extracts all video file references found within the content.
#' The video file paths are returned exactly as they appear in the source file, with no modifications
#' or transformations applied. Results can be displayed in the console and optionally saved to a file.
#' This function is specifically designed to inventory video usage in presentations, documentation,
#' and other multimedia content.
#'
#' @param target_file Character string specifying the path to the file to be analyzed.
#'   File can be any text-based format (Rmd, md, qmd, txt, html, etc.).
#' @param video_extensions Character vector of video file extensions to search for.
#'   Default is c("mp4", "mov", "avi", "wmv", "webm", "mkv", "flv", "m4v").
#'   Extensions are matched case-insensitively unless case_sensitive = TRUE.
#' @param print_output Logical. If TRUE (default), prints the found video references to console.
#'   If FALSE, suppresses console output.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, the list of video references will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the output file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param show_line_numbers Logical. If TRUE, shows the line number where each video
#'   reference was found. If FALSE (default), shows only the video paths.
#' @param case_sensitive Logical. If TRUE, searches for video extensions with exact case.
#'   If FALSE (default), matches ".mp4", ".MP4", ".Mp4", etc.
#'
#' @details
#' The function searches through each line of the target file using regular expressions
#' to identify video file references. It looks for any text pattern that ends with the
#' specified video extensions, regardless of the surrounding syntax (markdown, HTML,
#' video tags, file paths, etc.).
#'
#' Duplicate video references are preserved in the output to show the actual frequency
#' of usage in the source file. This is particularly useful for presentations where
#' the same video might be referenced multiple times or for analyzing bandwidth usage.
#' If you need unique references only, apply \code{unique()} to the returned vector.
#'
#' The function is particularly useful for:
#' \itemize{
#'   \item Inventorying video content in presentations and documentation
#'   \item Checking for broken video links before publishing
#'   \item Analyzing multimedia usage patterns for bandwidth planning
#'   \item Preparing video asset lists for deployment or CDN setup
#'   \item Auditing large video files that might affect page load times
#'   \item Compliance checking for accessibility video requirements
#' }
#'
#' Common video reference formats detected:
#' \itemize{
#'   \item HTML video tags: \code{<video src="path/video.mp4">}
#'   \item Source elements: \code{<source src="path/video.webm" type="video/webm">}
#'   \item Markdown links: \code{[Video](path/video.mov)}
#'   \item Direct file paths: \code{videos/presentation.mp4}
#'   \item Background videos: \code{background-video: url(path/video.mp4)}
#' }
#'
#' @return Character vector containing all video file references found in the target file,
#'   in the order they were encountered. Returns character(0) if no videos are found.
#'
#' @examples
#' \dontrun{
#' # Basic usage - list videos found in a presentation file
#' list_video_calls_in_target_file("sample-slides.qmd")
#'
#' # Show line numbers where videos are found
#' list_video_calls_in_target_file("presentation.rmd", show_line_numbers = TRUE)
#'
#' # Save results to file with custom header
#' list_video_calls_in_target_file("slides.qmd",
#'                                  save_to_file = "video-inventory.txt",
#'                                  add_section_header = "Video Content Inventory")
#'
#' # Search for specific video types only
#' list_video_calls_in_target_file("document.md",
#'                                  video_extensions = c("mp4", "webm"))
#'
#' # Case-sensitive search for exact extensions
#' list_video_calls_in_target_file("document.md", case_sensitive = TRUE)
#'
#' # Get results without console output for programmatic use
#' video_refs <- list_video_calls_in_target_file("sample-slides.qmd",
#'                                                print_output = FALSE)
#'
#' # Get unique video references only
#' unique_videos <- unique(list_video_calls_in_target_file("slides.qmd",
#'                                                         print_output = FALSE))
#'
#' # Check for high-bandwidth video formats
#' all_videos <- list_video_calls_in_target_file("presentation.qmd", print_output = FALSE)
#' large_videos <- grep("4k|uhd|hd|large", all_videos, value = TRUE, ignore.case = TRUE)
#'
#' # Analyze video distribution across different formats
#' video_list <- list_video_calls_in_target_file("website.md", print_output = FALSE)
#' mp4_count <- length(grep("\\.mp4$", video_list, ignore.case = TRUE))
#' webm_count <- length(grep("\\.webm$", video_list, ignore.case = TRUE))
#' cat("MP4 videos:", mp4_count, "WebM videos:", webm_count, "\n")
#'
#' # Use with package example file
#' example_file <- system.file("extdata", "slide-deck-sample.Rmd", package = "mediaformatr")
#' if (file.exists(example_file)) {
#'   list_video_calls_in_target_file(example_file)
#' }
#' }
#'
#' @seealso
#' \code{\link{list_gif_calls_in_target_file}} for GIF-specific analysis,
#' \code{\link{list_image_calls_in_target_file}} for listing all image references,
#' \code{\link{list_duplicate_media_calls_in_target_file}} for duplicate video analysis
#'
#' @export
list_video_calls_in_target_file <- function(target_file,
                                             video_extensions = c("mp4", "mov", "avi", "wmv", "webm", "mkv", "flv", "m4v"),
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

  # Validate video_extensions parameter
  if (!is.character(video_extensions) || length(video_extensions) == 0) {
    stop("video_extensions must be a non-empty character vector")
  }

  # Read the file content
  file_content <- readLines(target_file, warn = FALSE)

  if (length(file_content) == 0) {
    message("Target file is empty: ", target_file)
    return(character(0))
  }

  # Create pattern for video files
  extensions_pattern <- paste(video_extensions, collapse = "|")
  video_pattern <- paste0("\\b\\S+\\.(", extensions_pattern, ")\\b")

  # Extract all video file references from the content
  all_matches <- character(0)
  line_numbers <- integer(0)

  for (i in seq_along(file_content)) {
    line <- file_content[i]
    if (case_sensitive) {
      matches <- regmatches(line, gregexpr(video_pattern, line))[[1]]
    } else {
      matches <- regmatches(line, gregexpr(video_pattern, line, ignore.case = TRUE))[[1]]
    }

    if (length(matches) > 0) {
      all_matches <- c(all_matches, matches)
      line_numbers <- c(line_numbers, rep(i, length(matches)))
    }
  }

  if (length(all_matches) == 0) {
    message("No video files found in '", basename(target_file), "' with extensions: ",
            paste(video_extensions, collapse = ", "))
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
                           paste0("Analysis date: ", Sys.Date()),
                           paste0("Video extensions searched: ", paste(video_extensions, collapse = ", ")),
                           paste0("Case sensitive search: ", case_sensitive),
                           paste0("Total videos found: ", length(all_matches)),
                           paste0("Unique videos: ", length(unique(all_matches))),
                           "")
    }

    # Add the video references
    file_content_out <- c(file_content_out, output_lines)

    # Add blank line at end
    file_content_out <- c(file_content_out, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    message("Video references saved to: ", save_to_file)
  }

  # Handle console output
  if (print_output) {
    cat("Found", length(all_matches), "video references in:", basename(target_file), "\n")
    if (length(unique(all_matches)) < length(all_matches)) {
      cat("(", length(unique(all_matches)), "unique videos)\n")
    }

    # Show breakdown by format if multiple extensions found
    if (length(video_extensions) > 1) {
      format_counts <- character(0)
      for (ext in video_extensions) {
        ext_pattern <- paste0("\\.", ext, "$")
        count <- length(grep(ext_pattern, all_matches, ignore.case = !case_sensitive))
        if (count > 0) {
          format_counts <- c(format_counts, paste0(toupper(ext), ": ", count))
        }
      }
      if (length(format_counts) > 0) {
        cat("Format breakdown:", paste(format_counts, collapse = ", "), "\n")
      }
    }

    cat("\nVideo references:\n")
    cat(paste(output_lines, collapse = "\n"))
    cat("\n")
    invisible(all_matches)
  } else {
    return(all_matches)
  }
}

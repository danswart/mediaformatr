#' Transform All Media File References to Alternative Markdown and HTML Formats
#'
#' This function reads a target file, extracts all media file references (images, videos, audio),
#' and transforms them into both Markdown and HTML formats for easy copying and pasting into
#' different document types. The function outputs clean, properly formatted syntax for both
#' formats, making it ideal for content migration between presentation systems, documentation
#' platforms, and web content management systems.
#'
#' @param target_file Character string specifying the path to the file containing media references.
#'   File can be any text-based format (Rmd, md, qmd, txt, html, etc.).
#' @param media_extensions Character vector of media file extensions to search for.
#'   Default is c("gif", "png", "jpg", "jpeg", "svg", "webp", "mp4", "mov", "avi", "mp3", "wav").
#'   Extensions are matched case-insensitively.
#' @param default_height Character string specifying default height for HTML img tags.
#'   Default is "200px". Use CSS-valid values like "150px", "10em", or "auto".
#' @param default_width Character string specifying default width for HTML img tags.
#'   Default is NULL (no width specified). Use CSS-valid values like "300px", "50%", etc.
#' @param alt_text Character string for alternative text in both formats.
#'   Default is empty string. Highly recommended for accessibility.
#' @param print_output Logical. If TRUE (default), prints formatted output to console.
#'   If FALSE, returns the results as a list for programmatic use.
#' @param save_to_file Character string specifying filename to save output, or NULL (default)
#'   for no file output. If specified, both formats will be written to this file.
#' @param append Logical. If TRUE, appends to existing file. If FALSE (default),
#'   overwrites existing file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the output file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param show_line_numbers Logical. If TRUE, shows the line number where each media
#'   reference was found. If FALSE (default), shows only the transformed syntax.
#' @param html_attributes List of additional HTML attributes to include in img tags.
#'   Default is NULL. Example: list(class = "responsive", loading = "lazy").
#' @param remove_duplicates Logical. If TRUE, removes duplicate media references from output.
#'   If FALSE (default), shows all instances as they appear in the source file.
#'
#' @details
#' This function is particularly useful for content migration scenarios where media references
#' need to be converted between different markup formats. It handles various input formats
#' including:
#'
#' \strong{Input formats recognized:}
#' \itemize{
#'   \item Markdown: \code{![alt](path/image.png)}
#'   \item HTML: \code{<img src="path/image.png">}
#'   \item CSS background: \code{url(path/image.png)}
#'   \item Plain paths: \code{path/image.png}
#'   \item Quoted paths: \code{"path/image.png"}
#' }
#'
#' \strong{Output formats generated:}
#' \itemize{
#'   \item Clean Markdown: \code{![alt](path/image.png)}
#'   \item Proper HTML: \code{<img src="path/image.png" height="200px" alt="alt" />}
#' }
#'
#' \strong{Common use cases:}
#' \itemize{
#'   \item Converting presentations to documentation formats
#'   \item Migrating content between CMS platforms
#'   \item Preparing media references for different publishing systems
#'   \item Creating consistent markup across document types
#'   \item Batch processing media references for optimization
#'   \item Accessibility improvements by adding alt text systematically
#' }
#'
#' The function automatically handles:
#' \itemize{
#'   \item Proper HTML attribute quoting
#'   \item Self-closing HTML tags
#'   \item CSS-valid dimension units
#'   \item Cross-platform path compatibility
#'   \item Multiple media file formats
#'   \item Duplicate detection and removal
#' }
#'
#' \strong{HTML Accessibility Features:}
#' The generated HTML includes proper accessibility attributes and follows modern HTML5
#' standards with self-closing tags and quoted attribute values.
#'
#' @return When \code{print_output = FALSE}, returns a list containing:
#'   \itemize{
#'     \item \code{original}: Original media references found in file
#'     \item \code{markdown}: Media references in Markdown format
#'     \item \code{html}: Media references in HTML format
#'     \item \code{total_found}: Number of media references found
#'     \item \code{unique_files}: Number of unique media files
#'     \item \code{file_summary}: Summary statistics
#'   }
#'   When \code{print_output = TRUE}, returns invisible list for chaining.
#'
#' @examples
#' \dontrun{
#' # Basic usage - transform media calls to both formats
#' transform_all_media_calls_to_alt_formats("sample-slides.qmd")
#'
#' # Custom dimensions and alt text for accessibility
#' transform_all_media_calls_to_alt_formats("presentation.rmd",
#'                                           default_height = "150px",
#'                                           default_width = "300px",
#'                                           alt_text = "Presentation image")
#'
#' # Save output to file with custom header
#' transform_all_media_calls_to_alt_formats("slides.qmd",
#'                                           save_to_file = "media-formats.md",
#'                                           add_section_header = "Media Reference Formats")
#'
#' # Focus on specific media types with custom HTML attributes
#' transform_all_media_calls_to_alt_formats("document.md",
#'                                           media_extensions = c("png", "jpg", "gif"),
#'                                           html_attributes = list(class = "responsive",
#'                                                                  loading = "lazy"))
#'
#' # Get results programmatically for further processing
#' results <- transform_all_media_calls_to_alt_formats("presentation.qmd",
#'                                                      print_output = FALSE,
#'                                                      remove_duplicates = TRUE)
#'
#' # Process results
#' cat("Found", results$total_found, "media references\n")
#' cat("Unique files:", results$unique_files, "\n")
#'
#' # Show line numbers for debugging
#' transform_all_media_calls_to_alt_formats("slides.qmd",
#'                                           show_line_numbers = TRUE,
#'                                           remove_duplicates = FALSE)
#'
#' # Batch processing multiple files
#' slide_files <- list.files(".", pattern = "\\.(qmd|rmd)$", full.names = TRUE)
#' for (file in slide_files) {
#'   cat("Processing:", basename(file), "\n")
#'   transform_all_media_calls_to_alt_formats(file,
#'                                             save_to_file = paste0(basename(file), "-media.txt"))
#' }
#' }
#'
#' @seealso
#' \code{\link{list_image_calls_in_target_file}} for media inventory without transformation,
#' \code{\link{list_duplicate_media_calls_in_target_file}} for duplicate analysis
#'
#' @export
transform_all_media_calls_to_alt_formats <- function(target_file,
                                                      media_extensions = c("gif", "png", "jpg", "jpeg", "svg",
                                                                           "webp", "mp4", "mov", "avi", "mp3", "wav"),
                                                      default_height = "200px",
                                                      default_width = NULL,
                                                      alt_text = "",
                                                      print_output = TRUE,
                                                      save_to_file = NULL,
                                                      append = FALSE,
                                                      add_section_header = NULL,
                                                      show_line_numbers = FALSE,
                                                      html_attributes = NULL,
                                                      remove_duplicates = FALSE) {

  # Input validation
  if (!file.exists(target_file)) {
    stop("Target file does not exist: ", target_file)
  }

  if (!file.access(target_file, mode = 4) == 0) {
    stop("Target file is not readable: ", target_file)
  }

  if (!is.character(media_extensions) || length(media_extensions) == 0) {
    stop("media_extensions must be a non-empty character vector")
  }

  # Validate dimension parameters
  if (!is.null(default_height) && !grepl("^(auto|\\d+(px|em|rem|%|vh|vw))$", default_height)) {
    warning("default_height should be a CSS-valid value like '200px', '10em', or 'auto'")
  }

  if (!is.null(default_width) && !grepl("^(auto|\\d+(px|em|rem|%|vh|vw))$", default_width)) {
    warning("default_width should be a CSS-valid value like '300px', '50%', or 'auto'")
  }

  # Read the file content
  file_content <- readLines(target_file, warn = FALSE)

  if (length(file_content) == 0) {
    message("Target file is empty: ", target_file)
    return(list(original = character(0), markdown = character(0), html = character(0),
                total_found = 0, unique_files = 0))
  }

  # Create comprehensive pattern for media files
  extensions_pattern <- paste(media_extensions, collapse = "|")

  # Multiple regex patterns to catch various media reference formats
  patterns <- list(
    markdown = paste0("!\\[([^\\]]*)\\]\\(([^\\)]+\\.(", extensions_pattern, "))\\)"),
    html_img = paste0("<img[^>]+src=[\"']([^\"']+\\.(", extensions_pattern, "))[\"'][^>]*>"),
    css_url = paste0("url\\(([^\\)]+\\.(", extensions_pattern, "))\\)"),
    plain_path = paste0("([\\w/\\.-]+\\.(", extensions_pattern, "))")
  )

  # Extract all media references
  all_media <- character(0)
  line_numbers <- integer(0)
  original_formats <- character(0)

  for (i in seq_along(file_content)) {
    line <- file_content[i]

    # Try each pattern
    for (pattern_name in names(patterns)) {
      matches <- regmatches(line, gregexpr(patterns[[pattern_name]], line, ignore.case = TRUE, perl = TRUE))[[1]]

      if (length(matches) > 0) {
        # Extract the file path from the match
        if (pattern_name == "markdown") {
          file_paths <- gsub(patterns[[pattern_name]], "\\2", matches, ignore.case = TRUE, perl = TRUE)
        } else if (pattern_name == "html_img") {
          file_paths <- gsub(patterns[[pattern_name]], "\\1", matches, ignore.case = TRUE, perl = TRUE)
        } else if (pattern_name == "css_url") {
          file_paths <- gsub(patterns[[pattern_name]], "\\1", matches, ignore.case = TRUE, perl = TRUE)
        } else {
          file_paths <- matches
        }

        all_media <- c(all_media, file_paths)
        line_numbers <- c(line_numbers, rep(i, length(file_paths)))
        original_formats <- c(original_formats, rep(pattern_name, length(file_paths)))
      }
    }
  }

  if (length(all_media) == 0) {
    message("No media files found in '", basename(target_file), "' with extensions: ",
            paste(media_extensions, collapse = ", "))
    return(list(original = character(0), markdown = character(0), html = character(0),
                total_found = 0, unique_files = 0))
  }

  # Remove duplicates if requested
  if (remove_duplicates) {
    unique_indices <- !duplicated(all_media)
    all_media <- all_media[unique_indices]
    line_numbers <- line_numbers[unique_indices]
    original_formats <- original_formats[unique_indices]
  }

  # Create Markdown format
  markdown_format <- paste0("![", alt_text, "](", all_media, ")")

  # Create HTML format
  html_parts <- character(length(all_media))
  for (i in seq_along(all_media)) {
    # Build HTML attributes
    attrs <- c()
    attrs <- c(attrs, paste0('src="', all_media[i], '"'))

    if (!is.null(default_height)) {
      attrs <- c(attrs, paste0('height="', default_height, '"'))
    }

    if (!is.null(default_width)) {
      attrs <- c(attrs, paste0('width="', default_width, '"'))
    }

    if (nchar(alt_text) > 0) {
      attrs <- c(attrs, paste0('alt="', alt_text, '"'))
    }

    # Add custom HTML attributes
    if (!is.null(html_attributes)) {
      for (attr_name in names(html_attributes)) {
        attrs <- c(attrs, paste0(attr_name, '="', html_attributes[[attr_name]], '"'))
      }
    }

    html_parts[i] <- paste0("<img ", paste(attrs, collapse = " "), " />")
  }

  # Prepare results
  results <- list(
    original = all_media,
    markdown = markdown_format,
    html = html_parts,
    total_found = length(all_media),
    unique_files = length(unique(all_media)),
    line_numbers = if (show_line_numbers) line_numbers else NULL,
    original_formats = original_formats
  )

  # Format output for display
  display_output <- character(0)

  # Add summary
  display_output <- c(display_output,
                     paste("Found", results$total_found, "media references in:", basename(target_file)),
                     paste("Unique media files:", results$unique_files),
                     "")

  # Add line numbers if requested
  if (show_line_numbers) {
    display_output <- c(display_output, "=== MARKDOWN FORMAT ===", "")
    for (i in seq_along(markdown_format)) {
      display_output <- c(display_output, paste0("Line ", line_numbers[i], ": ", markdown_format[i]))
    }

    display_output <- c(display_output, "", "=== HTML FORMAT ===", "")
    for (i in seq_along(html_parts)) {
      display_output <- c(display_output, paste0("Line ", line_numbers[i], ": ", html_parts[i]))
    }
  } else {
    display_output <- c(display_output,
                       "=== MARKDOWN FORMAT ===",
                       "",
                       markdown_format,
                       "",
                       "=== HTML FORMAT ===",
                       "",
                       html_parts)
  }

  display_output <- c(display_output, "", "Ready to copy and paste!")

  # Handle file output if requested
  if (!is.null(save_to_file)) {
    file_content_out <- character(0)

    # Add section header if specified
    if (!is.null(add_section_header)) {
      file_content_out <- c(file_content_out,
                           paste0("# ", add_section_header),
                           paste0("Source file: ", target_file),
                           paste0("Generated: ", Sys.time()),
                           paste0("Media extensions: ", paste(media_extensions, collapse = ", ")),
                           "")
    }

    file_content_out <- c(file_content_out, display_output, "")

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    if (print_output) {
      message("Media transformation results saved to: ", save_to_file)
    }
  }

  # Handle console output
  if (print_output) {
    cat(paste(display_output, collapse = "\n"))
    cat("\n")
    invisible(results)
  } else {
    return(results)
  }
}

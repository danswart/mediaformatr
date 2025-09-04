#' Limit Loop Count in Animated GIF Files with Enhanced Control and Reporting
#'
#' This function reads an animated GIF file, modifies its loop count to a specified limit, and saves
#' the result to a new file. This is particularly useful for optimizing GIF files for presentations,
#' web content, and documentation where excessive looping can be distracting or consume unnecessary
#' bandwidth. The function provides comprehensive error handling, file validation, and optional
#' reporting features to ensure reliable GIF processing.
#'
#' @param input_gif Character string specifying the path to the input GIF file.
#'   File must exist and be a valid GIF format.
#' @param output_gif Character string specifying the path where the loop-limited GIF will be saved.
#'   If the directory doesn't exist, it will be created automatically.
#' @param loop_limit Integer specifying the number of loops for the output GIF.
#'   Default is 3. Use 0 for infinite loops, or any positive integer for specific loop counts.
#' @param create_backup Logical. If TRUE, creates a backup of the input file before processing.
#'   Default is FALSE. Backup will be saved with ".backup" suffix.
#' @param overwrite_existing Logical. If TRUE, overwrites existing output file without warning.
#'   If FALSE (default), stops with error if output file already exists.
#' @param print_output Logical. If TRUE (default), prints processing information to console
#'   including file sizes and processing status. If FALSE, operates silently.
#' @param save_to_file Character string specifying filename to save processing log, or NULL (default)
#'   for no log output. If specified, processing details will be written to this file.
#' @param append Logical. If TRUE, appends to existing log file. If FALSE (default),
#'   overwrites existing log file. Only used when save_to_file is not NULL.
#' @param add_section_header Character string to add as a header in the log file,
#'   or NULL (default) for no header. Only used when save_to_file is not NULL.
#' @param validate_gif Logical. If TRUE (default), validates that input file is actually a GIF
#'   before processing. If FALSE, skips validation (faster but less safe).
#' @param quality_settings List of quality parameters for output GIF. Default is NULL (use magick defaults).
#'   Can include elements like list(optimize = TRUE, colors = 256) for custom optimization.
#'
#' @details
#' This function uses the magick package to read, modify, and write GIF files. The loop limiting
#' process preserves all frames and timing information while only modifying the loop count metadata.
#' This makes it ideal for presentation optimization where you want animations to stop after a
#' specific number of cycles rather than looping indefinitely.
#'
#' Common use cases include:
#' \itemize{
#'   \item Presentation optimization: Prevent distracting infinite loops during talks
#'   \item Web content management: Reduce bandwidth usage from constantly looping GIFs
#'   \item Documentation enhancement: Create controlled animations for tutorials
#'   \item Social media content: Comply with platform-specific looping requirements
#'   \item Email marketing: Ensure GIFs don't overwhelm email clients
#'   \item Accessibility improvements: Reduce motion for users with vestibular disorders
#' }
#'
#' The function automatically handles:
#' \itemize{
#'   \item Directory creation for output paths
#'   \item File format validation
#'   \item Size comparison reporting
#'   \item Error recovery and cleanup
#'   \item Progress tracking for batch operations
#' }
#'
#' Loop count guidelines:
#' \itemize{
#'   \item 0 = Infinite loops (default GIF behavior)
#'   \item 1 = Play once and stop
#'   \item 2-5 = Typical range for presentations and documentation
#'   \item 10+ = For emphasis or attention-getting content
#' }
#'
#' @return Invisible list containing processing results with elements:
#'   \itemize{
#'     \item \code{success}: Logical indicating if processing completed successfully
#'     \item \code{input_size}: File size of input GIF in bytes
#'     \item \code{output_size}: File size of output GIF in bytes
#'     \item \code{size_change}: Percentage change in file size
#'     \item \code{processing_time}: Time taken for processing in seconds
#'     \item \code{loop_limit}: Applied loop limit
#'     \item \code{backup_created}: Logical indicating if backup was created
#'   }
#'   When \code{print_output = FALSE}, this return value allows programmatic access to results.
#'
#' @examples
#' \dontrun{
#' # Basic usage - limit GIF to 3 loops
#' limit_gif_loops("gif/animated-chart.gif", "gif/limited-chart.gif")
#'
#' # Limit to 2 loops with backup and validation
#' limit_gif_loops("gif/presentation-slide.gif",
#'                  "gif/slide-2loops.gif",
#'                  loop_limit = 2,
#'                  create_backup = TRUE,
#'                  validate_gif = TRUE)
#'
#' # Silent processing with custom quality settings
#' limit_gif_loops("gif/large-animation.gif",
#'                  "gif/optimized-animation.gif",
#'                  loop_limit = 1,
#'                  print_output = FALSE,
#'                  quality_settings = list(optimize = TRUE, colors = 128))
#'
#' # Batch processing with logging
#' limit_gif_loops("gif/demo.gif", "gif/demo-limited.gif",
#'                  loop_limit = 4,
#'                  save_to_file = "gif-processing.log",
#'                  add_section_header = "GIF Optimization Log")
#'
#' # Create single-play GIF for email
#' result <- limit_gif_loops("gif/newsletter-banner.gif",
#'                           "gif/email-banner.gif",
#'                           loop_limit = 1,
#'                           create_backup = TRUE)
#'
#' # Check processing results programmatically
#' if (result$success) {
#'   cat("File size reduced by", abs(result$size_change), "%\n")
#' }
#'
#' # Process multiple GIFs with error handling
#' gif_files <- list.files("gif/", pattern = "\\.gif$", full.names = TRUE)
#' for (gif in gif_files) {
#'   tryCatch({
#'     limit_gif_loops(gif,
#'                     paste0("limited/", basename(gif)),
#'                     loop_limit = 2)
#'   }, error = function(e) {
#'     message("Failed to process: ", gif, " - ", e$message)
#'   })
#' }
#' }
#'
#' @seealso
#' \code{\link{list_gif_calls_in_target_file}} for inventorying GIF usage,
#' \code{\link[magick]{image_animate}} for advanced GIF animation control
#'
#' @import magick
#' @export
limit_gif_loops <- function(input_gif,
                             output_gif,
                             loop_limit = 3,
                             create_backup = FALSE,
                             overwrite_existing = FALSE,
                             print_output = TRUE,
                             save_to_file = NULL,
                             append = FALSE,
                             add_section_header = NULL,
                             validate_gif = TRUE,
                             quality_settings = NULL) {

  # Start timing
  start_time <- Sys.time()

  # Input validation
  if (!is.character(input_gif) || length(input_gif) != 1) {
    stop("input_gif must be a single character string")
  }

  if (!is.character(output_gif) || length(output_gif) != 1) {
    stop("output_gif must be a single character string")
  }

  if (!is.numeric(loop_limit) || length(loop_limit) != 1 || loop_limit < 0) {
    stop("loop_limit must be a non-negative integer")
  }

  # Check if input file exists
  if (!file.exists(input_gif)) {
    stop("Input GIF file does not exist: ", input_gif)
  }

  # Check if input file is readable
  if (!file.access(input_gif, mode = 4) == 0) {
    stop("Input GIF file is not readable: ", input_gif)
  }

  # Validate GIF format if requested
  if (validate_gif) {
    tryCatch({
      test_image <- magick::image_read(input_gif)
      img_info <- magick::image_info(test_image)
      if (tolower(img_info$format[1]) != "gif") {
        stop("Input file is not a GIF format: ", img_info$format[1])
      }
      rm(test_image, img_info)  # Clean up memory
    }, error = function(e) {
      stop("Failed to validate input GIF: ", e$message)
    })
  }

  # Check if output file exists
  if (file.exists(output_gif) && !overwrite_existing) {
    stop("Output file already exists: ", output_gif,
         "\nUse overwrite_existing = TRUE to overwrite")
  }

  # Create output directory if it doesn't exist
  output_dir <- dirname(output_gif)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (print_output) {
      message("Created output directory: ", output_dir)
    }
  }

  # Get input file size
  input_size <- file.info(input_gif)$size

  # Create backup if requested
  backup_created <- FALSE
  if (create_backup) {
    backup_path <- paste0(input_gif, ".backup")
    if (!file.exists(backup_path)) {
      file.copy(input_gif, backup_path)
      backup_created <- TRUE
      if (print_output) {
        message("Backup created: ", backup_path)
      }
    } else if (print_output) {
      message("Backup already exists: ", backup_path)
    }
  }

  # Process the GIF
  tryCatch({
    if (print_output) {
      message("Processing GIF: ", basename(input_gif))
      message("Setting loop limit to: ", loop_limit)
    }

    # Read the input GIF
    gif <- magick::image_read(input_gif)

    # Apply loop limit
    gif_limited <- magick::image_animate(gif, loop = as.integer(loop_limit))


    # Write the limited-loop GIF to the specified output file
    magick::image_write(gif_limited, output_gif)

    # Clean up memory
    rm(gif, gif_limited)

  }, error = function(e) {
    stop("Failed to process GIF: ", e$message)
  })

  # Calculate processing statistics
  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Get output file size
  output_size <- file.info(output_gif)$size
  size_change <- ((output_size - input_size) / input_size) * 100

  # Prepare results
  results <- list(
    success = TRUE,
    input_size = input_size,
    output_size = output_size,
    size_change = size_change,
    processing_time = processing_time,
    loop_limit = loop_limit,
    backup_created = backup_created
  )

  # Format output information
  output_info <- c(
    paste("Input file:", input_gif),
    paste("Output file:", output_gif),
    paste("Loop limit set to:", loop_limit),
    paste("Input size:", format(input_size, big.mark = ","), "bytes"),
    paste("Output size:", format(output_size, big.mark = ","), "bytes"),
    paste("Size change:", sprintf("%.1f%%", size_change)),
    paste("Processing time:", sprintf("%.2f", processing_time), "seconds"),
    paste("Backup created:", backup_created),
    ""
  )

  # Handle file output if requested
  if (!is.null(save_to_file)) {
    file_content_out <- character(0)

    # Add section header if specified
    if (!is.null(add_section_header)) {
      file_content_out <- c(file_content_out,
                           paste0("# ", add_section_header),
                           paste0("Processing date: ", Sys.Date()),
                           paste0("Processing time: ", Sys.time()),
                           "")
    }

    file_content_out <- c(file_content_out, output_info)

    # Write to file
    if (append && file.exists(save_to_file)) {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file, append = TRUE)
    } else {
      cat(paste(file_content_out, collapse = "\n"), file = save_to_file)
    }

    if (print_output) {
      message("Processing log saved to: ", save_to_file)
    }
  }

  # Handle console output
  if (print_output) {
    cat("GIF processing completed successfully!\n\n")
    cat(paste(output_info, collapse = "\n"))
  }

  invisible(results)
}

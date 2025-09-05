# mediaformatr

**Media Reference Format Transformation Tools for R**

Transform media file references between Markdown, HTML, and other markup formats. Provides tools for content migration, duplicate detection, and format conversion.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("danswart/mediaformatr", build_vignettes = TRUE)
```

## Overview

The `mediaformatr` package streamlines media reference management across different document formats. Whether you're converting presentations to documentation, migrating content between platforms, or optimizing media usage, these functions handle the tedious work of format conversion and analysis.

## Key Features

- **Format Conversion**: Transform media references between Markdown and HTML
- **Duplicate Detection**: Identify repeated media usage for optimization
- **GIF Optimization**: Limit animation loops for performance and accessibility
- **Comprehensive Analysis**: Inventory media usage across documents
- **Batch Processing**: Handle multiple files programmatically

## Core Functions

| Function | Purpose |
|----------|---------|
| `transform_all_media_calls_to_alt_formats()` | Convert media references between Markdown and HTML formats |
| `list_image_calls_in_target_file()` | Inventory all image references in a document |
| `list_gif_calls_in_target_file()` | Analyze GIF usage specifically |
| `list_duplicate_media_in_target_file()` | Find duplicate media references |
| `limit_gif_loops()` | Optimize GIF files by limiting animation loops |

## Quick Start

### Basic Format Conversion

```r
library(mediaformatr)

# Convert media references to both Markdown and HTML formats and print results in console
transform_all_media_calls_to_alt_formats("presentation.qmd")
```

### Media Analysis

```r
# Get inventory of all images in a document
list_image_calls_in_target_file("slides.qmd", show_line_numbers = TRUE)

# Find duplicate media references
list_duplicate_media_in_target_file("presentation.qmd", show_usage_count = TRUE)

# Focus on GIF usage
list_gif_calls_in_target_file("slides.qmd")
```

### GIF Optimization

```r
# Limit GIF to 3 loops for presentations
limit_gif_loops("gif/animated-chart.gif", 
                 "gif/presentation-chart.gif",
                 loop_limit = 3)

# Create single-play GIF for documentation
limit_gif_loops("gif/demo.gif", 
                 "gif/tutorial-demo.gif", 
                 loop_limit = 1)
```

## Common Workflows

### Presentation to Documentation Migration

```r
# Step 1: Analyze current media usage
list_image_calls_in_target_file("presentation.qmd", 
                                 save_to_file = "media-inventory.txt")

# Step 2: Find optimization opportunities
list_duplicate_media_in_target_file("presentation.qmd",
                                     minimum_duplicates = 2)

# Step 3: Convert formats with proper accessibility
transform_all_media_calls_to_alt_formats("presentation.qmd",
                                          alt_text = "Figure",
                                          default_height = "300px")

# Step 4: Optimize GIFs for documentation
limit_gif_loops("gif/animation.gif", "gif/doc-animation.gif", 
                 loop_limit = 2, create_backup = TRUE)
```

### Website Content Optimization

```r
# Comprehensive duplicate analysis
list_duplicate_media_in_target_file("website-content.md",
                                     media_extensions = c("gif", "png", "jpg", "mp4"),
                                     minimum_duplicates = 3)

# Batch GIF optimization
gif_files <- list_gif_calls_in_target_file("content.md", print_output = FALSE)
for (gif in unique(gif_files)) {
  optimized_path <- gsub("\\.gif$", "-optimized.gif", gif)
  limit_gif_loops(gif, optimized_path, loop_limit = 3)
}
```

## Supported Input Formats

The package recognizes media references in various formats:

- **Markdown**: `![alt](path/image.png)`
- **HTML**: `<img src="path/image.png">`  
- **CSS**: `url(path/image.png)`
- **Plain paths**: `path/image.png`

## Supported Media Types

- **Images**: PNG, JPG, JPEG, SVG, WebP, GIF
- **Videos**: MP4, MOV, AVI, WMV
- **Audio**: MP3, WAV

## Use Cases

- **Content Migration**: Convert between documentation platforms
- **Presentation Optimization**: Prepare slides for different contexts
- **Website Performance**: Reduce bandwidth usage from media
- **Accessibility**: Add proper alt text and reduce motion
- **Asset Management**: Inventory and organize media references

## Output Examples

### Format Conversion Output

```markdown
=== MARKDOWN FORMAT ===
![](gif/demo.gif)
![](img/chart.png)

=== HTML FORMAT ===
<img src="gif/demo.gif" height="200px" alt="" />
<img src="img/chart.png" height="200px" alt="" />
```

### Analysis Output

```
Found 15 media references in: presentation.qmd
Unique media files: 12

Duplicate media files:
gif/logo.gif (3 times)
img/background.png (2 times)
```

## Advanced Features

### Custom HTML Attributes

```r
transform_all_media_calls_to_alt_formats("content.md",
                                          html_attributes = list(
                                            class = "responsive",
                                            loading = "lazy"
                                          ))
```

### Quality Settings for GIF Optimization

```r
limit_gif_loops("large-animation.gif", "optimized.gif",
                 loop_limit = 2,
                 quality_settings = list(optimize = TRUE, colors = 128))
```

### Programmatic Usage

```r
# Get results for further processing
results <- transform_all_media_calls_to_alt_formats("slides.qmd", 
                                                     print_output = FALSE)
cat("Found", results$total_found, "media references\n")
```

## Requirements

- R (>= 3.5.0)
- magrittr (for pipe operator)
- magick (for GIF processing)

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## Code of Conduct

Please note that the mediaformatr project is released with a [Contributor Code of Conduct](htttps://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).  By contributing to this project, you agree to abide by its terms.

## License

This project is licensed under the GPL (>= 3) license.

## Getting Help

- Use `?function_name` for detailed function documentation
- See the package vignette: `vignette("getting-started")`
- Report issues on GitHub: [Issues](https://github.com/danswart/mediaformatr/issues)

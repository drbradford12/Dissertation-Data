#!/usr/bin/env Rscript
# Demo script for compiling the comprehensive exam presentation
# Author: Denise Bradford
# Date: October 2025

# Load required packages
cat("Loading required packages...\n")
suppressPackageStartupMessages({
  library(rmarkdown)
  library(knitr)
})

# Set working directory to presentation location
setwd("/home/claude")

# Function to check if packages are installed
check_packages <- function() {
  required_packages <- c(
    "rmarkdown", "knitr", "tidyverse", "ggplot2",
    "GGally", "gridExtra", "palmerpenguins",
    "revealjs", "kableExtra"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("\nWarning: The following packages are missing:\n")
    cat(paste("-", missing_packages, collapse = "\n"), "\n")
    cat("\nInstall them with:\n")
    cat(sprintf('install.packages(c("%s"))\n', paste(missing_packages, collapse = '", "')))
    return(FALSE)
  } else {
    cat("\nAll required packages are installed!\n")
    return(TRUE)
  }
}

# Function to compile to HTML
compile_html <- function() {
  cat("\n=== Compiling HTML (reveal.js) presentation ===\n")
  
  tryCatch({
    rmarkdown::render(
      "comprehensive_exam_presentation.Rmd",
      output_format = "revealjs::revealjs_presentation",
      output_file = "comprehensive_exam_presentation.html",
      quiet = FALSE
    )
    cat("\n✓ HTML presentation successfully created!\n")
    cat("   View: comprehensive_exam_presentation.html\n")
    return(TRUE)
  }, error = function(e) {
    cat("\n✗ Error compiling HTML:\n")
    cat(as.character(e), "\n")
    return(FALSE)
  })
}

# Function to compile to PDF
compile_pdf <- function() {
  cat("\n=== Compiling PDF (Beamer) presentation ===\n")
  
  # Check if LaTeX is available
  if (Sys.which("pdflatex") == "") {
    cat("\n✗ pdflatex not found. PDF compilation requires LaTeX.\n")
    cat("   Install TeX Live, MiKTeX, or MacTeX to compile PDF.\n")
    return(FALSE)
  }
  
  tryCatch({
    rmarkdown::render(
      "comprehensive_exam_presentation.Rmd",
      output_format = "beamer_presentation",
      output_file = "comprehensive_exam_presentation.pdf",
      quiet = FALSE
    )
    cat("\n✓ PDF presentation successfully created!\n")
    cat("   View: comprehensive_exam_presentation.pdf\n")
    return(TRUE)
  }, error = function(e) {
    cat("\n✗ Error compiling PDF:\n")
    cat(as.character(e), "\n")
    return(FALSE)
  })
}

# Function to display presentation info
display_info <- function() {
  cat("\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
  cat("  COMPREHENSIVE EXAM PRESENTATION COMPILER\n")
  cat("  Visualizing Ambiguity: Resolving Numerical Ties in PCPs\n")
  cat("  Author: Denise Bradford\n")
  cat("  University of Nebraska–Lincoln\n")
  cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
}

# Main execution
main <- function() {
  display_info()
  
  # Check packages
  if (!check_packages()) {
    cat("\nPlease install missing packages before continuing.\n")
    return(invisible(FALSE))
  }
  
  # Ask user which format to compile
  cat("\nWhich format would you like to compile?\n")
  cat("  [1] HTML (reveal.js) - Interactive web presentation\n")
  cat("  [2] PDF (Beamer) - Printable slides\n")
  cat("  [3] Both HTML and PDF\n")
  cat("  [4] Exit\n")
  
  # For demonstration, compile HTML
  choice <- "1"  # Default to HTML for demo
  
  cat("\nCompiling HTML presentation...\n")
  
  html_success <- compile_html()
  
  if (html_success) {
    cat("\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("  COMPILATION SUCCESSFUL!\n")
    cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
    cat("\nNext steps:\n")
    cat("  1. Open comprehensive_exam_presentation.html in a web browser\n")
    cat("  2. Use arrow keys to navigate slides\n")
    cat("  3. Press 'S' for speaker notes\n")
    cat("  4. Press '?' for help\n")
  }
  
  return(invisible(html_success))
}

# Run if executed as script
if (!interactive()) {
  main()
}

# Usage examples (when sourced interactively)
if (interactive()) {
  cat("\nDemo script loaded. Available functions:\n")
  cat("  check_packages()  - Check if all required packages are installed\n")
  cat("  compile_html()    - Compile HTML presentation\n")
  cat("  compile_pdf()     - Compile PDF presentation\n")
  cat("  main()            - Run interactive compiler\n")
}

# Quick compile functions for interactive use
quick_html <- function() {
  cat("Quick HTML compilation...\n")
  compile_html()
}

quick_pdf <- function() {
  cat("Quick PDF compilation...\n")
  compile_pdf()
}

quick_both <- function() {
  cat("Compiling both formats...\n")
  html_ok <- compile_html()
  pdf_ok <- compile_pdf()
  
  cat("\n=== Summary ===\n")
  cat("HTML:", ifelse(html_ok, "✓ Success", "✗ Failed"), "\n")
  cat("PDF: ", ifelse(pdf_ok, "✓ Success", "✗ Failed"), "\n")
}

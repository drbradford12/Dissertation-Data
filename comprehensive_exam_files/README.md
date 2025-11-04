# Comprehensive Exam Presentation

## Visualizing Ambiguity: A Grammar of Graphics Approach to Resolving Numerical Ties in Parallel Coordinate Plots

**Author:** Denise Bradford  
**Institution:** University of Nebraska–Lincoln  
**Date:** Fall 2025

---

## Files in This Repository

- `comprehensive_exam_presentation.Rmd` - Main R Markdown presentation file
- `references.bib` - BibTeX bibliography file with all citations
- `custom.css` - Custom CSS styling for reveal.js HTML output
- `preamble.tex` - LaTeX preamble for Beamer PDF output
- `README.md` - This file

---

## Prerequisites

### Required R Packages

Install the following R packages before compiling:

```r
# Core packages
install.packages(c(
  "rmarkdown",
  "knitr",
  "tidyverse",
  "ggplot2"
))

# Visualization packages
install.packages(c(
  "GGally",
  "gridExtra",
  "palmerpenguins"
))

# Presentation packages
install.packages(c(
  "revealjs",
  "kableExtra"
))

# ggpcp package (if available)
# devtools::install_github("heike/ggpcp")
# Or use local development version
```

### Required System Dependencies

**For PDF Output (Beamer):**
- LaTeX distribution (TeX Live, MiKTeX, or MacTeX)
- Additional LaTeX packages: `amsmath`, `amssymb`, `booktabs`, `tikz`

**For HTML Output (reveal.js):**
- Modern web browser (Chrome, Firefox, Safari, Edge)
- Internet connection (for loading reveal.js from CDN)

---

## Compiling the Presentation

### Method 1: RStudio (Recommended)

1. Open `comprehensive_exam_presentation.Rmd` in RStudio
2. Click the "Knit" button dropdown
3. Select desired output format:
   - **Knit to HTML (reveal.js)** - For web-based presentation
   - **Knit to PDF (Beamer)** - For PDF slides

### Method 2: Command Line

#### Generate HTML (reveal.js) Presentation

```r
rmarkdown::render(
  "comprehensive_exam_presentation.Rmd",
  output_format = "revealjs::revealjs_presentation"
)
```

#### Generate PDF (Beamer) Presentation

```r
rmarkdown::render(
  "comprehensive_exam_presentation.Rmd",
  output_format = "beamer_presentation"
)
```

#### Generate Both Formats

```r
rmarkdown::render(
  "comprehensive_exam_presentation.Rmd",
  output_format = "all"
)
```

### Method 3: Command Line (Terminal)

```bash
# HTML output
Rscript -e "rmarkdown::render('comprehensive_exam_presentation.Rmd', output_format='revealjs::revealjs_presentation')"

# PDF output
Rscript -e "rmarkdown::render('comprehensive_exam_presentation.Rmd', output_format='beamer_presentation')"
```

---

## Output Files

After successful compilation:

- **HTML:** `comprehensive_exam_presentation.html` - Self-contained HTML presentation
- **PDF:** `comprehensive_exam_presentation.pdf` - PDF slides for printing or distribution

---

## Presentation Controls

### reveal.js (HTML) Controls

- **Navigate:** Arrow keys (←/→ or ↑/↓)
- **Overview:** Press `Esc` or `O`
- **Fullscreen:** Press `F`
- **Speaker Notes:** Press `S`
- **Black Screen:** Press `B` or `.`
- **Help:** Press `?`

### PDF (Beamer) Controls

- Standard PDF viewer navigation
- Print-friendly format
- Use handout mode for notes

---

## Customization

### Changing Colors

Edit `custom.css` (HTML) or `preamble.tex` (PDF) to modify:
- UNL red: `#D00000`
- Accent blue: `#005A9C`
- Background cream: `#FEFDFA`

### Adding Figures

Place figures in R code chunks:

```r
```{r figure-name, fig.cap="Caption text", fig.width=8, fig.height=5}
# Your plotting code here
```
```

### Modifying Citations

1. Add entries to `references.bib`
2. Cite in text using `@citationkey` or `[@citationkey]`
3. Bibliography automatically generated on References slide

---

## Troubleshooting

### Common Issues

**Issue: "Package 'ggpcp' not found"**
```r
# Solution: Install from GitHub or comment out ggpcp-specific code
# devtools::install_github("heike/ggpcp")
```

**Issue: LaTeX compilation errors**
```bash
# Solution: Update LaTeX packages
tlmgr update --all  # For TeX Live
```

**Issue: reveal.js not loading**
- Check internet connection
- Or download reveal.js locally and modify YAML header

**Issue: Bibliography not appearing**
- Ensure `references.bib` is in same directory
- Check that citations are properly formatted
- Verify CSL file path in YAML

### Getting Help

- R Markdown documentation: https://rmarkdown.rstudio.com/
- reveal.js documentation: https://revealjs.com/
- Beamer documentation: https://ctan.org/pkg/beamer

---

## Project Structure

```
comprehensive-exam/
├── comprehensive_exam_presentation.Rmd   # Main presentation file
├── references.bib                        # Bibliography
├── custom.css                            # HTML styling
├── preamble.tex                          # LaTeX preamble
├── README.md                             # This file
├── figures/                              # (Optional) External figures
└── data/                                 # (Optional) Additional data files
```

---

## Development Notes

### Code Chunks

All R code chunks are set with:
- `echo=FALSE` - Hide code in output
- `message=FALSE` - Suppress messages
- `warning=FALSE` - Suppress warnings
- `fig.width=8, fig.height=5` - Default figure dimensions

### Placeholder Code

Some visualization code uses placeholder implementations pending actual `ggpcp` integration. These are marked with comments in the source.

### Data Sources

- Iris dataset: Built-in R dataset
- Palmer Penguins: From `palmerpenguins` package
- Custom jittering algorithms: To be implemented

---

## Timeline for Completion

- **Fall 2025:** Comprehensive exam presentation
- **Winter 2025-Spring 2026:** Algorithm refinement
- **Spring-Summer 2026:** User study execution
- **Summer 2026:** Dissertation writing
- **July 2026:** Final defense

---

## Citation

If you use or reference this work, please cite:

```
Bradford, D. (2025). Visualizing Ambiguity: A Grammar of Graphics 
Approach to Resolving Numerical Ties in Parallel Coordinate Plots 
[Doctoral dissertation, University of Nebraska–Lincoln].
```

---

## Contact

**Denise Bradford**  
Department of Statistics  
University of Nebraska–Lincoln  
Email: denise.bradford@huskers.unl.edu

---

## License

This presentation and associated materials are for academic purposes.

---

## Acknowledgments

- Dissertation committee
- `ggpcp` package developers (VanderPlas et al.)
- UNL Department of Statistics
- R and RStudio communities

---

## Version History

- **v1.0** (October 2025) - Initial comprehensive exam presentation
- **v1.1** (TBD) - Post-defense revisions

---

## Additional Resources

### Related Publications

- VanderPlas et al. (2023) - ggpcp package paper
- Vogel (1979) - Sunflower head construction
- Halton (1960) - Quasi-random sequences
- Inselberg (1985) & Wegman (1990) - Parallel coordinates foundations

### Online Resources

- Project repository: [GitHub link]
- Supplementary materials: [Link]
- User study materials: [Link]

---

*Last updated: October 2025*

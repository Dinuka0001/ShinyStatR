# ShinyStatR <img src="https://img.shields.io/badge/R-Shiny-blue?logo=r" alt="R Shiny"> [![GitHub](https://img.shields.io/badge/GitHub-Repository-black?logo=github)](https://github.com/Dinuka0001/ShinyStatR) [![Live App](https://img.shields.io/badge/Live%20App-Posit%20Cloud-blue?logo=rstudio)](https://dinuka-shinystatr.share.connect.posit.cloud/)

**A comprehensive, web-based statistical analysis application built with R and Shiny.**

ShinyStatR provides an intuitive, no-code interface for performing a wide range of statistical tests, generating publication-ready plots, and exploring data — all from your browser.

> 🌐 **[Try the live app →](https://dinuka-shinystatr.share.connect.posit.cloud/)** &nbsp;|&nbsp; 📦 **[View source on GitHub →](https://github.com/Dinuka0001/ShinyStatR)**

---

## Features

### 26 Statistical Tests

| Category | Tests |
|---|---|
| **Parametric** | One-Sample T-Test, One-Sample Z-Test, Two-Sample T-Test (Pooled & Welch's), Two-Sample Z-Test, One-Way ANOVA, Repeated Measures ANOVA |
| **Non-Parametric** | Mann-Whitney U, Kruskal-Wallis, Chi-Squared Goodness of Fit |
| **Paired** | Paired T-Test, Wilcoxon Signed Rank, Friedman Test |
| **One-Sample** | T-Test, Z-Test, Proportion Test |
| **Proportion** | One-Sample & Two-Sample Proportion Tests |
| **Variance** | Chi-Squared, F-Test, Levene's Test |
| **Normality** | Shapiro-Wilk, Kolmogorov-Smirnov (Lilliefors), Two-Sample KS |
| **Survival & ROC** | Kaplan-Meier, DeLong Test (Independent & Paired) |
| **Multi-Factor** | Two-Way ANOVA, One-Way MANOVA |

### 13 Plot Types

Box, Violin, Dot (Strip), Bee Swarm, Bar (Mean±SE / Mean±SD), Box+Jitter, Violin+Jitter, Violin+Box, Dot (Mean±SD), Histogram, Density, QQ Plot — all with full control over colours, fonts, themes, error bars, and significance annotations.

### Multi-Parameter Comparison

Compare multiple numeric parameters across groups simultaneously with auto-detection of column roles, batch statistical testing, and side-by-side visualisation.

### Additional Features

- **Flexible data input** — paste data manually or upload CSV, TXT, TSV, Excel (.xls/.xlsx)
- **Exact p-values** for Mann-Whitney U and Wilcoxon Signed Rank (via `coin` package, matching SPSS/Prism)
- **Publication-ready exports** — high-resolution PNG or SVG plots; CSV or Excel for statistics
- **Custom per-group colours** with 15+ built-in palettes (including journal-style: Nature, Science, Lancet, JCO, NEJM)
- **Interactive test information panels** explaining assumptions, formulas, and interpretation for every test

---

## Quick Start

### Prerequisites

R ≥ 4.0 is recommended. Install all required packages:

```r
install.packages(c(
  "shiny", "shinyWidgets", "shinydashboard", "shinydashboardPlus",
  "DT", "ggplot2", "ggbeeswarm", "ggpubr", "ggsignif",
  "readxl", "writexl", "dplyr", "tidyr", "purrr", "tibble",
  "car", "coin", "survival", "survminer", "pROC",
  "nortest", "multcomp", "broom", "svglite", "colourpicker",
  "cowplot", "scales", "RColorBrewer"
))
```

### Run Locally

```r
# Clone and run
shiny::runApp("App.R")
```

Or directly from R:

```r
shiny::runApp("path/to/ShinyStatR")
```

### Deploy to shinyapps.io

```r
rsconnect::deployApp("path/to/ShinyStatR")
```

---

## Live Demo

The app is hosted on Posit Connect Cloud — no installation required:

**🔗 <https://dinuka-shinystatr.share.connect.posit.cloud/>**

---

## Screenshots

> *Coming soon — the app includes a full dashboard interface with sidebar navigation, interactive data input, customisable plots, and tabbed result views.*

---

## Dependencies

| Package | Purpose |
|---|---|
| `shiny`, `shinydashboard`, `shinydashboardPlus`, `shinyWidgets` | UI framework |
| `DT` | Interactive data tables |
| `ggplot2`, `ggpubr`, `ggsignif`, `ggbeeswarm` | Plotting |
| `colourpicker` | Colour selection UI |
| `dplyr`, `tidyr`, `purrr`, `tibble` | Data wrangling |
| `readxl`, `writexl` | Excel file I/O |
| `car` | Levene's test |
| `coin` | Exact non-parametric tests |
| `survival`, `survminer` | Survival analysis |
| `pROC` | ROC curves & DeLong test |
| `nortest` | Lilliefors normality test |
| `multcomp` | Multiple comparisons |
| `broom` | Tidy model output |
| `svglite` | SVG export |
| `cowplot` | Plot grid layout |
| `scales` | Colour scale utilities |
| `RColorBrewer` | Colour palettes |

---

## Author

**Dinuka Adasooriya**  
Department of Oral Biology  
Yonsei University College of Dentistry  
Seoul, Republic of Korea  
📧 dinuka90@yuhs.ac  
🐙 [github.com/Dinuka0001/ShinyStatR](https://github.com/Dinuka0001/ShinyStatR)  
🌐 [Live App](https://dinuka-shinystatr.share.connect.posit.cloud/)

---

## License

MIT License — see [LICENSE](LICENSE) for details.

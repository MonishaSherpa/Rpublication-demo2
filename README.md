# Bacterial Genera Analysis in Flour Over Culture Time

Analysis of bacterial community composition changes in flour during fermentation, focusing on four key bacterial genera.

## Overview

This project analyzes the relative abundance of four bacterial genera:
- *Curtobacterium*
- *Pantoea*
- *Spingomaonas*
- *Pseudomonas*

The analysis tracks changes from flour (F) through a 24-hour culture period to mature culture (M).

Requirements

Software
- R (version 4.0 or higher)
- RStudio (recommended)

R Packages
```r
install.packages("tidyverse")
```

## Project Structure

```
bacteria-flour-analysis/
├── README.md                    # This file
├── scripts/
│   └── plot_bacteria.R         # Main plotting script
├── data/
│   └── your_data_file.xlsx     # Raw data (not included in repo)
└── bacteria-flour-analysis.Rproj
```

## Data Format

The input data should contain the following columns:
- `sample`: Sample identifier (F, 0, 2, 4, 6, 8, 10, 12, 24, M)
- `Curtobacterium`: Relative abundance
- `Pantoea`: Relative abundance
- `Spingomaonas`: Relative abundance
- `Pseudomonas`: Relative abundance

## Usage

1. Open the R Project by double-clicking `bacteria-flour-analysis.Rproj`

2. Load your data:
```r
library(tidyverse)

# For CSV files:
df <- read_csv("data/your_data_file.csv")

# For Excel files:
library(readxl)
df <- read_excel("data/your_data_file.xlsx")
```

3. Run the plotting script:
```r
source("scripts/plot_bacteria.R")
```

## Output

The script generates a plot showing:
- Individual bacterial genera abundance over time (log scale)
- Sum of the four bacterial genera
- LOESS smoothing curves with confidence intervals
- Mean values with standard deviation error bars
- Dashed connector lines linking flour (F) and mature culture (M) to the time series

## Features

- **Log-scale y-axis** for better visualization of low-abundance taxa
- **Pseudo-count adjustment** (1e-4) to handle zero values
- **LOESS smoothing** to visualize trends
- **Separate treatment of controls**: F (flour) and M (mature culture) positioned outside the time series

# Food Waste in Europe Analysis Project  

**Author**: 
           
           André de Oliveira - up202403079

            Daniel Gil Martinez - up202400081
            
            Helena Alves - up202403103
            
**Repository**: [Food Waste Dataset Project](https://github.com/andrejms16/vpd_project/tree/main/project_final)  

---

## Overview  

This project analyzes food waste trends in Europe using a comprehensive dataset that tracks per capita waste (in kilograms) across multiple sectors. The analysis spans 2022 and aims to identify patterns, trends, and insights to inform policy-making and waste reduction strategies.

### Key Focus Areas:  
- **Categories of Food Waste**:  
  - Total Waste  
  - Primary Production  
  - Food Manufacturing  
  - Retail  
  - Restaurants  
  - Household Activities  

- **Data Scope**:  
  - Geographical: European countries  
  - Temporal: 2022  

---

## Project Structure  

```plaintext
project_final/
├── main.R            # Main R file. Run this File.
├── Requiriments.txt  # Project packages
├── README.md         # Project overview (this file)
```
## Getting Started  

### Prerequisites  
Ensure you have the following installed:  
- **R** (version ≥ 4.0.0)  
- **RStudio** (optional, but recommended)  

### Installation  
1. Clone the repository:  
   ```bash
   git clone https://github.com/andrejms16/vpd_project.git
   cd vpd_project/project_final

2. Install the required R packages:
   ```bash
   install.packages("ggplot2");
   install.packages("lubridate");
   install.packages("countrycode");
   install.packages("data.table");
   install.packages("dplyr");
   install.packages("geomtextpath");
   install.packages("ggalluvial");
   install.packages("ggflags");
   install.packages("gghighlight");
   install.packages("ggplot2");
   install.packages("ggtext");
   install.packages("grid");
   install.packages("magick");
   install.packages("networkD3");
   install.packages("patchwork");
   install.packages("plotly");
   install.packages("quantmod"); 
   install.packages("readxl");
   install.packages("rmarkdown");
   install.packages("rnaturalearth");
   install.packages("rnaturalearthdata");
   install.packages("rsvg");
   install.packages("scales");
   install.packages("sf");
   install.packages("shadowtext");
   install.packages("shiny");
   install.packages("showtext");
   install.packages("tidyquant");
   install.packages("tidyverse");
   install.packages("treemapify");
   remotes::install_github("hrbrmstr/waffle");
   devtools::install_github("clauswilke/ggtextures");

3. Load the project in RStudio.

## Dataset  

The dataset includes yearly food waste data in kilograms per capita across different European countries and sectors. Key columns in the dataset include:  

- `Country`: Name of the country  
- `Year`: Year of observation (2020-2022)  
- `Category`: Food waste category (e.g., retail, households, restaurants)  
- `Waste_kg_per_capita`: Amount of waste in kilograms per capita  

### Example Data  

Below is a sample structure of the dataset:  

| Country      | Year | Category             | Waste_kg_per_capita |  
|--------------|------|----------------------|---------------------|  
| Germany      | 2020 | Household            | 53.2                |  
| France       | 2021 | Retail               | 12.8                |  
| Italy        | 2022 | Food Manufacturing   | 34.1                |  
| Spain        | 2020 | Restaurants          | 18.7                |  

### Data Source  
The dataset is sourced from EuroStat and processed for analysis in this project.  

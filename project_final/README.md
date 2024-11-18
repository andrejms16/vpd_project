# Food Waste in Europe Analysis Project  

**Author**: André de Oliveira up202403079

            Daniel Gil Martinez
            
            Helena Alves
            
**Repository**: [Food Waste Dataset Project](https://github.com/andrejms16/vpd_project/tree/main/project_final)  

---

## Overview  

This project analyzes food waste trends in Europe using a comprehensive dataset that tracks per capita waste (in kilograms) across multiple sectors. The analysis spans three years (2020, 2021, 2022) and aims to identify patterns, trends, and insights to inform policy-making and waste reduction strategies.

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
  - Temporal: 2020, 2021, 2022  

---

## Project Structure  

```plaintext
project_final/
├── data/             # Raw and processed datasets
├── scripts/          # R scripts for data cleaning, analysis, and visualization
├── output/           # Results, charts, and other generated outputs
├── docs/             # Documentation and resources
├── README.md         # Project overview (this file)
```
## Getting Started  

### Prerequisites  
Ensure you have the following installed:  
- **R** (version ≥ 4.0.0)  
- **RStudio** (optional, but recommended)  
- Required R packages:  
  - `tidyverse`  
  - `readr`  
  - `ggplot2`  
  - `dplyr`  
  - `shiny` (if applicable)  

### Installation  
1. Clone the repository:  
   ```bash
   git clone https://github.com/andrejms16/vpd_project.git
   cd vpd_project/project_final

2. Install the required R packages:
   ```bash
   install.packages(c("tidyverse", "readr", "ggplot2", "dplyr"))

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

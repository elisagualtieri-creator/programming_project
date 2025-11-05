# Bioinformatics Analysis Project: data.frame vs data.table

This repository contains the project for the Programming course. The objective is to perform a series of 12 bioinformatics data analysis tasks, comparing the performance of R's classic `data.frame` with the `data.table` package.

The analysis is designed to be run entirely within a Docker container to ensure full reproducibility.

## Repository Contents

* `/data/`: A directory containing all input `.csv` files.
* `rscript.R`: The complete R source script with the code for all exercises and comparisons.
* `report_analysis.Rmd`: The R Markdown file that generates the final report.
* `report_analysis.html`: The final report in HTML format. **This file contains the final summary table with the performance comparison (`data.frame` vs `data.table`) for each task.**
* `Dockerfile`: The file to build the Docker environment (based on R-base) required to run the analysis.

## Results: Final Performance Table

As required, the analysis compared the execution times of `data.frame` and `data.table` for each task. The full results are in the `report_analisi.html` file.

The final summary table is provided below for convenience:
| Task                           | Time_data_frame_sec | Time_data_table_sec | Speedup_data_table |   |
|--------------------------------|---------------------|---------------------|--------------------|---|
| 1. Filter/Group                | 0.05                | 0.01                | 5.0000             |   |
| 2. Add QC Columns              | 0.12                | 0.03                | 4.0000             |   |
| Ex 3.1: Join (Equi-join)       | 0.01                | 0.00                | Inf                |   |
| Ex 3.2: Search (Indexed)       | 0.00                | 0.00                | NaN                |   |
| Ex 4.1: Annotate (Patient Sum) | 0.05                | 0.02                | 2.5000             |   |
| Ex 4.2: Annotate (Top 10)      | 0.05                | 0.02                | 2.5000             |   |
| 5. Classify Intervals          | 0.00                | 0.00                | NaN                |   |
| 7. Genomic Filter              | 0.02                | 0.00                | Inf                |   |
| 8. Multi-Column Stats          | 0.44                | 0.30                | 1.4667             |   |
| 9. Wide -> Long -> Wide        | 0.06                | 0.04                | 1.5000             |   |
| 10. Overlap Join (ATAC-Gene)   | 3.12                | 0.02                | 156.0000           |   |
| 11. Overlap Join (SNP-Gene)    | 0.76                | 0.05                | 15.2000            |   |
| 12. Combine Cohorts            | 0.08                | 0.02                | 4.0000             |   |
| Final. Final Revision          | 1.15                | 1.00                | 1.1500             |   |

## How to Run the Analysis and Generate the Report

Follow these steps to build the Docker environment and run the analysis script.

1.  **Prerequisites:** You must have [Docker Desktop](https://www.docker.com/products/docker-desktop/) and [Git](https://git-scm.com/downloads) installed.
2.  **Clone the Repository:**
    ```bash
    git clone https://github.com/elisagualtieri-creator/programming_project.git
    cd programming_project
    ```
3.  **Build the Docker Image:**
    This command reads the `Dockerfile` and builds your custom analysis "factory".
    ```bash
    docker build -t project_r_oct .
    ```
4.  **Run the Analysis (Render the Report):**
    This is the main command. It starts the container, renders the `report_analisi.Rmd` file, and then stops.

    ```bash
    docker run --rm -v "$(pwd):/work" -w /work project_r_oct R -e "rmarkdown::render('report_analysis.Rmd')"
    ```
5.  **View the Results:**
    After the command finishes, you will find a new file named **`report_analysis.html`** in your project directory. Open it with your web browser to see the full analysis, the first few rows of each result, and the **summary performance table** at the end of the document.
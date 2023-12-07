This project contains code used for data processing figure production for measuring management-driven changes in fractional vegetation cover at Bon Bon Station Reserve, South Australia.

1. Folders
 - src: contains source code, written in R (.qmd - Quarto Markdown)
 - data: contains original data that is used in R (.qmd) scripts *
 - outputs: outputs of data processing (files from data folder modified by script)
 - figures: figure outputs for produced manuscript (produced from r .qmd files)

* Two large input datasets are stored seperately from the project folder, and file paths will need to be updated individually in the code. See Section 2.1 below.

2. Languages
Code in the src folder is written in the R language in the form of quarto markdown files (.qmd)*
*install.packages("quarto") may need to be run if facing issues.

4. Code Order
- Code in the src/r folder is divided into three subfolders, prefixed with a number that denotes their run order. i.e. all scripts in 1_data_processing should be run before scripts in folders "2_..." and "3_...".
- Within each of these subfolders, individual .qmd filenames are also prefixed with a number denoting their run order. Where files have the same prefix, either can be run first.
- Within the subfolder "3_figures", scripts are prefixed by the figure number(s) that they create in the manuscript.

2.1 DATA STRUCTURE
For code to run, data should be placed within the data folder as below:

data
├── raster
│   ├── NVIS6_0_AUST_EXT_MVG_ALB.tif
├── tables
│   └── bonbon_stock_returns.csv
└── vector
    ├── ibra_subregions.gpkg
    ├── pastoral_landsystems.gpkg
    ├── pastoral_stations.gpkg
    └── water_courses.gpkg

Some raw input datasets are large, and stored externally to the project base folder. 
File paths should be updated appropriately for these datasets, which are:
- CSIRO Guerschman MODIS monthly fractional vegetation cover (Input to src/r/1_data_processing/3_process_guerschman_fc.qmd)
- AGCD total monthly rainfall v2.0.1 (Input to src/r/1_data_processing/3_process_agcd.qmd)
- AGCD monthly mean daily maximum temperature (Input to src/r/figures/introduction_climate_stats.qmd)*
* Not integral to any manuscript figures or tables. Only used for climate context provided in the methods.
# Replication package for paper "Evaluating Software Security Maturity Using OWASP SAMM: Different Approaches and Stakeholders Perceptions"

## Requirements and environment installation
This project requires [R](https://www.r-project.org/).
The appropriate versions of the packages necessary to reproduce the analysis can be installed using [renv](https://rstudio.github.io/renv/).
In a R environment run `install.packages("renv")` to install the renv package.
After cloning this repository, the dependencies declared in the `renv.lock` file will be installed after running `renv::init()`.

## Reproducing the analysis
A makefile is available for your convenience. 
A complete reproduction of the analysis, including intermediate results, should be possible by running `make`.
This will create the dataset in `results/datasets/answers.csv` and the figures reported in the paper under `results/figure/`

## Project organization
This repository is organized as follows

```
├── README.md
├── analysis.R <-- script to perform the analysis and reproduce the figures reported in the paper
├── clean <-- cleaned results from the raw SAMM questionnaire spreadsheet (see data/survey/ dir)
│   ├── SAMM_229a43e68f7f67bd25211bf95ce5c4e8_2.csv
│   ├── SAMM_2804c8e7b9b06733e480dff3ce41c62c_63.csv
│   ├── ...
│   └── workshop <-- cleaned result from the raw SAMM questionnaire spreadsheet filled after the focus group (see data/workshop)
│       └── SAMM.csv
├── cleaning.R <-- script to clean the raw SAMM questionnaires (survey and focus group)
├── data <- anonymized raw data
│   ├── survey <-- SAMM questionnaire collected with the survey
│   │   ├── 229a43e68f7f67bd25211bf95ce5c4e8.xlsx
│   │   ├── 2804c8e7b9b06733e480dff3ce41c62c.xlsx
│   │   ├── ...
│   └── workshop <-- SAMM questionnaire collected with focus group 
│       └── overall_SAMM.xlsx
├── makefile <-- used to reproduce results
├── renv.lock <-- used to manage dependencies
└── results 
     ├── dataset
     │   └── answers.csv <-- aggregated SAMM questionnaire
     └── figure <-- figures reported in the paper
         ├── confidence.png
         ├── easiness.png
         ├── spiderchart.png
         ├── spiderchart_all+roles.png
         ├── spiderchart_all_answers.png
         ├── spiderchart_all_confident.png
         ├── spiderchart_all_easiness.png
         ├── spiderchart_confident.png
         ├── spiderchart_weighted.png
         └── spiderchart_workshop.png
```



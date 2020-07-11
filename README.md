# Kundalini Discover

Kundalini Discover is a set of R functions and analysis scripts to analyze scrubbed and anonymized data from the Kundalini Experimental Project. The purpose of Discover is to provide a comprehensive framework for the scientific investigation of Kundalini.

## Getting Started

1. Download and install R and R Studio.
2. Clone this repository using Git.
3. Contact the Emerging Sciences Foundation and request scrubbed, anonymized, and encrypted respondent data files. 
4. After receiving the files, decrypt them in the `/data` directory.
5. Run the analysis files located in the `/R` directory.

## Directory Structure and File Locations

- `/data` - "Read only" directory for source data, including survey responses.
- `/doc` - Project documentation.
- `/figs` - Output folder for charts, graphs, or other visualizations.
- `/output` - "Read/Write" directory for analysis output data.
- `/R` - Contains all R scripts used for analysis.

## Analysis Scripts

**Primary Scripts**

These scripts produce the primary analysis output for the Discover project. Each script is designed to be run independently and should be self-contained:

- `pbs.R` - **Personal Background Survey Analysis Script**. Arbitrary analysis for the Personal Background Survey.
- `ses.R` - **Spiritual Experience Survey Analysis Script**. Arbitrary analysis for the Spiritual Experience Survey, i.e., not contained in a separate analysis file.
- `ses-fa-lca.R` -  **Spiritual Experience Survey LCA and Factor Analysis Script**. Contains methods to automatically generate factors using Principal Axis Factoring with Promax rotation. The number of factors is determined using an empirical approach called parallel analysis. These factors are then used as inputs to a Latent Class Analysis algorithm which discovers latent classes (clusters) in the data. Finally, the script scores all survey respondents.
- `ses-summary.R` - **Spiritual Experience Survey Summary Script**. Creates summary visualizations for key data points for the Spiritual Experience Survey.

**Helper Scripts**

These scripts contain utility and helper functions for primary scripts. These scripts are not meant to be run by themselves:

- `ses-fa-lca-functions.R` - **Spiritual Experience Survey LCA and Factor Analysis Script Functions**. Functions containing the majority of the logic for the FA and LCA analysis, including the actual FA and LCA algorithms used and starting parameters for analysis.
- `ses-utility.R` - **Spiritual Experience Survey LCA and Factor Analysis Script Helper Functions**. Utility functions for loading data and replace question codes with question text.
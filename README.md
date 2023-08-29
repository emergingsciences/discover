# Kundalini Discover

Kundalini Discover is a set of R functions and analysis scripts to analyze scrubbed and anonymized data from the Kundalini Experimental Project. The purpose of Discover is to provide a comprehensive framework for the scientific investigation of Kundalini.

## Getting Started

1. Download and install R and R Studio.
2. Clone this repository using Git.
3. Contact the Emerging Sciences Foundation and request scrubbed, anonymized, and encrypted respondent data files. 
4. After receiving the files, decrypt them in the `/data` directory.
5. Run the analysis files located in the `/R` directory.

## Directory Structure and File Locations

- `/analysis` - Scripts needed to run all the steps of the analysis.
- `/code` - Scripts containing the functions used in the analysis.
- `/data` - Read-only directory for source data, including survey responses.
- `/docker` - Docker configuration and scripts.
- `/documents` - All the documents and other relevant materials. E.g., reports, slides, or other relevant materials.
- `/outputs` - All the analysis output.

## Analysis Scripts

**Primary Scripts**

These scripts produce the primary analysis output for the Discover project. Each script is designed to be run independently and should be self-contained:

- `aohc.R` - **Aspects of Higher Consciousness research paper analysis**. All analysis relevant to the Aspects of Higher Consciousness research paper.
- `kfire.R` - **Kundalini Findings, Insights, and Recommendations Engine (FIRE)**. Initial attempt at classifying a person's spiritual experience using an LCA model.
- `pbs.R` - **Personal Background Survey Analysis Script**. Arbitrary analysis for the Personal Background Survey.
- `ses-fa-lca.R` -  **Spiritual Experience Survey LCA and Factor Analysis Script**. Contains methods to automatically generate factors using Principal Axis Factoring with Promax rotation. The number of factors is determined using an empirical approach called parallel analysis. These factors are then used as inputs to a Latent Class Analysis algorithm which discovers latent classes (clusters) in the data. Finally, the script scores all survey respondents.
- `ses-import.R` - **Spiritual Experience Survey Import Script**. Import functions for the SES survey directly from LimeSurvey
- `ses.R` - **Spiritual Experience Survey Analysis Script**. Arbitrary analysis for the Spiritual Experience Survey.

**Code Scripts**

These scripts contain utility and helper functions for primary scripts. These scripts are not meant to be run by themselves:

- `ses-fa-lca-functions.R` - **Spiritual Experience Survey LCA and Factor Analysis Script Functions**. Functions containing the majority of the logic for the FA and LCA analysis, including the actual FA and LCA algorithms used and starting parameters for analysis.
- `ses-utility.R` - **Spiritual Experience Survey LCA and Factor Analysis Script Helper Functions**. Utility functions for loading data and replace question codes with question text.
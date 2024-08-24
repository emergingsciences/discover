# Kundalini Discover

Kundalini Discover is an R framework for the scientific investigation of Kundalini according to the Kundalini thesis as proposed by Gopi Krishna.

## Getting Started

Scrubbed, anonymized, and encrypted respondent data files are available upon request from the Emerging Sciences Foundation. After you receive the data files, decrypte them and place them in the `/data` directory.

## Directory Structure and File Locations

-   `/analysis` - Main analysis scripts.
-   `/code` - Function definitions.
-   `/data` - Source data, including survey responses.
-   `/docker` - Docker configuration and scripts.
-   `/documents` - Documents and other relevant materials. E.g., reports, slides, or other relevant materials.
-   `/outputs` - All analysis output.

## Analysis Scripts

These scripts produce the primary analysis output for the Discover project. Each script is designed to be run independently and should be self-contained:

-   `aohc.R` - **Aspects of Higher Consciousness research paper analysis**. Analysis for the Aspects of Higher Consciousness research paper.
-   `aohc-selfless.R` - **Selflessness and HC Analysis**. SEM and predictive RO-SEM analysis from the Aspects of Higher Consciousness research paper.
-   `politicaldemocracy.R` - **RO-SEM Political Democracy Empirical Example**. RO-SEM example from the Aspects of Higher Consciousness research paper.
-   `ses-import.R` - **Spiritual Experience Survey Import Script**. Import functions for the SES survey directly from LimeSurvey
-   `pbs.R` - **Personal Background Survey Analysis Script**. Initial analysis for the Personal Background Survey (in progress).

## Code Scripts

These scripts contain utility and helper functions for primary scripts. These scripts are not meant to be run by themselves:

-   `ses-utility.R` - **Spiritual Experience Survey Helper Functions**. Utility functions for loading data and replace question codes with question text for the spiritual experience survey.

## Libraries

Some libraries may require the following options in the \~/.R/Makevars file to compile correctly

```         
CXXFLAGS+=-Wno-ignored-attributes
CXX11FLAGS+=-Wno-ignored-attributes
CXX14FLAGS+=-Wno-ignored-attributes
PKG_CXXFLAGS = -Wno-ignored-attributes
```

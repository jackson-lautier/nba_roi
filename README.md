<h1 align="center"><project-name></h1>

<p align="center"><project-description></p>

## Introduction

This repository is intended as an online supplement to the manuscript,
_A new framework to estimate return on investment for player salaries
in the National Basketball Association_.  The latest version of the
manuscript may be found [here](https://img1.wsimg.com/blobby/go/e126e6bc-09bb-4685-8200-323fe6a91322/downloads/nba_roi_03202024.pdf?ver=1710946816281).
Please attribute any citations of this repository to the latest
version of the manuscript.

Historical versions: [arXiv](https://arxiv.org/abs/2309.05783).


This repository includes:

- **raw_data** The raw 2022-2023 NBA regular season game-by-game data downloaded with
a custom query wrapping around [nba_api](https://github.com/swar/nba_api).  Custom
query wrapping not provided.

- **clean_data** Cleaned data used to generate all results in the reference manuscript. May be replicated using the data processing script.

- **code** Replication code to reproduce all results, figures, and tables in the reference manuscript (use data_analysis.R).  Other scripts represent R functions used throughout.

## Links

- [Repo](https://github.com/jackson-lautier/nba_roi)

- [Latest Manuscript](https://img1.wsimg.com/blobby/go/e126e6bc-09bb-4685-8200-323fe6a91322/downloads/nba_roi_03202024.pdf?ver=1710946816281)

- [arXiv](https://arxiv.org/abs/2309.05783) [HISTORICAL VERSION]

## Screenshots

![Wealth Distributions](/illustrative_figures/wealth_shape.pdf)

![Missed Games Break-Even Analysis](/illustrative_figures/WL_comp.pdf)

## Built With

- R

## Author

**Jackson P. Lautier**

- [Website](https://jacksonlautier.com/)

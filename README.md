# Country-level Social Cost of Carbon

This is the source code of the program to generate 
the country-level social costs of 
carbon. 
It is based on the material provided with the paper 
K. Ricke, L. Drouet, K. Caldeira and M. Tavoni, 
*Country-level Social Cost of Carbon*,
**Nature Climate Change**.
DOI: 10.1038/s41558-018-0282-y.
Other materials are available at https://country-level-scc.github.io.

The main script is `generate_cscc.R`

An example is provided by `country-level-social-cost-of-carbon.Rmd`

## Dependencies
Packages: haven, docopt, countrycode, stringr
Other files: Burke, Hsiang, Miguel 2015 data needs to be ported into the 
"data\BurkeHsiangMiguel2015_Replication\data\input" folder, available 
from https://purl.stanford.edu/wb587wt4560


## Changelog

v1 (26-16-2018)
* Version published with the paper

v2 (18-01-2019)
* Update Rich/Poor threshold: 2268.528$ for BHM, 2449.36$ for djo_richpoor.
* Use 5-lag DJO specification for DJO alternative specification.

v2.5 (07/12/2020)
* Update README to include details of libraries required
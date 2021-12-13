Replication Paper for Gov 2001

Catherine Darin and Zagreb Mukerjee

To generate the tables and figures in our final PDF:
1) Clone or download this repository to a folder. 
2) Create an empty /data directory. 
3) Get a Census API key (https://api.census.gov/data/key_signup.html) 
4) Add the key to 'main.R'
5) Run 'main.R' (after installing appropriate packages). 
    - This will pull data from the Census - it may take a long time. 
    - It will clean and process the data, and store it in the data folder. 
6) Run replicateRegsAndExtend_short v2.R. This will create the estimates used in the paper. 
    - Additionally, this program reproduces the main table from Baccini and Weymouth's analysis (Table 2). See lines 122-128 to produce a replicated table.
7) Run tablesandfigures.R
8) Run counterfactual analysis.R
9) Run Final Draft.Rmd to produce the PDF. 
    - Final Draft.Rmd will make descriptive and regression tables
    - The time series of nationwide manufacturing job changes is based on an Excel file downloaded from the Census QWI Explorer (https://qwiexplorer.ces.census.gov/static/explore.html)
    - The county-level voting data comes from the MIT Election Data and Science Lab https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
    - The map figures are generated in the Rmd.

Original Paper: 
BACCINI, L., & WEYMOUTH, S. (2021). Gone For Good: Deindustrialization, White Voter Backlash, and US Presidential Voting. American Political Science Review, 115(2), 550-567. doi:10.1017/S0003055421000022

Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/7U2UIQ

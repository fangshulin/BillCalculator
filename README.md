# BillCalculator
Easily estimate a water bill across any part of California with OWRS compliant water rate data specified. OWRS bill calculator refactoring tool of same name from MNWD. 

## Objective

Develop a bill calculator leveraging Open Water Rate Specification (OWRS) data. This tool would allow users to put in their address and water use for a given month and see the estimated bill for that month. Extensions include comparing that to other water utilities. 

ARGO team lead: Christopher Tull

## Progress to Date

RateParser was incorporated in the bill calculator tool. Based on user input address, the most recent OWRS information will be retrieved. The tool will calculate commodity charge, total bill, water use and charges in each tier shown in plots and table. Bill calculator is built compatible with different rate structures. A customized analytics product was built for City of Santa Monica.

Check out the demo [here](https://fl1210.shinyapps.io/BillCalculator_RateParser_AddressInput/)

## Resources

General background information on water rate design [here](https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification/blob/master/AWE-Building-a-better-RateStructure.pdf). Especially pages 24+

Information on the Open Water Rate Specification (OWRS) is [here](https://github.com/California-Data-Collaborative/Open-Water-Rate-Specification).

There are lots of R Shiny tutorials [here](https://shiny.rstudio.com/tutorial/).




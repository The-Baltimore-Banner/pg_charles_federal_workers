# Trump’s federal worker cuts are destabilizing the nation’s two wealthiest Black counties

### by [Greg Morton](mailto:greg.morton@thebaltimorebanner.com)

- [Overview](#overview)
- [Data](#data)
- [Methodology](#method)
- [Limitations](#limitations)
- [License](#license)

## Overview

This repository contains the code to recreate the analysis featured in the Baltimore Banner story ["Trump’s federal worker cuts are destabilizing the nation’s two wealthiest Black counties"]()

Find the code to reproduce the findings from the story in the `fact_check` folder. The analysis is done in R and uses the `tidyverse` and `survey` packages to analyze the data. You will need a Census API key for the `tidycensus` package.

<a id="method"></a>

## Methodology

<a id="limitations"></a>

This analysis uses data from the U.S. Census Bureau's American Community Survey (ACS) 5-year estimates for 2019-2023. It also makes use of IPUMS USA data, which provides access to a wide range of census and survey data. The analysis focuses on the federal workforce in the Washington, D.C. metropolitan area, particularly in Prince George's County, Maryland and Charles County, Maryland.

To calculate the share of federal workers in the workforce, both in each county and others around the country, we used aggregated data from the ACS to estimate first the number of total workers (as the census defines them, those over 16 who are not in the military) and then the number of federal workers among that group. The share of federal workers is calculated as the number of federal workers divided by the total number of workers.

To calculate the share of Black federal workers, we used IPUMS ACS microdata, which provides individual-level data on the characteristics of federal workers, in combination with the `survey` package. We filtered the data to include only those who identified as Black or African American and then calculated the share of federal workers who identified as Black.

Since the data is given at an individual level, we were able to use it to understand characteristics of Black federal workers like age, income, and education level. We used that to analyze things like the difference in incomes between Black federal workers and non-federal workers, and the other statistics mentioned in the article.

## Limitations

<a id="license"></a>

This analysis is limited by the availability and accuracy of the data. The ACS data is based on a sample of the population, which means that there may be some sampling error. Additionally, the IPUMS data is also based on a sample, which may not be representative of the entire population of federal workers. Though the organizations that compile this data take great care to ensure that it is representative and accurate, it is not completely comprehensive.

## License

Copyright 2025, The Venetoulis Institute for Local Journalism

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


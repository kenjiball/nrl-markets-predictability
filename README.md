# nrl-markets-predictability

## NRL Betting Markets Predicitability

We attempt to quantify predicitability using betting markets and the log loss functions to test the hypothesis:

**"Was the 2020 NRL season the most predictable season in recent history?"**

## Getting Started

The code to load data, build modeland run metrics is found in the subdirectory `/src`.

The data used in this script is contained in the `/data` directory.

The data is downloaded from: http://www.aussportsbetting.com/data/ 

## Prerequisites

To run the code you will need to install the following packages on your local machine by running the code below.

```
install.packages(implied)
install.packages(janitor)
install.packages(lubridate)
install.packages(poibin)
install.packages(readxl)
install.packages(tidyverse)
install.packages(zoo)
```

## Instructions
All code can be found in the src directory.

The analysis code is in **nrl_markets_predictability.R**

Within this script you will find:

* Data load
* Data preparation
* Summaries
* Plots
* Log Loss Analysis
* Poisson Binomial Analysis

## Authors

* **Kenji Ball** - [Twitter](https://twitter.com/home), [Medium](https://medium.com/@kenji.ball)

## Acknowledgments
* **AUS Sports Betting** - http://www.aussportsbetting.com/data/ 
* **The team at Measurem** - http://measurem.com.au/

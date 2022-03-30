<div id="top"></div>


<h3 align="center"> Canadian Bonds Yield Curve, YTM Curve and Forward Rate Calculator in R </h3>
  <p align="center">
  Mathematical Finance Project completed in 2022
  </p>
</div>


<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#Set-up">Set-up</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>


## About The Project

This project focuses on analyzing the closing prices of 10 Canadian bonds with a maturity of less than 10 years, from Jan.10 - 24, 2022 to generate:
* 1-5 year yield/spot curves,
* 5-year YTM(yield to maturity) curve,
* and 1-year forward curve with terms ranging from 2-5 years,  

and to calculate:
* the covariance matrix for the time series of daily log-returns of yield,
* the covariance matrix for the 1yr-1yr, 1yr-2yr, 1yr-3yr, 1yr-4yr forward rates,
* the eigenvalues and eigenvectors of both covariance matrices.

### Built With
R packages:
* readxl
* janitor
* lubridate
* numDeriv
* tidyverse

### File Guide
* A1 R Code.R: code containing all algorithm and computation;
* Assignment 1 Bond Data: contains closing price of all Canadian bonds with a maturity of less than 10 years, from Jan.10 - 24, 2022;
* LICENSE: MIT License for the project.

## Usage

The code can be referenced for data cleaning, derivative computation, bootstrapping, Newton's method application, and the financial calculations specified above. 
Please make your own judgement of what to incorporate and contributions are welcomed.

<p align="right">(<a href="#top">back to top</a>)</p>

## Set-up
Run the following code to check the libraries are installed: 

```
packages_needed <- c("MASS", "leaps", "pnag", "xtable")
package.check <- lapply(
  packages_needed,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE, 
      repos = "https://cloud.r-project.org/")}})
rm(packages_needed, package.check)
```

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

## Contact

Sherry Xiaoman Lu - sherry.luxiaoman@gmail.com
[![LinkedIn][linkedin-shield]][linkedin-url]

Project Link: [https://github.com/SherryLuXM/APM466A1.git](https://github.com/SherryLuXM/APM466A1.git)

## Acknowledgments

* This is the first assignment from MAT1856/APM466 Mathematical Theory of Finance, taken during the Winter 2021-2022 semester, at the St.George Campus, at University of Toronto, taught by Professor Luis
Seco.

<p align="right">(<a href="#top">back to top</a>)</p>


[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/sherry-l-633854132/

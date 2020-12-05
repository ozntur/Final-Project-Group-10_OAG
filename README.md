
# Welcome to Group 10 Final Project Webpage!

**Package name:** `linreg10`  
**Name of the GitHub repository:** `Final-Project-Group-10_OAG`  
**URL to GitHub:**
<a href="https://github.com/AU-R-Programming/Final-Project-Group-10_OAG" class="uri">https://github.com/AU-R-Programming/Final-Project-Group-10_OAG</a>  
**URL to package website:**
<a href="https://au-r-programming.github.io/Final-Project-Group-10_OAG/" class="uri">https://au-r-programming.github.io/Final-Project-Group-10_OAG/</a>

This is a result of the Final Project of *STAT-6210 R Programming for
Data Science* course. This package contains the basic functions to
perform linear regression. `linreg10` package can calculate the
confidence intervals, estimate linear regression coefficients, compute
mean square prediction error, and calculate p-value. Additionally, the
package can plot:

-   Residuals vs Fitted-values
-   qq-plot of Residuals,
-   Histogram of Residuals and
-   Density of Residuals

Please go to the
[Reference](https://au-r-programming.github.io/Final-Project-Group-10_OAG/reference/index.html)
link on top of this webpage for the explanations of the functions.

`pkgdown::buld_site()` was used to build this website.

## How to install?

In addition to `linreg10` we will load the following packages:

-   `ggplot2` was used to plot and customize the graphs.
-   `cowplot` which is a simple add-on to ggplot, provides various
    features that help with creating publication-quality figures.
-   `devtools` to install the package from github.

<!-- -->

    # Install dependencies
    install.packages("ggplot2", "cowplot")

Next, we can install `linreg10`:

    # Install linreg10
    devtools::install_github("AU-R-Programming/Final-Project-Group-10_OAG")

### Authors:

-   Ozan Turkes
-   Ayomide Afolabi
-   Geeta Kharel

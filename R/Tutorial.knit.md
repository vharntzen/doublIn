---
title: "Tutorial doublIn package"
author: "Vera Arntzen"
date: "2024-04-03"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Tutorial doublIn package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



First, we load the package doublIn.


```r

# Load the package.
library(doublIn)
#> Warning: replacing previous import 'coda::thin' by 'epicontacts::thin' when
#> loading 'doublIn'
#> Warning: replacing previous import 'DT::formatDate' by 'mStats::formatDate'
#> when loading 'doublIn'
#> Warning: replacing previous import 'lubridate::day' by 'mStats::day' when
#> loading 'doublIn'
#> Warning: replacing previous import 'dplyr::recode' by 'mStats::recode' when
#> loading 'doublIn'
#> Warning: replacing previous import 'lubridate::month' by 'mStats::month' when
#> loading 'doublIn'
#> Warning: replacing previous import 'lubridate::is.Date' by 'mStats::is.Date'
#> when loading 'doublIn'
#> Warning: replacing previous import 'lubridate::year' by 'mStats::year' when
#> loading 'doublIn'
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'doublIn'
#> Warning: replacing previous import 'DT::dataTableOutput' by
#> 'shiny::dataTableOutput' when loading 'doublIn'
#> Warning: replacing previous import 'DT::renderDataTable' by
#> 'shiny::renderDataTable' when loading 'doublIn'
#> Warning: replacing previous import 'mStats::label' by 'xtable::label' when
#> loading 'doublIn'
```

In this tutorial we walk through an analysis as is outlined in the corresponding
paper (REF) but using example data. 

### Estimate latency time

Before we start, let's generate a simple 
data set.


```r

# The random starting point can be changed to another value if you like.
set.seed(123)

# For details, see the example in the help file of the function Estimate_doublIn.
  L1 <- sample(1:5, 100, replace = T) 
  L <- runif(100, 0, L1)
  times <- rgamma(100, shape = 4.05, rate = 0.74)
  R <- L + times
  R0 <- floor(R); R1 <- floor(R + 1)
  L1 <- ifelse(L1 > R1, R1, L1)
  Q <- L1 + sample( c(5, 10, 15, 20, 25), 100, replace = T)
  mydat <- data.frame(R = R, L0 = 0, L1 = L1, R0 = R0, R1 = R1,
                      Trunc = Q)
  mydat <- mydat[which( (mydat$R > mydat$Trunc) == F), ]
  mydat$R <- NULL
  rownames(mydat) <- NULL

# Inspect the data.
  knitr::kable(head(mydat, 5))
```



| L0| L1| R0| R1| Trunc|
|--:|--:|--:|--:|-----:|
|  0|  3|  3|  4|    18|
|  0|  3|  6|  7|    23|
|  0|  2|  2|  3|    22|
|  0|  2|  7|  8|    17|
|  0|  3| 13| 14|    23|



The example data is based on the structure of real observations of the latency 
time, but may be used to illustrate the analysis for any doubly interval 
censored time-to-event (L to R). Therefore, in the remainder of this tuturial 
we use the notation L and R instead of E (infection/exposure) and S 
(symptom onset/start-of-shedding).

The data set has five columns that define the exposure window (L0; L1), the 
start-of-shedding window (R0; R1) and the truncation time. The truncation
time does not have to be part of the data set necessarily. It is only required 
when truncation is addressed during the analysis, as we will see later.

Each cell represents the number of days elapsed since the start of exposure, 
which is L0 (0 is this case) but this may be since any calendar time, and in a
measurement unit that the user prefers.

In the analysis, we take the following factors into account, which is important 
to obtain unbiased estimates:
(i) the infection risk within the exposure window (exponential growth);
(ii) the under-representation of long latency times (right truncation);
(iii) the potentially longtailed latency time distribution, requiring a 
flexible model (generalized gamma).
The factors that are relevant depend from context to context.

For now, we assume that the risk of infection remained constant during the
fictive outbreak. We come back to (i) in the second part of this tutorial.


```r

# Run the model with truncation.
out_list  <- Estimate_doublIn(dat = mydat,
             infection_risk_distribution = "constant",
             method = "gamma", percentiles = c(0.5, 0.9, 0.95, 0.99),
             right_truncation = T, iters = 1000,
             burnin_period = 10, thin = 1,
             further_thin_plots = 1,
             plot_rm_burnin = T)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 93
#>    Unobserved stochastic nodes: 188
#>    Total graph size: 1215
#> 
#> Initializing model

# Inspect the output.
summary(out_list)
#>                        Length Class      Mode
#> estimates              10     data.frame list
#> plot_running_quantiles  9     gg         list
#> plot_parameters         9     gg         list
```

The output is a list with three components: a data.frame with the estimates;
a diagnostic plot of the quantiles; a diagnostic plot of the parameters.


```r

# Inspect the estimates.
knitr::kable(out_list$estimates)
```



|                        |       est|   lower_CI|  upper_CI|par                     | iters| thinning| burnin_period|f     |g        |right_truncation |
|:-----------------------|---------:|----------:|---------:|:-----------------------|-----:|--------:|-------------:|:-----|:--------|:----------------|
|theta                   |  1.151373|  0.7388587|  1.794394|theta                   |  1000|        1|            10|gamma |constant |TRUE             |
|kappa                   |  4.470885|  3.0217032|  6.805943|kappa                   |  1000|        1|            10|gamma |constant |TRUE             |
|delta                   |  1.000000|  1.0000000|  1.000000|delta                   |  1000|        1|            10|gamma |constant |TRUE             |
|mean                    |  5.154868|  4.6062243|  5.854882|mean                    |  1000|        1|            10|gamma |constant |TRUE             |
|variance                |  5.926442|  3.6720793|  9.960792|variance                |  1000|        1|            10|gamma |constant |TRUE             |
|0.5                     |  4.772929|  4.2384939|  5.373800|0.5                     |  1000|        1|            10|gamma |constant |TRUE             |
|0.9                     |  8.402811|  7.3685454|  9.897148|0.9                     |  1000|        1|            10|gamma |constant |TRUE             |
|0.95                    |  9.699733|  8.3908732| 11.625889|0.95                    |  1000|        1|            10|gamma |constant |TRUE             |
|0.99                    | 12.429447| 10.4820913| 15.411251|0.99                    |  1000|        1|            10|gamma |constant |TRUE             |
|Gelman diag.: lambda_gg |  1.066234|         NA|  1.138211|Gelman diag.: lambda_gg |  1000|        1|            10|gamma |constant |TRUE             |
|Gelman diag.: r         |  1.068361|         NA|  1.144237|Gelman diag.: r         |  1000|        1|            10|gamma |constant |TRUE             |



The estimated mean is 5.14 with 95% credible interval (CrI) of (4.63; 5.81).

### Visualize the results

Let's plot the estimated distribution.


```r

# Make a function of the plot in the paper.
Plot_doublIn(doublIn_obj = out_list, 
             label_x = "Latency time (days)", 
             label_y = "Probability")
```

![](Tutorial_files/figure-html/Plot the distribution-1.png)<!-- -->

In the above plot, we see that the fitted distribution is right skewed. The 
vertical line pieces correspond to the median, the 90th and 95th percentile,
respectively.


```r

# Plot the running quantiles.
out_list$plot_running_quantiles
```

![](Tutorial_files/figure-html/Diagnostic plot - running quantiles-1.png)<!-- -->

Each color represents one of the three Markov Monte Carlo chains. Essentially,
the model starts at three random points. The model converged properly if the 
quantiles (2.5%, 50%, 97.5%) of the parameters using all iterations up to the 
respective iteration (omitting the burnin period) overlap almost entirely on the
right side of the plot. So, if all is well, we observe three bundles with each
three lines of a different color.


```r

# Plot the parameter values per iteration.
out_list$plot_parameters
```

![](Tutorial_files/figure-html/Diagnostic plot - parameter values-1.png)<!-- -->

Instead of a cumulative measure, the second plot represents the parameter values
at each iteration. When mixing is properly, there is no pattern visible; just a
blur. When, for example, the first 200 iterations the values are clearly different
from the remaining iterations, the burnin period needs to be longer.

### Estimate the exponential growth factor

To address (i), we need the exponential growth factor for our specific sampling
period and place. To this end, in the analysis for our paper on SARS-CoV-2 latency time we
utilized time series of the incidence of individuals with a first positive test 
from an extensive line list. However, for illustration purposes, we use openly 
available data from the R package 'outbreaks', concerning the daily incidence of 
influenza cases in a boarding school in England. We use the R package 'incidence'
to obtain the estimate of the exponential growth factor r.


```r

# Load the package.
require(incidence)
#> Loading required package: incidence
require(outbreaks)
#> Loading required package: outbreaks

data("influenza_england_1978_school")

# Format as an incidence object.
inc_obj <- as.incidence(influenza_england_1978_school$in_bed, 
                      influenza_england_1978_school$date)
plot(inc_obj)
#> Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
#> ggplot2 3.3.4.
#> ℹ Please use "none" instead.
#> ℹ The deprecated feature was likely used in the incidence package.
#>   Please report the issue at <https://github.com/reconhub/incidence/issues>.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

![](Tutorial_files/figure-html/Estimation of the growth factor-1.png)<!-- -->

```r

# Find the peak.
peak <- find_peak(inc_obj)

# Select only the incidence up to the peak for estimation.
inc_obj <- inc_obj[inc_obj$dates <= peak,]

# Estimate the growth rate.
# We will only use the increasing phase for estimation, hence the term '-1'
inc_fit <- incidence::fit(inc_obj)
r <- inc_fit$info$r
plot(inc_fit)
```

![](Tutorial_files/figure-html/Estimation of the growth factor-2.png)<!-- -->

```r

# Inspect r .
r
#> [1] 0.973572
```

Notice that influenza outbreaks tend to grow much faster than those of SARS-CoV-2
due to the short incubation and latency time.

### Explore the possibilities of the visualization app


```r

# Run the app
Visualize_contact_tracing_data()
#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.

#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.

#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.

#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.

#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.

#> Warning: Navigation containers expect a collection of
#> `bslib::nav_panel()`/`shiny::tabPanel()`s and/or
#> `bslib::nav_menu()`/`shiny::navbarMenu()`s. Consider using `header` or `footer`
#> if you wish to place content above (or below) every panel's contents.
#> PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```


```{=html}
<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
```



### References

Lessler J, Reich NG, Cummings DAT and The DOHMH Swine Influenza Investigation Team. Outbreak of 2009 Pandemic Influenza A (H1N1) at a New York City School. New England Journal of Medicine. 2009. 361(27):2628-2636.

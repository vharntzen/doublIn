% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_Estimate_doublIn.R
\name{Estimate_doublIn}
\alias{Estimate_doublIn}
\title{Estimate the incubation or latency time of an infectious disease,
i.e. a doubly interval censored time-to-event}
\usage{
Estimate_doublIn(
  dat,
  infection_risk_distribution = "constant",
  exp_growth_rate = NULL,
  exp_growth_rate_SE = NULL,
  method = "GenGamma",
  percentiles = c(0.5, 0.9, 0.95, 0.99),
  right_truncation = FALSE,
  iters = 5000,
  burnin_period = 250,
  thin = 1,
  further_thin_plots = 10,
  plot_rm_burnin = TRUE
)
}
\arguments{
\item{dat}{data.frame with one row per individual and the variables L0, L1, R0,
R1 representing the left and right window containing the time origin and
endpoint, respectively. When right truncation needs to be addressed, an
additional variable Trunc is required.}

\item{infection_risk_distribution}{either exponential growth ("exp_growth")
or a constant risk of infection ("constant") is assumed within the exposure
window.}

\item{exp_growth_rate}{when exponential growth is assumed, the estimated
growth factor r.}

\item{exp_growth_rate_SE}{the Standard Error of the estimated growth factor.}

\item{method}{assumed distribution for the time-to-event; can be "gamma",
"GenGamma" (generalized gamma) or "Weibull".}

\item{percentiles}{the percentiles of interest as a vector with probabilities.}

\item{right_truncation}{whether right truncation occurred in the data (T) or not
(F); an additional variable 'Trunc' in the data represents the calendar
truncation time.}

\item{iters}{the number of iterations for the MCMC chain.}

\item{burnin_period}{burnin_period, i.e. the number of initial iterationals to be
removed before analyzing the chains.}

\item{thin}{a thinning factor, meaning that every so many iterations is saved.}

\item{further_thin_plots}{additional thinning factor for plots (default is 10).}

\item{plot_rm_burnin}{omits the burnin period from the diagnostic plots,
as these iterations are removed from the actual analysis (default is T).}
}
\value{
A list: the estimates including Gelman diagnostic criterion;
the settings that were used to run the model; a diagnostic plot with the
running quantiles per parameter; a diagnostic plot with the running parameter
estimates.
}
\description{
Estimate the distribution of doubly interval censored observations of
time-to-event allowing for (i) constant risk of initial event within the
window containing the time origin or a risk according to exponential growth
(as for infection risk in the beginning of an outbreak); (ii) different
shapes of the distribution (gamma, generalized gamma,Weibull); (iii) right
truncation; (iv) (partial) overlap of the two windows. Provides estimates of
the mean, median, 95th percentile and parameters, as well as diagnostics.
}
\details{
The function estimates in the Bayesian framework, running JAGS via R and
employing three parallel Markov Chain Monte Carlo chains per model. We
extended the code by Charniga et al. (2022). The code for the diagnostic
plots is written by Ronald Geskus.
}
\examples{

 # NB: the example takes a short while to run.

 # Draw an exposure window width 1, 2, 3, 4, 5
 L1 <- sample(1:5, 100, replace = TRUE)

 # Draw the infection moment from a uniform distribution on (L0, L1)
 L <- runif(100, 0, L1)

 # Draw latency times (as estimated by Xin et al., 2022)
 times <- rgamma(100, shape = 4.05, rate = 0.74)
 R <- L + times

 # Draw end of quarantine (last test moment)
 Q <- L1 + sample( c(5, 10, 15, 20, 25), 100, replace = TRUE)

 # Define the data set
 mydat <- data.frame(R = R, L0 = 0, L1 = L1,
                     R0 = floor(R), R1 = floor(R + 1), Trunc = Q)

 # Apply the truncation
 mydat <- mydat[which( (mydat$R > mydat$Trunc) == FALSE), ]
 mydat$R <- NULL

 # If exposure ends after the last possible moment of the endpoint, end
 # exposure earlier
 mydat$L1 <- ifelse(mydat$L1 > mydat$R1, mydat$R1, mydat$L1)

 # Run the model with truncation
  \donttest{Estimate_doublIn(dat = mydat,
 infection_risk_distribution = "constant",
 method = "gamma", percentiles = c(0.5, 0.9, 0.95, 0.99),
 right_truncation = TRUE, iters = 1000,
 burnin_period = 10, thin = 1,
 further_thin_plots = 1)}


}
\references{
Stacy, E. W., and G. A. Mihram, Parameter estimation for a
generalized gamma distribution, Technometrics, 7 (3), 349–358,
doi:10.1080/00401706.1965.10490268, 1965

Charniga, K., et al., Estimating the incubation period of
monkeypox virus during the 2022 multi-national outbreak, medRxiv,
doi:10.1101/2022.06.22.22276713, 2022

LeBauer et al., Translating Probability Density
Functions: From R to BUGS and Back Again, The R Journal, 2013

Plummer, M., JAGS user manual, 2017
\url{https://people.stat.sc.edu/hansont/stat740/jags_user_manual.pdf}

Rubio, J.F, The Generalised Gamma Distribution, 2020
\url{https://rpubs.com/FJRubio/GG}
}
\author{
Vera Arntzen, \email{v.h.arntzen@math.leidenuniv.nl}
}
\keyword{survival}

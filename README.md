<!-- badges: start -->

[![R-CMD-check](https://github.com/MansMeg/ada_pop/workflows/R-CMD-check/badge.svg)](https://github.com/MansMeg/ada_code/actions)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

A Poll of Polls model
=====================

This repository contain code to run the Ada Poll of Polls model using
Stan.

To run the models and (re)produce the output:

1.  You need to have Stan and Rstan installed. To install Rstan (and
    Stan), see [Rstan Installation
    information](https://mc-stan.org/users/interfaces/rstan.html).
2.  Install the SwedishPolls R package. See the install instructions
    [here](https://github.com/MansMeg/SwedishPolls).
3.  Install the `ada` R package (see below).
4.  Run either
    [run\_ada/run\_ada.R](https://github.com/MansMeg/ada_code/blob/main/run_ada/run_ada.R)
    in R or
    [run\_ada/run\_ad\_bash.sh](https://github.com/MansMeg/ada_code/blob/main/run_ada/run_ada_bash.sh).
5.  Play around with the resulting model object in R.

The model used
==============

We continuously develop the model and improves it. The actual model we
use is set in the
[run\_ada/ada\_cfg.yml](https://github.com/MansMeg/ada_code/blob/main/run_ada/ada_cfg.yml)
(`model` argument). The same model will exist as a stan file in the R
package, that you can find in
[rpackage/inst/stan\_models/](https://github.com/MansMeg/ada_code/blob/main/rpackage/inst/stan_models/).

The hyperparameter settings we use are then either set in the config
file
([run\_ada/ada\_cfg.yml](https://github.com/MansMeg/ada_code/blob/main/run_ada/ada_cfg.yml))
or as the default values. The default values are printed when running
the model in R.

Unfortunately we do not have a better description of the model right
now. We know that it can be cumbersome to read, but if you have any
questions feel free to reach out on twitter or leave an issue
[here](https://github.com/MansMeg/ada_code/issues) at github.

R package
=========

All functionality and tests of implemented functionality is implemented
in the R package `ada`.

Installation
------------

To install, just build the local package (if the repo is cloned):

``` r
devtools::install_local("rpackage")
```

### Access polling data

There are two types of data we can access through the `ada` package.
First, we can access real polling data from Sweden (and Spain, and
Germany).

To access the polls data from the R package, use:

``` r
library(ada)
data("swedish_polls_curated")
data("swedish_elections")
```

Based on the data we create a polls\_data object.

``` r
pd <- polls_data(y = swedish_polls_curated[, 3:10],
                 house = swedish_polls_curated$Company,
                 publish_date = swedish_polls_curated$PublDate,
                 start_date = swedish_polls_curated$collectPeriodFrom,
                 end_date = swedish_polls_curated$collectPeriodTo,
                 n = swedish_polls_curated$n)
pd
```

    ## A polls_data object with 1660 polls from 28 houses 
    ## that range from 2000-01-03 to 2021-06-29.
    ##  # A tibble: 1,660 × 14
    ##    .poll_id     M     L     C    KD     S     V    MP    SD .house   .publish_date
    ##    <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <fct>    <date>       
    ##  1 1        0.221 0.034 0.106 0.068 0.234 0.113 0.033 0.182 Demoskop 2021-06-29   
    ##  2 2        0.218 0.025 0.079 0.05  0.255 0.112 0.035 0.214 Novus    2021-06-29   
    ##  3 3        0.212 0.028 0.079 0.051 0.252 0.102 0.033 0.194 Sentio   2021-06-29   
    ##  4 4        0.22  0.02  0.1   0.06  0.24  0.12  0.03  0.19  Ipsos    2021-06-22   
    ##  5 5        0.224 0.023 0.099 0.051 0.258 0.096 0.035 0.202 Sifo     2021-06-18   
    ##  6 6        0.226 0.023 0.094 0.053 0.261 0.096 0.042 0.192 Novus    2021-06-11   
    ##  7 7        0.229 0.036 0.107 0.056 0.242 0.085 0.035 0.192 Demoskop 2021-06-05   
    ##  8 8        0.224 0.025 0.095 0.045 0.282 0.089 0.038 0.189 SCB      2021-06-02   
    ##  9 9        0.193 0.033 0.07  0.052 0.255 0.103 0.05  0.21  Sentio   2021-05-31   
    ## 10 10       0.22  0.03  0.09  0.05  0.26  0.1   0.04  0.2   Ipsos    2021-05-29   
    ## # … with 1,650 more rows, and 3 more variables: .start_date <date>,
    ## #   .end_date <date>, .n <int>

To see all available datasets, use:

``` r
data(package = "ada")
```

The real polling data comes both as an original dataset, and a curated
dataset that has been curated to be internally consistent. See
`rpackage/data-raw` for details exactly on how the curation has been
conducted. Every file without suffix `_functions` will create the
datasets stored in the R-package.

Documentation on each dataset can be found in the R package docs or in
`rpackage/R/docs_data.R`.

To simplify modeling we can also simulate polls data. This can be done
using `simulate_polls()` as follows.

``` r
library(ada)
data(x_test)
set.seed(4711)
spd <- simulate_polls(x = x_test[[1]],
                      pd = pd_test,
                      npolls = 150,
                      time_scale = "week",
                      start_date = "2010-01-01")
```

We can also plot the simulated polls with:

``` r
plot(spd, y = "x")
```

### Accessing true state (election) data

In the next step we add information on where we know the true underlying
latent state. In the case of real data this is election data (see
above). Below we create a dataset with week 3, 4, and 72 being known.

``` r
true_idx <- c(3, 44, 72)
known_state <- tibble::tibble(date = as.Date("2010-01-01") + lubridate::weeks(true_idx), x = x_test[[1]][true_idx])
known_state
```

    ## # A tibble: 3 × 2
    ##   date           x
    ##   <date>     <dbl>
    ## 1 2010-01-22  0.25
    ## 2 2010-11-05  0.25
    ## 3 2011-05-20  0.3

### Running a poll of polls model

To fit the model we only need use the `poll_of_polls()` function.

``` r
output <- capture.output(suppressWarnings(
  pop <- poll_of_polls(y = "x",
                       model = "model8h3",
                       polls_data = spd,
                       time_scale = "week",
                       known_state = known_state,
                       warmup = 1000, 
                       iter = 2000, 
                       chains = 4)
))
```

    ## Default value(s) set:

    ## sigma_kappa_hyper = 0.005

    ## kappa_1_sigma_hyper = 0.02

    ## g_scale = 0.46986301369863

    ## use_industry_bias = 0

    ## use_house_bias = 0

    ## use_design_effects = 0

    ## use_constrained_party_house_bias = 0

    ## use_constrained_house_house_bias = 0

    ## use_constrained_party_kappa = 0

    ## use_ar_kappa = 0

    ## use_latent_state_version = 0

    ## use_t_dist_industry_bias = 0

    ## use_multivariate_version = 0

    ## use_softmax = 1

    ## estimate_alpha_kappa = 0

    ## estimate_alpha_beta_mu = 0

    ## estimate_alpha_beta_sigma = 0

    ## alpha_kappa_known = 1

    ## alpha_beta_mu_known = 1

    ## alpha_beta_sigma_known = 1

    ## beta_mu_1_sigma_hyper = 0.02

    ## sigma_beta_mu_sigma_hyper = 0.01

    ## beta_sigma_1_sigma_hyper = 1

    ## sigma_beta_sigma_sigma_hyper = 1

    ## kappa_sum_sigma_hyper = 0.01

    ## beta_mu_sum_party_sigma_hyper = 0.01

    ## beta_mu_sum_house_sigma_hyper = 0.01

    ## estimate_kappa_next = 1

    ## nu_kappa_raw_alpha = 6.5

    ## nu_kappa_raw_beta = 1

    ## alpha_kappa_mean = 0

    ## alpha_kappa_sd = 1

    ## alpha_beta_mu_mean = 0

    ## alpha_beta_mu_sd = 1

    ## alpha_beta_sigma_mean = 0

    ## alpha_beta_sigma_sd = 1

    ## nu_lkj = 1

    ## x1_prior_p = 0.268781302170284, 0.731218697829716

    ## x1_prior_alpha0 = 100

    ## Registered S3 methods overwritten by 'adapop':
    ##   method                   from
    ##   [.latent_state           ada 
    ##   [.polls_data             ada 
    ##   as.data.frame.polls_data ada 
    ##   length.polls_data        ada 
    ##   pairs.poll_of_polls      ada 
    ##   plot.poll_of_polls       ada 
    ##   plot.polls_data          ada 
    ##   print.poll_of_polls      ada 
    ##   print.polls_data         ada 
    ##   subset.polls_data        ada

We can also extract some basic information and the results.

``` r
pop
```

    ## ==== Poll of Polls Model (33.8 MB) ==== 
    ## Model is fit during the period 2010-01-01--2011-11-27
    ## Stan model: model8h3.stan
    ## Number of parameters: 506 
    ## Parties: x 
    ## Time scale: week 
    ## 
    ## == Data == 
    ## A polls_data object with 150 polls from 1 houses 
    ## that range from 2010-01-01 to 2011-11-27.
    ##  
    ## A known state object with 3 known states
    ## that range from 2010-01-22 to 2011-05-20.
    ##  
    ## == Stan arguments == 
    ## warmup: 1000.0
    ## iter: 2000.0
    ## chains: 4.0
    ## 
    ## == Model arguments == 
    ## ~
    ## 
    ## == Model diagnostics == 
    ## no_divergent_transistions: 0
    ## no_max_treedepth: 0
    ## no_low_bfmi_chains: 0
    ## no_Rhat_above_1_1: 0
    ## no_Rhat_is_NA: 14
    ## mean_no_leapfrog_steps: 108
    ## mean_chain_step_size: 0.0484539
    ## mean_chain_inv_mass_matrix_min: 0.0416008
    ## mean_chain_inv_mass_matrix_max: 1.0767375
    ## mean_chain_warmup_time: 19
    ## mean_chain_sampling_time: 22
    ## 
    ## == Git == 
    ## git sha: 05b80248038a271ffb72253a71e8bdcabfff7614 
    ## 
    ## == Cache == 
    ## sha: 0bc8c0f14ea23e05ef4e65e8fdd28fa462712280 
    ## cache directory: /var/folders/8x/bgssdq5n6dx1_ydrhq1zgrym0000gn/T//RtmpV15V6A/pop_cache

The stan object can be found in `pop5$stan_fit`

``` r
head(rstan::summary(pop$stan_fit)$summary)[,1:3]
```

    ##                  mean      se_mean          sd
    ## x_pred[1,1] 0.2548916 1.683106e-04 0.010130557
    ## x_pred[1,2] 0.7451084 1.683106e-04 0.010130557
    ## x_pred[2,1] 0.2546602 1.219312e-04 0.008128779
    ## x_pred[2,2] 0.7453398 1.219312e-04 0.008128779
    ## x_pred[3,1] 0.2533631 8.510874e-05 0.005537453
    ## x_pred[3,2] 0.7466369 8.510874e-05 0.005537453

We can also quickly visualize the latent series with:

``` r
plot(pop, "x")
```

Updating/adding new Stan models
-------------------------------

All Stan Code can be found in `rpackage/inst/stan_models`. The purpose
is that the stan files should be a part of the rpackage for testing.

Although local stan models can be tested direct by:

    pop <- poll_of_polls(...,
                        model = "path/to/my/stan/model.stan",
                        ...)

In this way a model can be edited quickly without needing to rebuild the
R package.

Note that the filename need to have the same name as the available
models to identify how data should be parsed.

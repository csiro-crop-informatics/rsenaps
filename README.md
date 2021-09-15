<!-- badges: start -->
[![Travis build status](https://travis-ci.org/csiro-crop-informatics/rsenaps.svg?branch=master)](https://travis-ci.org/csiro-crop-informatics/rsenaps)
[![CircleCI build status](https://circleci.com/gh/csiro-crop-informatics/rsenaps.svg?style=svg)](https://circleci.com/gh/csiro-crop-informatics/rsenaps)
[![DOI](https://zenodo.org/badge/278291231.svg)](https://zenodo.org/badge/latestdoi/278291231)
<!-- badges: end -->

# RSenaps


`RSenaps` is an R package to manage communication with the [Senaps API](http://sensor-cloud.io). 

## Getting started


### Installation

Currently on [Github](https://github.com/csiro-crop-informatics/rsenaps) only. Install with:

```r
remotes::install_github('csiro-crop-informatics/rsenaps')
```

### Authentication

Use of the Senaps API requires a user account. You can create one via [Senaps](http://sensor-cloud.io). 

The API accepts authentication with either an API key, or username/password. It it *STRONGLY RECOMMENDED* that you use an API key for authentication, *NOT* a username and password. 

Regardless of which you use, never store credentials in a script, or commit them to a version control repository.

The best way to manage your credentials is with an environment variable, through a `.Renviron` file. Create the `.Renviron` file in root of your directory with contents of the structure:

`SENAPS_APIKEY=putyourkeyhere`

(replace `putyourkeyhere` with your actual key from Senaps).

The key is then accessible in R with `Sys.getenv('SENAPS_APIKEY')`.


To set your API key with RSenaps, use the `senaps_options()` function:

```r
senaps_options(apikey = Sys.getenv('SENAPS_APIKEY'))
```

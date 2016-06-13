# githubtools

The goal of githubtools is to ease the integration between GitHub and R. 

## Installation

You can install githubtools from github with:

```R
# install.packages("devtools")
devtools::install_github("jonocarroll/githubtools")
```

## Features

### Create a link back to the GitHub package right in the RStudio help viewer.

An Rmd macro inserted at build introduces a discreet pull-up tab at the bottom of your help files

```R
#' An R function with nifty HTML documentation
#'
#' Some stuff about the function
#'
#' \feedbackfooter{'jonocarroll/htmlhelp'}{'R/hello.R'}{TRUE}
#'
#' @return Prints a value
#' @export
hello <- function() {
  print("Hello, world!")
}
```

If this macro is added with this package loaded, then the feature set will update when this package updates.

![](https://camo.githubusercontent.com/8e6aab5c14977a3a5859912e767be74a2cdbc558/687474703a2f2f692e696d6775722e636f6d2f6456703561376a2e676966)

Full credit to @noamross for the [noamross/htmlhelp](http://github.com/noamross/htmlhelp) package 
which inspired and enabled the use of the Rmd macro for help files.

### Scan installed GitHub packages and analyse them

Think about your R library right now... How old are the packages you installed from GitHub? Have the developers 
been improving those packages in the meantime? Did you install in the middle of a feature integration? Now you can 
find out. 

```R
setwd("tmp_directory")
scan_gh_pkgs()
```

Produces a HTML file displaying an array of GitHub-styled tile graphs, one for each packge you have installed from 
GitHub (presumably via `devtools::install_github()`).

![](https://github.com/jonocarroll/githubtools/blob/master/man/figures/scan.png?raw=true)



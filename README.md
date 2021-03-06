# githubtools

The goal of githubtools is to ease the integration between GitHub and R. 

## Installation

You can install githubtools from github with:

```R
# install.packages("devtools")
devtools::install_github("jonocarroll/githubtools")
```

## Features

### Create links back to an installed GitHub package right in the RStudio help viewer.

![](http://i.imgur.com/CemtYVA.gif)

Full credit to @noamross for the
[noamross/htmlhelp](http://github.com/noamross/htmlhelp) package which inspired
and enabled the use of the hijacking of `Rd2HTML`. Full details to follow, but this
currently works on any package installed from GitHub (other than those that this package is 
dependent on itself).

### Scan installed GitHub packages and analyse them

Think about your R library right now... How old are the packages you installed
from GitHub? Have the developers been improving those packages in the meantime?
Did you install in the middle of a feature integration? Now you can find out.

```R
setwd("tmp_directory")
scan_gh_pkgs()
```

Produces a HTML file displaying an array of GitHub-styled tile graphs, one for 
each packge you have installed from GitHub (presumably via 
`devtools::install_github()`). Commits for the last 12 months for each 
GiHub-loaded package are obtained. Darker green tiles indicate more commits were
performed on that day, lighter green indicates less commits that day. Light grey
indicates no commits that day, and dark grey indicates the boundaries of the
available commits (expanded to the last 12 months). The red tile indicates when
you installed the packge. It should now be clear whether or not you installed
before, during, or after a flurry of commits, or if the developer has been
active/quiet regarding that packge in the last 12 months.

![](https://github.com/jonocarroll/githubtools/blob/master/man/figures/scan.png?raw=true)

This works best if you are authenticated to GitHub. Steps to achieve that to
follow. Note also that this won't search any packages you're currently forking,
as they will be local installs. To view which packages you have installed in all
your libraries (note, this may be a lot) use the convenience wrapper to
`devtools::session_info` which finds *all* installed packages, not just those
currently loaded.

*NOTE* this will scan the **master** branch of the requested repo. Development that occurs 
on a forked branch will not be captured.

```R
view_all_sources()
```

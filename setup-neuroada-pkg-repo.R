# Let's create an Rproject
# File > New Project > "Associate Project with Existing Directory"

# Let's load the required packages
# install.packages("devtools")
devtools::install_github("hadley/pkgdown")
library(tidyverse)
library(magrittr)
library(here)
library(roxygen2)
library(testthat)
library(devtools)

# Install usethis and load it
# install.packages("usethis")
library(usethis)

# Let's examine usethis
ls("package:usethis") %>% base::length()
# There are 64 functions. Quite a few. Let's examine them sorted by name
# to get a feel for which ones will be useful

ls("package:usethis") %>% base::sort()
# Nice! They seem to have a consistent naming convention {verb}_{description}
# e.g. use_readme_md or create_package
# usethis_fns[usethis_fns %>% str_detect(string = ., pattern = "proj")]

# Let's just examine the frequency of the verb prefixes
usethis_fns_freq <- ls("package:usethis") %>%
                        stringr::str_split(string = ., pattern = "_") %>%
                        purrr::map(~.x[[1]]) %>%
                        base::unlist() %>%
                        base::table() %>%
                        base::sort(decreasing = TRUE)
usethis_fns_freq

# So the main {verb} is "use" and then "edit", "browse" etc

# Great so we are dealing with a library that has an consistent and easy
# to understand naming convention for it's functions. Let's start using them
# to create a package

# Create a new package in the neuroada folder
usethis::create_package(path = here::here())
#> Changing active project to neuroada
#> ✔ Creating 'R/'
#> ✔ Creating 'man/'
#> ✔ Writing 'DESCRIPTION'
#> ✔ Writing 'NAMESPACE'
#> Error: Could not find template 'template.Rproj'

# Set up other files -------------------------------------------------
use_readme_md()
#> ✔ Writing 'README.md'
#> ● Edit 'README.md'

# In RStudio, this opens up the README.md file which is already
# filled in with the devtools installation guide for `neuroada` - nice!

# Use MIT License - modify the description
usethis::use_mit_license("Shamindra Shrotriya")
#> ✔ Setting License field in DESCRIPTION to 'MIT + file LICENSE'
#> ✔ Writing 'LICENSE.md'
#> ✔ Adding '^LICENSE\\.md$' to '.Rbuildignore'
#> ✔ Writing 'LICENSE'

# Load external packages to be used in neuroada
usethis::use_package("tidyverse")
# ✔ Adding 'tidyverse' to Imports field in DESCRIPTION
# ● Refer to functions with `tidyverse::fun()`
usethis::use_package("magrittr")
# ✔ Adding 'magrittr' to Imports field in DESCRIPTION
# ● Refer to functions with `magrittr::fun()`

# Use Roxygen
usethis::use_roxygen_md()
# ✔ Setting Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
# ✔ Setting RoxygenNote field in DESCRIPTION to '6.0.1'
# ● Re-document

# Load the magrittr pipe locally
usethis::use_pipe()
# ✔ Setting Roxygen field in DESCRIPTION to 'list(markdown = TRUE)'
# ✔ Setting RoxygenNote field in DESCRIPTION to '6.0.1'
# ● Re-document

#  imports a standard set of helpers to facilitate programming with
# the tidy eval toolkit.
usethis::use_tidy_eval()
# ✔ Adding 'rlang' to Imports field in DESCRIPTION
# ✔ Writing 'R/utils-tidy-eval.R'
# ● Run document()

# We want to write unit tests effectively and the preferred way is using
# the `testthat` package
usethis::use_testthat()
# ✔ Adding 'testthat' to Suggests field in DESCRIPTION
# ✔ Creating 'tests/testthat/'
# ✔ Writing 'tests/testthat.R'

usethis::use_news_md()
#> ✔ Writing 'NEWS.md'
#> ● Edit 'NEWS.md'

# Let's update to use travis
# It will interactively take you to the travis page for the `neroada` repo
# so that you can activate it - very cool.
# I've temporarily deactivated it, but the travis yml files are created
# which is great!
usethis::use_travis()
# ✔ Writing '.travis.yml'
# ✔ Adding '^\\.travis\\.yml$' to '.Rbuildignore'
# ● Add a Travis build status badge by adding the following line to your README:
#     Copying code to clipboard:
#     [![Travis build status](https://travis-ci.org/shamindras/neuroada.svg?branch=master)](https://travis-ci.org/shamindras/neuroada)
# ● Turn on travis for your repo at https://travis-ci.org/shamindras/neuroada

# We may want to export this to a website later. So just setup pkgdown now
usethis::use_pkgdown()
# ● Modify '_pkgdown.yml'
# ✔ Adding '^_pkgdown\\.yml$' to '.Rbuildignore'
# ✔ Creating 'doc/'
# ✔ Adding '^doc$' to '.Rbuildignore'

pkgdown::build_site()
# Initialising site ----------------------------------------------------------------
#     Creating 'docs/'
# ✔ Adding '^docs$' to '.Rbuildignore'
# Copying '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/packrat/lib/x86_64-apple-darwin15.6.0/3.4.2/pkgdown/assets/jquery.sticky-kit.min.js'
# Copying '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/packrat/lib/x86_64-apple-darwin15.6.0/3.4.2/pkgdown/assets/link.svg'
# Copying '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/packrat/lib/x86_64-apple-darwin15.6.0/3.4.2/pkgdown/assets/pkgdown.css'
# Copying '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/packrat/lib/x86_64-apple-darwin15.6.0/3.4.2/pkgdown/assets/pkgdown.js'
# Building home --------------------------------------------------------------------
#     Writing '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/docs/LICENSE.html'
# Writing '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/docs/authors.html'
# Writing '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/docs/index.html'
# Building function reference ------------------------------------------------------
#     Creating 'docs/reference/'
# Loading neuroada
# Writing '/Users/shamindras/PERSONAL/LEARNING/REPOS/neuroada/docs/reference/index.html'
# Building news --------------------------------------------------------------------
#     Creating 'docs/news/'
# Writing 'docs/news/index.html'

# Local page will load

# Other things to add later

## Vignettes

# Let's setup vignette's - we are going to use them soon enough
# usethis::use_vignette(name = )
# ✔ Adding 'knitr' to Suggests field in DESCRIPTION
# ✔ Setting VignetteBuilder field in DESCRIPTION to 'knitr'
# ✔ Adding 'rmarkdown' to Suggests field in DESCRIPTION
# ✔ Creating 'vignettes/'
# ✔ Adding '*.html', '*.R' to 'vignettes/.gitignore'
# ✔ Adding 'inst/doc' to './.gitignore'
# Error in stopifnot(is.character(x)) :
# argument "name" is missing, with no default

# R Code
# usethis::use_r("read-utils")

# Test
# usethis::use_test("read-utils-test")


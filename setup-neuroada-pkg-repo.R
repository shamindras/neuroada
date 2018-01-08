# Let's create an Rproject
# File > New Project > "Associate Project with Existing Directory"

# Let's load the required packages
install.packages("testthat")
library(tidyverse)
library(magrittr)
library(here)
library(roxygen2)
library(testthat)

# Install usethis and load it
install.packages("usethis")
library(usethis)

# Let's examine usethis
ls("package:usethis") %>% base::length()
# There are 64 functions. Quite a few. Let's examine them sorted by name
# to get a feel for which ones will be useful

ls("package:usethis") %>%
    base::sort()
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

# We may want to export this to a website later. So just setup pkgdown now
usethis::use_pkgdown()
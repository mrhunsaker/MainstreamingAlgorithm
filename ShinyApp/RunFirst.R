# This will allow you to have a working R Environment for using the INCLUDE Algorithm

packages <- c("shiny", "shinyjs", "gplots", "RColorBrewer", "rpart", "rpart.plot", "e1071", "gridExtra", "shinythemes")
install_if_missing <- function(packages) {
  # Check if each package is installed, install if not
  for (pkg in packages) {
    if (!(pkg %in% installed.packages())) {
      install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org")
    }
  }
}

install_if_missing(packages)

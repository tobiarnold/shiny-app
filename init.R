my_packages = c("shiny", "shinydashboard", "shinyWidgets","shinycssloaders","ggplot2","plotly","leaflet","ranger")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
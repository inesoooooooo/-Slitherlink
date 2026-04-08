
<!-- README.md is generated from README.Rmd. Please edit that file -->

# projetR

<!-- badges: start -->

<!-- badges: end -->

Le projetR génère des grilles de Slitherlink et permet d’y jouer via une
application Shiny.

## Installation

You can install the development version of projetR like so:

``` r
remotes::install_github("inesoooooooo/-Slitherlink")
```

## Example

``` r
library(projetR)
shiny::shinyApp(ui = projetR:::ui, server = projetR:::server)
```

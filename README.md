
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jasmines

<!-- badges: start -->

<!-- badges: end -->

``` r
remotes::install_github("djnavarro/jasmines")
```

## Example 1

``` r
library(dplyr)
library(jasmines)

use_seed(1) %>%
  entity_circle(grain = 1000) %>%
  unfold_tempest(iterations = 10) %>%
  style_ribbon(background = "wheat")
```

<img src="man/figures/README-example1-1.png" width="100%" />

## Example 2

``` r
use_seed(1) %>%
  entity_circle(grain = 1000, size = 2) %>%
  unfold_warp(iterations = 100) %>%
  style_ribbon(palette = "rainbow")
```

<img src="man/figures/README-example2-1.png" width="100%" />

## Example 3

``` r
use_seed(1) %>%
  entity_heart(grain = 1000) %>%
  unfold_slice(iterations = 10) %>%
  style_ribbon(
    palette = "base", 
    colour = "ind", 
    background = "mistyrose"
  ) %>%
  style_overlay(border = "white")
```

<img src="man/figures/README-example3-1.png" width="100%" />

## Example 4

``` r
use_seed(1) %>%
  scene_discs(
    rings = 3, points = 5000, size = 5
  ) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 1,
    scale = .5, 
    output = "layer" 
  ) %>%
  unfold_tempest(
    iterations = 20,
    scale = .01
  ) %>%
  style_ribbon(
    palette = palette_named("vik"),
    colour = "ind",
    alpha = c(.1,.1),
    background = "oldlace"
  )
```

<img src="man/figures/README-example4-1.png" width="100%" />

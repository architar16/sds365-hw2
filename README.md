# SDS 365 – Homework 2: PCA Image Compression

Author: Archita Roy

## Overview

This project explores image compression using Principal Component Analysis (PCA). For a set of grayscale images, each pixel matrix is approximated using only its top *k* principal components, and the resulting approximation error (measured with the Frobenius norm) is analyzed as *k* increases.

## Contents

- **`HW2_ImageCompressionFunction.R`** — Standalone R script implementing `compress_image(X, k)`, which returns the rank-k PCA approximation of an image matrix and its Frobenius norm error.
- **`index.Rmd` / `index.html`** — The project website (knit output), showing the compression function's code, the rank-k approximation grid (k = 1 to 10) for each image, an error-vs-k plot, and an analysis of the first eigenvector and first principal component score for each image.
- **`HW2_DesignInteractiveApp.Rmd` / `HW2_DesignInteractiveApp.html`** — A design mockup and written rationale for an interactive PCA compression app.
- **`HW2_InteractiveApp.R`** — A working Shiny app implementing the interactive design: upload an image, choose *k* with a slider, and view the compressed result alongside the original in real time.
- **`image1.csv`–`image4.csv`** — Pixel matrices for the four images used throughout the analysis.

## How the compression works

For an image matrix `X`, PCA is performed without centering (`center = FALSE`), so the top *k* components can be used to build a literal rank-k approximation:

```r
compress_image(X, k)
```

This returns a list containing:
- `approximation` — the rank-k reconstructed image matrix
- `error` — the Frobenius norm of the difference between the original and the approximation

## Live links

- Website: [https://architar16.github.io/sds365-hw2/index.html](https://architar16.github.io/sds365-hw2/index.html)
- App design mockup: [https://architar16.github.io/sds365-hw2/appdesign.html](https://architar16.github.io/sds365-hw2/appdesign.html)
- Interactive Shiny app: [https://architaroy.shinyapps.io/hw2imagefiles/](https://architaroy.shinyapps.io/hw2imagefiles/)

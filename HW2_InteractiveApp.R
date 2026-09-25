library(shiny)
library(png)
library(jpeg)

## ---- compression engine (same code as the web site) -------------
compress_image = function(X, k) {
  
  X = data.matrix(X)
  pca = prcomp(X, center = FALSE, scale. = FALSE)
  
  if (k < 1 || k > ncol(pca$x)) {
    stop("k must be between 1 and ", ncol(pca$x))
  }
  
  scores_k = pca$x[, 1:k, drop = FALSE]
  loadings_k = pca$rotation[, 1:k, drop = FALSE]
  
  Xk = scores_k %*% t(loadings_k)
  
  error = norm(X - Xk, type = "F")
  
  return(list(
    approximation = Xk,
    error = error
  ))
}


## Decompose once so the slider can move without recomputing eigen().
pca_parts = function(X) {
  X = data.matrix(X)
  eig = eigen(t(X) %*% X, symmetric = TRUE)
  U = eig$vectors
  list(X = X, U = U, Z = X %*% U, values = eig$values)
}

rank_k <- function(parts, k) {
  parts$Z[, 1:k, drop = FALSE] %*% t(parts$U[, 1:k, drop = FALSE])
}

## drawing 

plot_img <- function(M, title = "", zlim = range(M)) {
  n <- nrow(M)
  M[M < zlim[1]] <- zlim[1]
  M[M > zlim[2]] <- zlim[2]
  op <- par(mar = c(0.3, 0.3, 2.0, 0.3)); on.exit(par(op))
  image(t(M[n:1, ]), col = gray.colors(256, start = 0, end = 1, gamma = 1),
        zlim = zlim, axes = FALSE, useRaster = TRUE,
        asp = nrow(M) / ncol(M))
  title(main = title, cex.main = 1.2, font.main = 1, line = 0.6)
}

## reading an uploaded file into a matrix 

read_image_matrix <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    return(data.matrix(read.csv(path)))
  }
  if (ext == "png" && requireNamespace("png", quietly = TRUE)) {
    a <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg") && requireNamespace("jpeg", quietly = TRUE)) {
    a <- jpeg::readJPEG(path)
  } else {
    stop("Upload a .csv matrix, or a .png / .jpg image.")
  }
  ## Collapse colour channels to grey scale.
  if (length(dim(a)) == 3) a <- apply(a[, , 1:min(3, dim(a)[3]), drop = FALSE], c(1, 2), mean)
  a * 255
}


ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; }
    .title-bar { border-bottom: 1px solid #e6e9ed; padding: 14px 0 16px; margin-bottom: 18px; }
    .title-bar h2 { margin: 0; font-weight: 650; font-size: 24px; letter-spacing: -0.01em; }
    .title-bar .sub { color: #6b7480; font-size: 14px; margin-top: 3px; }
    .step { font-size: 11px; letter-spacing: .08em; text-transform: uppercase;
            color: #8a929c; font-weight: 700; margin: 20px 0 8px; }
    .kval { font-size: 26px; font-weight: 650; letter-spacing: -0.02em; }
    .kval small { font-size: 13px; font-weight: 400; color: #8a929c; margin-left: 6px; }
    .statrow { display: flex; justify-content: space-between; padding: 7px 0;
               border-bottom: 1px solid #ebeef1; font-size: 13.5px; }
    .statrow b { font-variant-numeric: tabular-nums; }
    .good { color: #1f4e79; }
    .well { background: #fafbfc; border-color: #e6e9ed; }
  "))),
  
  div(class = "title-bar",
      h2("Lowrank"),
      div(class = "sub", "Compress an image with principal components. Move the slider to choose how many.")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(class = "step", "1 — Image"),
      fileInput("file", NULL,
                accept = c(".csv", ".png", ".jpg", ".jpeg"),
                buttonLabel = "Browse", placeholder = "No file selected"),
      helpText("A CSV of pixel values, or a PNG/JPEG."),
      
      div(class = "step", "2 — Compression"),
      uiOutput("kreadout"),
      uiOutput("kslider"),
      
      div(class = "step", "3 — Result"),
      uiOutput("stats"),
      
      br(),
      downloadButton("dl", "Download compressed matrix (.csv)", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      conditionalPanel(
        condition = "output.loaded != true",
        div(style = "padding:60px 20px; text-align:center; color:#8a929c;",
            h4("Upload an image to begin"),
            p("Try one of the homework CSV files, or any PNG or JPEG."))
      ),
      conditionalPanel(
        condition = "output.loaded == true",
        fluidRow(
          column(6, plotOutput("original",   height = "460px")),
          column(6, plotOutput("compressed", height = "460px"))
        ),
        hr(),
        plotOutput("errorcurve", height = "260px")
      )
    )
  )
)


server <- function(input, output, session) {
  
  ## The uploaded image, as a matrix.
  img <- reactive({
    req(input$file)
    read_image_matrix(input$file$datapath, input$file$name)
  })
  
  ## One eigendecomposition per uploaded file; the slider reuses it.
  parts <- reactive(pca_parts(img()))
  
  output$loaded <- reactive(!is.null(input$file))
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  ## Slider bounds depend on p, so the control is built server-side.
  output$kslider <- renderUI({
    req(input$file)
    p <- ncol(img())
    sliderInput("k", NULL, min = 1, max = p,
                value = min(20, p), step = 1, width = "100%")
  })
  
  output$kreadout <- renderUI({
    req(input$k)
    div(class = "kval", paste0("k = ", input$k),
        tags$small(paste0("of ", ncol(img()), " components")))
  })
  
  ## The rank-k approximation, recomputed only when k or the file changes.
  approx <- reactive({
    req(input$k)
    rank_k(parts(), input$k)
  })
  
  output$original <- renderPlot({
    X <- img()
    plot_img(X, sprintf("Original  —  %d × %d", nrow(X), ncol(X)))
  })
  
  output$compressed <- renderPlot({
    X <- img()
    plot_img(approx(), sprintf("Rank-%d approximation", input$k), zlim = range(X))
  })
  
  output$errorcurve <- renderPlot({
    pr <- parts(); X <- pr$X; p <- ncol(X)
    ## Error(k)^2 equals the sum of the discarded eigenvalues, so the whole
    ## curve comes straight from the spectrum without rebuilding any matrices.
    ev   <- pmax(pr$values, 0)
    errs <- sqrt(pmax(sum(ev) - cumsum(ev), 0))
    op <- par(mar = c(4.2, 4.6, 2.2, 1.2)); on.exit(par(op))
    plot(seq_len(p), errs, type = "l", lwd = 2, col = "#1f4e79",
         xlab = "k (number of principal components)",
         ylab = "Frobenius error",
         main = "Approximation error vs. k", font.main = 1, cex.main = 1.1)
    grid(col = "grey88", lty = 1)
    lines(seq_len(p), errs, lwd = 2, col = "#1f4e79")
    abline(v = input$k, col = "#c0504d", lty = 2)
    points(input$k, errs[input$k], pch = 19, col = "#c0504d", cex = 1.2)
  })
  
  output$stats <- renderUI({
    req(input$k)
    X  <- img(); pr <- parts()
    n  <- nrow(X); p <- ncol(X); k <- input$k
    e  <- norm(X - approx(), type = "F")
    rel  <- e / norm(X, type = "F")
    varp <- sum(pmax(pr$values, 0)[1:k]) / sum(pmax(pr$values, 0))
    keep <- k * (n + p); full <- n * p
    
    row <- function(lab, val, cls = "") {
      div(class = "statrow", span(lab), tags$b(class = cls, val))
    }
    tagList(
      row("Storage", sprintf("%s of %s  (%.1f%%)",
                             format(keep, big.mark = ","),
                             format(full, big.mark = ","),
                             100 * keep / full), "good"),
      row("Frobenius error", sprintf("%.2f", e)),
      row("Relative error",  sprintf("%.2f%%", 100 * rel)),
      row("Variance kept",   sprintf("%.2f%%", 100 * varp))
    )
  })
  
  output$dl <- downloadHandler(
    filename = function() sprintf("compressed_k%d.csv", input$k),
    content  = function(f) write.csv(approx(), f, row.names = FALSE)
  )
}

shinyApp(ui, server)

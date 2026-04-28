library(ggplot2)

set.seed(42)

generate_chart <- function(target_pct, size = 200) {
  # Place random ellipses in a unit square until we hit target area
  # Use rejection sampling approach: place shapes, measure fill
  
  n_shapes <- round(target_pct * 2.5) + 3  # rough starting count
  
  # Generate random elliptical "grains"
  shapes <- data.frame(
    x = runif(n_shapes * 3, 0.05, 0.95),
    y = runif(n_shapes * 3, 0.05, 0.95),
    rx = abs(rnorm(n_shapes * 3, 0.04, 0.025)),
    ry = abs(rnorm(n_shapes * 3, 0.03, 0.02)),
    angle = runif(n_shapes * 3, 0, pi)
  )
  
  # Clip radii
  shapes$rx <- pmin(shapes$rx, 0.08)
  shapes$ry <- pmin(shapes$ry, 0.06)
  shapes$rx <- pmax(shapes$rx, 0.01)
  shapes$ry <- pmax(shapes$ry, 0.008)
  
  # Rasterize to measure actual fill
  grid_n <- 200
  gx <- seq(0, 1, length.out = grid_n)
  gy <- seq(0, 1, length.out = grid_n)
  grid <- expand.grid(gx = gx, gy = gy)
  
  filled <- rep(FALSE, nrow(grid))
  used_shapes <- data.frame()
  current_pct <- 0
  
  for (i in seq_len(nrow(shapes))) {
    if (current_pct >= target_pct) break
    
    s <- shapes[i, ]
    # Rotated ellipse test
    cos_a <- cos(s$angle)
    sin_a <- sin(s$angle)
    dx <- grid$gx - s$x
    dy <- grid$gy - s$y
    rx <- (dx * cos_a + dy * sin_a) / s$rx
    ry <- (-dx * sin_a + dy * cos_a) / s$ry
    inside <- (rx^2 + ry^2) <= 1
    
    filled <- filled | inside
    used_shapes <- rbind(used_shapes, s)
    current_pct <- sum(filled) / nrow(grid) * 100
  }
  
  # Build polygon approximations of ellipses for ggplot
  theta <- seq(0, 2 * pi, length.out = 36)
  
  polys <- do.call(rbind, lapply(seq_len(nrow(used_shapes)), function(i) {
    s <- used_shapes[i, ]
    cos_a <- cos(s$angle)
    sin_a <- sin(s$angle)
    ex <- s$rx * cos(theta)
    ey <- s$ry * sin(theta)
    data.frame(
      x = s$x + ex * cos_a - ey * sin_a,
      y = s$y + ex * sin_a + ey * cos_a,
      id = i
    )
  }))
  
  list(polys = polys, actual_pct = round(current_pct, 1))
}

# Generate charts for key percentages
percentages <- c(1, 3, 5, 7, 10, 15, 20, 25, 30, 40, 50, 55, 60)

for (pct in percentages) {
  result <- generate_chart(pct)
  
  p <- ggplot(result$polys, aes(x = x, y = y, group = id)) +
    geom_polygon(fill = "black", color = NA) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "grey40", linewidth = 1),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  ggsave(
    sprintf("www/images/comparison/pct_%02d.png", pct),
    p, width = 1.2, height = 1.2, dpi = 150, bg = "white"
  )
  
  cat(sprintf("  %2d%% target → %4.1f%% actual\n", pct, result$actual_pct))
}

cat("Done.\n")

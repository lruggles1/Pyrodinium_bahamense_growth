# Plotting growth curves function
# dat is input dataframe
# x is x variable
# y is y variable 
# facet_col = NULL is input variable for column faceting
# facet_row = NULL is input variable for column faceting
library(ggplot2)

library(ggplot2)

plot_func <- function(dat, x_val, y_val,
                      facet_row = NULL, facet_col = NULL,
                      save_plot = FALSE, filename = "plot.png") {
  
  # --- Define your base color palette ---
  base_colors <- c(
    "-Cobalamin" = "#102E50",
    "Replete"    = "#F5C45E",
    "-Thiamine"  = "deeppink4",
    "-Biotin"    = "#667028"
  )
  
  # --- Define legend labels with em-dashes ---
  base_labels <- c(
    "-Cobalamin" = expression("\u2013B"[12]),
    "-Thiamine"  = expression("\u2013Thiamine"),
    "-Biotin"    = expression("\u2013Biotin"),
    "Replete"    = "Replete"
  )
  
  # --- Detect which treatments are present ---
  treatments_present <- unique(dat$Treatment)
  
  # --- Assign colors dynamically (navy if not in base_colors) ---
  palette_colors <- sapply(treatments_present, function(trt) {
    if (trt %in% names(base_colors)) base_colors[trt] else "navy"
  })
  names(palette_colors) <- treatments_present
  
  # --- Base plot ---
  p <- ggplot(dat, aes_string(x = x_val, y = y_val, color = "Treatment")) +
    geom_point(stroke = 1.2, size = 2) +
    geom_smooth(se = FALSE) +
    scale_color_manual(
      values = palette_colors,
      labels = base_labels[names(palette_colors)],
      name = "Treatment"
    ) +
    labs(
      x = "Time (days)",
      y = expression(paste("Chlorophyll-", italic("a"), " fluorescence (RFU)"))
    ) +
    
    scale_y_continuous(
      labels = comma
      
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(fill = NA, color = "black"),
      panel.grid = element_blank(),
      axis.title.y = element_text(family = "Arial", size = 20),
      axis.title = element_text(family = "Arial", size = 20),
      axis.text = element_text(family = "Arial", size = 20, color = "black"),
      strip.text = element_text(family = "Arial", size = 20, color = "black"),
      legend.title = element_text(family = "Arial", size = 16, color = "black"),
      legend.text = element_text(family = "Arial", size = 16, color = "black"),
      legend.position = "bottom"
    )
  
  # --- Add faceting dynamically ---
  if (!is.null(facet_row) && !is.null(facet_col)) {
    p <- p + facet_grid(reformulate(facet_col, facet_row))
  } else if (!is.null(facet_row)) {
    p <- p + facet_grid(rows = vars(!!as.symbol(facet_row)))
  } else if (!is.null(facet_col)) {
    p <- p + facet_grid(cols = vars(!!as.symbol(facet_col)))
  }
  
  # --- Optional save ---
  if (save_plot) {
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
  }
  
  return(p)
}



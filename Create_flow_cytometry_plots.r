```r
# Load required libraries
library(ggplot2)
library(ggbreak)
library(dplyr)
library(tidyr)
library(readxl)

# Function to create plots with Y-axis break (or without)
create_broken_bar_plot <- function(
    # Data parameters
    file_path,
    sheet_name,
    cell_type = "M1",  # "M1", "M2" or "CTL"
    day = 11,          # 11 or 20
    
    # Plot parameters
    title = "M1 Cells - Day 11",
    sign = NULL,  # Label above Y axis (e.g., "A", "B", "C")
    sign_fontsize = 14,  # Label font size
    sign_fontface = "bold",  # Label font face
    sign_x_offset = 0,  # Label horizontal offset
    sign_y_offset = 0,  # Label vertical offset
    title_fontsize = 14,
    y_axis_title = "Value (%)",
    y_axis_title_fontsize = 12,
    y_ticks_fontsize = 11,
    x_axis_title = NULL,
    x_axis_title_fontsize = 12,
    x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
    x_labels_fontsize = 11,
    show_legend = TRUE,
    legend_fontsize = 10,
    breaks = NULL,  # If NULL - plot without break
    y_limits = NULL,  # Y-axis limits for plot without break
    y_breaks = NULL,  # Y-axis labels for plot without break
    upper_breaks = seq(4, 20, by = 4),
    lower_breaks = seq(0, 0.001, by = 0.00025),
    
    # Save parameters
    save_plot = TRUE,
    output_filename = "custom_plot.png",
    plot_width = 10,
    plot_height = 8,
    plot_dpi = 300
) {
  
  # 1. Read data from Excel file
  data <- read_excel(file_path, sheet = sheet_name)
  
  # Rename first column for convenience
  colnames(data)[1] <- "Treatment"
  
  # 2. Determine required columns based on cell type
  if (cell_type == "M1") {
    cell_columns <- c("Treatment", 
                      paste0("M1 sim"), 
                      paste0("M1 ", 1:4, " exp"))
  } else if (cell_type == "M2") {
    cell_columns <- c("Treatment", 
                      paste0("M2 sim"), 
                      paste0("M2 ", 1:4, " exp"))
  } else if (cell_type == "CTL") {
    cell_columns <- c("Treatment", 
                      paste0("CTL sim"), 
                      paste0("CTL ", 1:4, " exp"))
  } else {
    stop("cell_type must be one of: 'M1', 'M2', 'CTL'")
  }
  
  # Filter data for selected cells only
  cell_data <- data[, cell_columns]
  
  # 3. Transform data to long format
  cell_long <- cell_data %>%
    pivot_longer(
      cols = -Treatment,
      names_to = c("Type", "Replicate"),
      names_sep = " ",
      values_to = "Value"
    ) %>%
    mutate(
      Type = case_when(
        Replicate == "sim" ~ "Simulation",
        TRUE ~ "Experimental"
      ),
      Replicate = ifelse(Replicate == "sim", "sim", Replicate)
    )
  
  # 4. Create separate data for simulation and experiment
  sim_data <- cell_long %>%
    filter(Type == "Simulation") %>%
    group_by(Treatment) %>%
    summarise(Mean = mean(Value), .groups = "drop")
  
  exp_data <- cell_long %>%
    filter(Type == "Experimental") %>%
    group_by(Treatment) %>%
    summarise(
      Mean = mean(Value),
      SD = sd(Value),
      .groups = "drop"
    )
  
  # 5. Create separate data for individual points
  individual_points <- cell_long %>%
    filter(Type == "Experimental") %>%
    mutate(Replicate_num = as.numeric(gsub("exp", "", Replicate)))
  
  # 6. Create factor for correct treatment order
  sim_data$Treatment <- factor(sim_data$Treatment, levels = x_labels)
  exp_data$Treatment <- factor(exp_data$Treatment, levels = x_labels)
  individual_points$Treatment <- factor(individual_points$Treatment, levels = x_labels)
  
  # 7. Create artificial variable for positioning bars side by side
  sim_data$Position <- as.numeric(sim_data$Treatment) - 0.2
  exp_data$Position <- as.numeric(exp_data$Treatment) + 0.2
  individual_points$Position <- as.numeric(individual_points$Treatment) + 0.2
  
  # 8. Create base plot
  p_base <- ggplot() +
    # Columns for experiment with standard deviation (left)
    geom_col(
      data = exp_data,
      aes(x = Position, y = Mean, fill = "Experimental"),
      width = 0.35,
      alpha = 0.7
    ) +
    # Columns for simulation (right)
    geom_col(
      data = sim_data,
      aes(x = Position, y = Mean, fill = "Simulation"),
      width = 0.35,
      alpha = 0.7
    ) +
    # Error bars for experimental data
    geom_errorbar(
      data = exp_data,
      aes(x = Position, ymin = Mean - SD, ymax = Mean + SD),
      width = 0.2
    ) +
    # Individual points for experimental data
    geom_point(
      data = individual_points,
      aes(x = Position, y = Value),
      position = position_jitter(width = 0.1),
      size = 2,
      color = "black",
      alpha = 0.5
    ) +
    # X-axis settings
    scale_x_continuous(
      name = x_axis_title,
      breaks = seq_along(x_labels),
      labels = x_labels,
      limits = c(0.5, length(x_labels) + 0.5)
    ) +
    # Color settings
    scale_fill_manual(
      values = c("Simulation" = "#4682B4", "Experimental" = "coral"),
      name = "Data Type"
    ) +
    # Appearance settings
    labs(
      title = title,
      x = x_axis_title,
      y = y_axis_title
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5, 
        face = "bold", 
        size = title_fontsize
      ),
      axis.text.x = element_text(
        angle = 45, 
        hjust = 1, 
        size = x_labels_fontsize
      ),
      axis.text.y = element_text(size = y_ticks_fontsize),
      axis.title.x = element_text(size = x_axis_title_fontsize),
      axis.title.y = element_text(size = y_axis_title_fontsize),
      legend.position = ifelse(show_legend, "bottom", "none"),
      legend.title = element_text(size = legend_fontsize),
      legend.text = element_text(size = legend_fontsize),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    )
  
  # 9. Add label outside plot (above Y axis)
  if (!is.null(sign) && sign != "") {
    p_base <- p_base +
      labs(tag = sign) +  # Use tag for label outside plot
      theme(
        plot.tag = element_text(
          size = sign_fontsize,
          face = sign_fontface,
          hjust = 0.5 + sign_x_offset,  # Center by default
          vjust = 0.5 + sign_y_offset,  # Center by default
          margin = margin(t = 0, r = 0, b = 0, l = 0)  # No margins
        ),
        plot.tag.position = c(0.02, 0.98)  # Position in plot fractions
      )
  }
  
  # 10. Check if Y-axis break is needed
  if (is.null(breaks)) {
    # Plot WITHOUT Y-axis break
    cat("Creating plot WITHOUT Y-axis break\n")
    
    # Automatically determine suitable Y-axis limits and labels
    if (is.null(y_limits)) {
      # Find maximum value among all data
      all_values <- c(exp_data$Mean + exp_data$SD, sim_data$Mean, individual_points$Value)
      y_max <- max(all_values, na.rm = TRUE) * 1.1  # Add 10% margin
      y_limits <- c(0, y_max)
    }
    
    if (is.null(y_breaks)) {
      # Automatically generate nice labels
      y_breaks <- pretty(y_limits, n = 6)
    }
    
    p <- p_base +
      scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks,
        expand = expansion(mult = c(0, 0.1))
      )
    
  } else {
    # Plot WITH Y-axis break
    cat("Creating plot WITH Y-axis break\n")
    
    # Create axis labels
    all_ticklabels <- c(lower_breaks, upper_breaks)
    
    p <- p_base + 
      scale_y_break(
        breaks = breaks,
        scales = 1,
        expand = expansion(mult = c(0, 0.1)),
        space = 0.15,
        ticklabels = all_ticklabels
      ) +
      theme(
        # Remove X-axis labels from top panel
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title.x.top = element_blank(),
        # Preserve label position
        plot.tag.position = c(0.02, 0.98)
      )
  }
  
  # 11. Save plot if required
  if (save_plot) {
    ggsave(
      output_filename,
      plot = p,
      width = plot_width,
      height = plot_height,
      dpi = plot_dpi,
      bg = "white"
    )
    cat("Plot saved as '", output_filename, "'\n", sep = "")
  }
  
  # 12. Return plot
  return(p)
}

# Font size settings
my_title_fontsize = 26
my_y_axis_title_fontsize = 22
my_x_axis_title_fontsize = 22
my_x_labels_fontsize = 18
my_y_ticks_fontsize = 14
my_sign_fontsize = 26

# Plot for M1-like type cells, day 11
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_11_day (2)",
  cell_type = "M1",
  day = 11,
  title = "M1-like type (day 11)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 10,
  sign = "(A)",
  sign_fontsize = my_sign_fontsize,
  #breaks = c(0.001, 3),
  #upper_breaks = seq(2, 20, by = 6),
  #lower_breaks = seq(0, 0.001, by = 0.00025),
  output_filename = "M1_day11_CD45_3.png"
)

# Plot for M1-like type cells, day 20
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_20_day (2)",
  cell_type = "M1",
  day = 11,
  title = "M1-like type (day 20)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 10,
  sign = "(D)",
  sign_fontsize = my_sign_fontsize,
  #breaks = c(0.001, 3),
  #upper_breaks = seq(2, 20, by = 6),
  #lower_breaks = seq(0, 0.001, by = 0.00025),
  output_filename = "M1_day20_CD45_3.png"
)

# Plot for M2-like type cells, day 11
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_11_day (2)",
  cell_type = "M2",
  day = 11,
  title = "M2-like type (day 11)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 10,
  sign = "(B)",
  sign_fontsize = my_sign_fontsize,
  #breaks = c(0.001, 3),
  #upper_breaks = seq(2, 20, by = 6),
  #lower_breaks = seq(0, 0.001, by = 0.00025),
  output_filename = "M2_day11_CD45_3.png"
)

# Plot for M2-like type cells, day 20
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_20_day (2)",
  cell_type = "M2",
  day = 11,
  title = "M2-like type (day 20)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 24,
  sign = "(E)",
  sign_fontsize = my_sign_fontsize,
  #breaks = c(0.001, 3),
  #upper_breaks = seq(2, 20, by = 6),
  #lower_breaks = seq(0, 0.001, by = 0.00025),
  output_filename = "M2_day20_CD45_3.png"
)

# Plot for CD8+ T-cells, day 11
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_11_day (2)",
  cell_type = "CTL",
  day = 11,
  title = "CD8+ T-cells  (day 11)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 10,
  sign = "(C)",
  sign_fontsize = my_sign_fontsize,
  breaks = c(0.0008, 0.001),
  upper_breaks = seq(0.001, 5, by = 2),
  lower_breaks = seq(0, 0.0008, by = 0.0002),
  output_filename = "CTLs_day11_CD45_3.png"
)

# Plot for CD8+ T-cells, day 20
plot_m1_day11_live <- create_broken_bar_plot(
  file_path = "c:\\Users\\Sirius\\Desktop\\Аспирантура\\Статья Frontiers in Immunology\\Tables\\Flow_cytometry_individual.xlsx",
  sheet_name = "%_of_CD45+_20_day (2)",
  cell_type = "CTL",
  day = 11,
  title = "CD8+ T-cells (day 20)",
  title_fontsize = my_title_fontsize,
  y_axis_title = "% of CD45",
  y_axis_title_fontsize = my_y_axis_title_fontsize,
  y_ticks_fontsize = my_y_ticks_fontsize,
  #x_axis_title = "Treatment groups",
  x_axis_title_fontsize = my_x_axis_title_fontsize,
  x_labels = c("PBS", "rVSV-GFP", "rVSV-mIL12-mGMCSF"),
  x_labels_fontsize = my_x_labels_fontsize,
  show_legend = FALSE,
  legend_fontsize = 10,
  sign = "(F)",
  sign_fontsize = my_sign_fontsize,
  breaks = c(0.015, 0.1),
  #upper_breaks = seq(2, 20, by = 6),
  #lower_breaks = seq(0, 0.001, by = 0.00025),
  output_filename = "CTLs_day20_CD45_3.png"
)
```
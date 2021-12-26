
# Loading packages, data, functions and setting needed variables
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, ggforce,ggtext)

dims <- list(
  length = 110,
  width = 73,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.21,
  penalty_spot_distance = 11,
  central_circle_radius = 9.15, 
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)


# function to draw the pitch
get_pitch <- function(gp, dims, pitch_fill = "white", pitch_col = "grey70", background_fill = pitch_fill, margin = 0){
  
  contorno_df <- data.frame(x = dims$origin_x, 
                            xend = dims$length,
                            y = dims$origin_y,
                            yend = dims$width)
  
  x_start_areas <- c(dims$origin_x, dims$origin_x, dims$length - dims$penalty_box_length, dims$length - dims$six_yard_box_length)
  
  x_end_areas <- c(dims$penalty_box_length, dims$six_yard_box_length, dims$length, dims$length)
  
  y_start_areas <- c((dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2, (dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2)
  
  y_end_areas <- c(dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2, dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2)
  
  areas_df<- data.frame(x = x_start_areas, 
                        xend = x_end_areas, 
                        y = y_start_areas,
                        yend = y_end_areas)
  
  gp +
    theme_void() +
    theme(panel.background = element_rect(fill = background_fill, colour = "transparent"),
          plot.margin = unit(c(margin, margin, margin, margin), "cm")) +
    
    # rectangles
    #areas
    geom_rect(data = contorno_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    geom_rect(data = areas_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    #nets
    geom_rect(aes(xmin = dims$length, xmax = dims$length + 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    geom_rect(aes(xmin = dims$origin_x, xmax = dims$origin_x - 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    
    #points
    geom_point(aes(x = dims$length/2, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$length - dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    
    #central circle
    geom_circle(aes(x0 = dims$length/2, y0 = dims$width/2, r = dims$central_circle_radius), color = pitch_col) +
    #central line
    geom_segment(aes(x = dims$length/2, xend = dims$length/2, y = dims$width, yend = dims$origin_y), color = pitch_col) +
    
    #semi circles boxes
    geom_arc(aes(x0 = dims$length - dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = -37*pi/180, end = -143*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = 37*pi/180, end = 143*pi/180), col = pitch_col) +
    
    #semi circles corners
    geom_arc(aes(x0 = dims$length, y0 = dims$origin_y, r = 1, 
                 start = 270*pi/180, end = 360*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$length, y0 = dims$width, r = 1, 
                 start = 180*pi/180, end = 270*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$origin_y, r = 1, 
                 start = 0*pi/180, end = 90*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$width, r = 1, 
                 start = 90*pi/180, end = 180*pi/180), col = pitch_col)
}
#----------------------

#load data
data <- read_csv("data_goal_di_maria.csv")

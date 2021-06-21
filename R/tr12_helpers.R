#' Calculate dowel bearing strength at an angle to grain
#'
#' @param f_e_parallel The dowel bearing strength parallel to grain in psi
#' @param f_e_perp The dowel bearing strength perpendicular to grain in psi
#' @param theta The angle between the direction of load and the direction of grain
#'
#' @return The composite dowel bearing strength when load is applied at an angle per equation NDS 12.3-11
#' @export
#'
#' @examples
compute_f_e_theta <- function(f_e_parallel, f_e_perp, theta){
  (f_e_parallel*f_e_perp) / (f_e_parallel*(sin(theta*pi/180)^2) + f_e_perp*(cos(theta*pi/180)^2))
}

#' Reduction term as noted in NDS 2018 Table 12.3.1B
#'
#' @param diameter The diameter of the dowel
#' @param theta_main The angle between the direction of load and the direction of grain for the main member
#' @param theta_side The angle between the direction of load and the direction of grain for the side member
#' @param reduction_mode The mode designated from NDS 2018 Table 12.3.1B
#'
#' @return An integer reduction value
#' @export
#'
#' @examples
compute_reduction_term <- function(diameter, theta_main, theta_side, reduction_mode) {
  reduction_value <- 0
  max_angle <- max(c(theta_main,theta_side))
  K_theta <- (1+0.25*max_angle/90)
  
  if(diameter <= 0.17) {
    reduction_value <- 2.2
  } else if (diameter <= 0.25) {
    reduction_value <- 10*diameter+.5
  } else if (diameter > 0.25 && diameter <= 1) {
    switch(mode,
           "1" = {reduction_value <- 4 * K_theta},
           "2" = {reduction_value <- 3.6 * K_theta},
           "3" = {reduction_value <- 3.2 * K_theta},
           "4" = {reduction_value <- 3.2 * K_theta})
  }
  
  reduction_value
  
}



#' Calculate the dowel bearing resistances "q" for main and side members q_m and q_s
#'
#' @param f_e_parallel The dowel bearing strength parallel to grain in psi
#' @param f_e_perp The dowel bearing strength perpendicular to grain in psi
#' @param theta The angle between the direction of load and the direction of grain
#' @param diameter The diameter of the dowel
#'
#' @return
#' @export
#'
#' @examples
compute_q <- function(f_e_parallel, f_e_perp, theta, diameter) {
  F_e_m_theta <- compute_f_e_theta(f_e_parallel, f_e_perp, theta)
  F_e_m_theta * diameter
}


#' Calculate the bearing moment resistance of a dowel of given bending yield and diameter
#'
#' @param f_yb Specified minimum dowel bending yield strength of a fastener, psi.
#' @param diameter Dowel diameter
#'
#' @return M_d, dowel moment resistance
#' @export
#'
#' @examples
compute_dowel_moment <- function(f_yb, diameter) {
  f_yb * (diameter^3)/6
}

compute_bearing_moment <- function(dowel_bearing_resistance, length ){
  0
}

#' Maximum moment from dowel and bearing moment.
#'
#' @param dowel_moment 
#' @param bearing_moment 
#'
#' @return
#' @export
#'
#' @examples
get_max_moment <- function(dowel_moment, bearing_moment) {
  max(c(dowel_moment,bearing_moment))
}

yield_mode_i_main <- function( ){}

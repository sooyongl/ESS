#' @include tab1_function.r
#'
tab2_plot_data <- function(dataInp, information, vvv){

  dataUse_1 <- dataInp
  # SD <- ifelse(is.na(information$data_ready$SD_data[vvv]), 1,
  #              information$data_ready$SD_data[vvv])

  target_filter <- information$base_data$target_nm
  targ_p <- which(names(dataUse_1) == target_filter)
  lv_names <- names(dataUse_1)[(targ_p + 1):(ncol(dataUse_1)-1)]
  loc_nm <- information$base_data$loc_nm

  dataUse_1 <-
    dataUse_1 %>%
    mutate(OOD_1 = paste0(OOD,"\n", !!as.name(loc_nm)),
           OOD_1 = factor(OOD_1, levels = paste0(OOD,"\n", !!as.name(loc_nm))
           )
    ) %>%

    select(OOD_1, !!as.name(loc_nm), (targ_p + 1):(ncol(dataUse_1)-1)) %>%
    gather(., "Cut Level", "Inconsistency", -c(OOD_1), -!!as.name(loc_nm))

  if(information$base_data$WESS) {
    dataUse_1 <-
      dataUse_1 %>%
      filter( str_detect(`Cut Level`, "_W" ))
    # %>%
    #   mutate(Inconsistency = round(Inconsistency/SD,1))

  } else {
    dataUse_1 <-
      dataUse_1 %>%
      filter( !str_detect(`Cut Level`, "_W" ))
  }
  return(dataUse_1)
}
#'

tab2_plot <- function(dataUse, WESS){

  breaks = function(x, xby) x[seq(1, length(x), by=xby)]

  cutLevel <- dataUse %>% pull(`Cut Level`) %>% unique()

  incon_value <- foreach(i = 1:length(cutLevel), .combine = 'cbind') %do% {
    dataUse %>% filter(`Cut Level` == cutLevel[i]) %>% pull(Inconsistency)
  }

  cross_value <- foreach(i = 1:(dim(incon_value)[2] - 1), .combine = 'c') %do% {
    ii = i + 1
    incon_value[which( incon_value[,i] == incon_value[,ii] )[1], 1]
  }

  bk_x = ceiling(length(unique(dataUse$OOD_1))/20)
  x_disc <- breaks(unique(dataUse$OOD_1), bk_x)

  unique_incons <- unique(dataUse$Inconsistency)
  bk_y = ceiling(length(unique_incons)/20)
  y_disc <- breaks(unique(dataUse$Inconsistency), bk_y)

  pp1 <-
    dataUse %>%
    ggplot(aes(x = OOD_1, y = Inconsistency)) +
    geom_point(aes(colour = factor(`Cut Level`)),
               fill = "white", size = 1, stroke = .5, shape = 21) +
    geom_line(aes(group = factor(`Cut Level`),
                  colour = factor(`Cut Level`),
                  linetype = factor(`Cut Level`)),
              size = 1.5,
              alpha = .7) +
    theme_bw(base_size = 16) +
    guides(linetype = FALSE) +
    scale_x_discrete(breaks = x_disc) +
    scale_y_continuous(breaks = y_disc) +
    theme(plot.margin = unit(c(.1,.1,.1,.1), "cm")) +
    theme(legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.position = "bottom",# c(0.025,0.975),
          legend.justification = c(0, 1)) +
    labs(x = "Order of Difficulty",
         y = "Number of Inconsistent Items",
         colour = "Cut Level",
         title = "Number of Inconsistent Items by OOD",
         caption = "")

  if(WESS){
    pp1 <-
      pp1 +
      labs(y = "Total Weights",
           title = "Total Weight by OOD") +
      scale_y_continuous(
        breaks = seq(from = round(min(dataUse$Inconsistency),0), to = round(max(dataUse$Inconsistency),0), length.out = 5)
      )
  }
  pp1
}

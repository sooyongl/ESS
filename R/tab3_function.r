#' @include tab2_function.r
#' gen_page_data
gen_page_data <- function(ep, datainp, fourthData, fifthData,
                          pageName, targetLoc, lvnm){
  # pageName <- page_name; targetLoc <- target_loc

  dataUse_1 <- datainp[ep, ]
  gca_id <- dataUse_1 %>% pull(1)
  cutpoint_inp <- dataUse_1 %>% select(all_of(pageName))
  loc_num <- fourthData[[ep]] %>% pull(targetLoc)

  # sclScore <- fifthData[[ep]] %>% pull(2)

  sclScore <-
    if(sum(str_detect(toupper(names(fifthData[[ep]])), toupper("freq"))) == 0) {
      fifthData[[ep]] %>% pull(2)

    } else {
      freq_to_vec(fifthData[[ep]])
    }

  min_point <- cutpoint_inp %>% unlist()

  lv_data <- lv_por(sclScore, loc_num, min_point)

  scale_score <- map(1:length(min_point), ~ loc_num[min_point[.x]]) %>% unlist()

  perc_in <- lv_data$pors
  perc_atabo <- lv_data$atabv
  perc_bel <- lv_data$bel

  scale_scores <-
    scale_score %>%
    tibble( scaleScore = ., Level = lvnm[-1], GCA = gca_id)

  perc_ins <-
    perc_in %>%
    tibble( percIn = ., Level = lvnm, GCA = gca_id)

  perc_atabos <-
    perc_atabo %>%
    tibble( percAtabo = ., Level = lvnm[-1], GCA = gca_id)

  perc_bel <-
    perc_bel %>%
    tibble( percBel = ., Level = lvnm[-1], GCA = gca_id)

  return(
    list(scale_scores = scale_scores,perc_ins = perc_ins,
         perc_atabos = perc_atabos, perc_bel = perc_bel))
}
#' freq_to_vec
freq_to_vec = function(data) {

  rep(data$score, data$freq)
}
#' lv_por
lv_por <-
  function(scores, locnum, minp){
    lv_first_por <-
      round(sum(scores < locnum[(minp[1])]) / length(scores), 3)*100

    lv_md_por <-
      foreach(i = 1:(length(minp)-1), .combine = 'c') %do% {

        myi <- i
        round(sum( scores < locnum[(minp[(myi+1)])] & scores >= locnum[(minp[myi])]) / length(scores), 3)*100

      }
    lv_last_por <- round(sum( scores >= locnum[(minp[length(minp)])]) / length(scores), 3)*100

    atabv <-
      foreach(i = 1:(length(minp)), .combine = 'c') %do% {
        round(sum( scores >= locnum[(minp[i])]) / length(scores), 3)*100
      }

    bel <-
      foreach(i = 1:(length(minp)), .combine = 'c') %do% {
        round(sum( scores < locnum[(minp[i])]) / length(scores), 3)*100
      }

    list(pors = c(lv_first_por, lv_md_por, lv_last_por),
         atabv = atabv, bel = bel)
  }
#' gen_blank_page
gen_blank_page <-
  function(GCA, lvnm){
    # GCA = gca_nm; lvnm = information$data_ready$level_nm[[1]]
    `.` <- c("OIB Page","Cut Scores","Percent in Level",
             "Percent at or Above Level","Percent Below Level")
    page_name <- paste0("Level", 1:length(lvnm))
    sum_table_1 <- tibble(GCA, `.`)
    for(pn in page_name) {
      sum_table_1 <- sum_table_1 %>%
        mutate(!!pn := 0)
    }
    return(sum_table_1)
  }
#' summarize_page
summarize_page <-
  function(ep, blankPage, perIns, percAtabove, perBelow, minData, lvnm,
           numItem, locNum){
    # ep=1; blankPage = blank_page; perIns = perIn; percAtabove = percAtabv;
    # perBelow = percBel; minData = min_data; lvnm = level_names; numItem = num_item; locNum <- loc_num

    sum_page <- blankPage %>% .[[ep]]
    perIn_c <- perIns %>% .[[ep]] %>% pull(perIn_c)
    percAtabo_c <- percAtabove %>% .[[ep]] %>% pull(percAtabo_c)
    percBel_c <- perBelow %>% .[[ep]] %>% pull(percBel_c)
    min_point <- minData[ep, ] %>% unlist()
    numItem_1 <- numItem %>% .[[ep]]
    locNum_1 <- locNum %>% .[[ep]] %>% unlist()

    levelData <- vector("list", length(lvnm))
    levelData[[1]] <- c(glue("1-{min_point[1]-1}"), "", perIn_c[1], "100%", "" )

    for(i in 1:length(min_point)) { # i = 1
      ii <- i + 1
      point_inp <- ifelse(ii <= length(min_point), min_point[ii] - 1, numItem_1)
      levelData[[ii]] <- c(glue("{min_point[i]}-{point_inp}"), locNum_1[min_point[i]],perIn_c[ii], percAtabo_c[i], percBel_c[i])
    }

    for(li in 1:length(levelData)) { # li = 1
      lii <- li + which(names(sum_page)==".")
      sum_page[,lii] <- levelData[[li]]
    }
    return(sum_page)
  }

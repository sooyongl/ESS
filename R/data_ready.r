#' @include import.r
NULL

#' read_data
#'
#' @param filePath a character for a file path.
#' @return a named list containing standard setting information.
#' @examples
#' \dontrun{
#' require(embededss)
#' filePath <- "data/freq_data.xlsx"
#' imported_data <- read_data(filePath)
#' }
#' @export
read_data <- function(filePath) {
  imprt_data <-
    filePath %>%
    excel_sheets() %>%
    map(read_excel, path = filePath) %>%
    set_names(., nm = c("setup", "panelist","rating","item_data","examinee_data"))

  return(imprt_data)
}

#' data_ready
#'
#' @param imprtData a named list containing standard setting information
#' @return a list
#' @examples
#' \dontrun{
#' require(embededss)
#' filePath <- "data/freq_data.xlsx"
#' imported_data <- read_data(filePath)
#' data_list <- data_ready(imported_data)
#' }
#' @export
data_ready <- function(imprtData) { # imprtData = imprt_data

  setup_data    <- imprtData[[1]]
  panelist_data <- imprtData[[2]]
  rating_data   <- imprtData[[3]]
  item_data     <- imprtData[[4]]
  examinee_data <- imprtData[[5]]
  # setup data
  setup_name <- names(setup_data)

  lv_position <- which(remove_blank(setup_name) == remove_blank("Level Options"))
  # level_opt_name <- setup_name[lv_position]

  lv_list <-
    map(str_split(setup_data[[lv_position]], ","),remove_blank)
  lv_vec <-
    lv_list %>%
    lapply(., function(x) sapply(1:length(x), function(xx) paste0("Level",xx) )) %>%
    map(., ~paste(.x, collapse = ", ")) %>%
    do.call("rbind", .)

  setup_data[[lv_position]] <- lv_vec
  setup_data  <- setup_data %>% arrange(1)

  # rating data
  rating_name <- names(rating_data)

  ald_position <- which(remove_blank(rating_name) == remove_blank("ald"))
  ald_opt_name <- rating_name[ald_position]

  panel_position <- which(str_detect(remove_blank(rating_name), "USE|PANEL"))

  new_lv_name <-
    rating_data %>%
    mutate(ALD = remove_blank(ALD)) %>%
    group_split(GCA) %>%
    map(., ~ .x %>% pull(ALD)) %>%
    map2(., lv_list, ~ paste0("Level", match(.x, .y))) %>%
    unlist(.)

  names(rating_name) <- c("GCA","Subject","Grade","Round","Table","Panelist","ALD")

  rating_data[[ald_position]] <- new_lv_name

  # item data
  item_data <- item_data  %>% arrange(1,2)

  # examinee data
  examinee_data <- reorganize_examinee(examinee_data)
  gca_position <-
    which(remove_blank(names(examinee_data)) == remove_blank("grade")|
          remove_blank(names(examinee_data)) == remove_blank("gca"))

  names(examinee_data)[gca_position] <- "GCA"

  res <- list(setup_data = setup_data, panelist_data = panelist_data,
          rating_data = rating_data, item_data = item_data,
          examinee_data = examinee_data)

  return(res)
}

#' get_data_info
#'
#' @return a list containing all the information for later estimation.
#' @example
#' \dontrun{
#' require(embededss)
#' filePath <- "data/freq_data.xlsx"
#' imported_data <- read_data(filePath)
#' data_list <- data_ready(imported_data)
#' data_information <- get_data_info(data_list, grade = c("M3"), ald = "ALD", location = "Loc_RP60", wess = F, modal = F, threshold = F)
#'
#' }
#' @export
get_data_info <- # readyData <- data_list; inputs <- list(grade = c("M3"), ald = "ALD", location = "Loc_RP60", wess = F, modal = F, threshold = F)
  function(readyData, ...){ # readyData <- data_list

  information <- list()

  inputs <- list(...)
  names(inputs) <- tolower(names(inputs))

  filtered_data <- readyData[[3]] %>% filter(GCA %in% inputs[["grade"]])
  id_list = get_ID(filtered_data)
  SD_data <- get_SD(readyData[[1]], id_list)
  lv_p <- which(remove_blank(names(readyData[[1]])) == "LEVELOPTIONS")
  level_nm0 <- readyData[[1]][readyData[[1]]$GCA %in% inputs[["grade"]], lv_p]
  # names(level_nm0) <- inputs[["grade"]]
  information$imported_data <-
    list(
      setup_data     = readyData[[1]],
      panel_data     = readyData[[2]],
      rating_data    = readyData[[3]],
      item_data      = readyData[[4]],
      examinee_data  = readyData[[5]]
    )

  # information$base_data <-
  information$given_data <-
    list(
      target_nm     = inputs[["ald"]],
      loc_nm        = inputs[["location"]],
      WESS_nm       = inputs[["wess"]],
      modal_nm      = inputs[["modal"]],
      threshold     = inputs[["threshold"]],
      filtered_data = filtered_data
    )

  information$data_ready <-
    list(
      id_list = id_list,
      level_nm = get_lvnm(level_nm0, inputs[["grade"]]),
      location_ready = get_location(readyData[[4]], inputs[["location"]], inputs[["grade"]]),
      SD_data = SD_data
    )

  information$split_data <-
    filtered_data %>%
    group_split(!!as.name(names(filtered_data)[str_detect(names(filtered_data), "Panel|User")]))

  return(information)
  }

###############################################################################
# Helper functions
#------------------------------------------------------------------------------
#' remove_blank_vector
#'
remove_blank <- function(inpData) {

  vec <- inpData %>% stri_replace_all_charclass(., "\\p{WHITE_SPACE}", "")
  vec <- toupper(vec)
  return(vec)
}
#
reorganize_examinee <- function(inpData){
    if(sum(str_detect(toupper(names(inpData)), toupper("freq"))) == 0){

      return(inpData)

    } else {
      inpdata_reorg <- vector("list", ncol(inpData))

      for(i in 1:ncol(inpData)) {
        # i <- 4
        if(sum(is.na(inpData[i])) == nrow(inpData)){
          next
        }
        inpdata_reorg[[i]] <- inpData[i]
      }
      inpdata_reorg <- inpdata_reorg %>% bind_cols()

      inpdata_reorg <-
        foreach(i = 1:(ncol(inpdata_reorg)/3), .combine = 'rbind') %do% {
          ii = 1 + (i - 1)*3
          iii = ii + 2
          inpdata_reorg[,ii:iii] %>% drop_na() %>%
            set_names(., nm = c("score", "freq","GCA"))
        }
      return(inpdata_reorg)
    }
  }

get_ID <- function(filteredData) {

  GCAID <-
    filteredData %>%
    distinct(GCA) %>%
    pull()

  TableID<-
    filteredData %>%
    distinct(GCA, Table)

  Table_n <- TableID %>% group_by(GCA) %>% count() %>% pull(n)

  p1 <- as.name(names(filteredData)[str_detect(names(filteredData), "Panel|User")])

  UserID <-
    filteredData %>%
    distinct(GCA, Table, !!p1)

  return(list(GCA = GCAID, Table = TableID, Table_n = Table_n,
              PanelID = UserID))
}
#
get_SD <- function(setupData, id_list){
  SD_data =
    tryCatch({
      a1 <- setupData %>% filter(GCA %in% id_list$GCA) %>% pull(SD)
      a1[which(is.na(a1))] <- 1
      a1
    },
    error = function(e) 1
    )

  SD_data <- data.frame(GCAid = id_list$GCA, SD = SD_data)
}
#
get_lvnm <- # inpData = level_nm0; GCAID = inputs[["grades"]]
  function(inpData, GCAID){
    .get_lvnm <-
      function(inpData) {
        # inpData <- first_data[,5]
        lvnm <-
          inpData %>%
          str_split(., ",") %>%
          unlist() %>%
          stri_replace_all_charclass(.,
                                     "\\p{WHITE_SPACE}", "")
        return(lvnm)
      }

    apply(inpData, 1, .get_lvnm) %>%
      data.frame() %>%
      set_names(., nm = GCAID)
  }
#
get_location <- function(fourthData, locNm, testinp){
  # fourthData <- fourth_data; locNm <- input$loc
  dataUsed <- fourthData %>% filter(GCA %in% testinp)
  GCAId <- fourthData %>% filter(GCA %in% testinp) %>% pull(GCA) %>% unique()

  location <-
    dataUsed %>%
    group_split(GCA) %>%
    map(., ~ .x %>% select(GCA, Item_ID, all_of(locNm))) %>%
    set_names(., nm = GCAId)

  bind_loc <-
    location %>%
    map(., ~ .x %>% arrange(!!as.name(locNm)) %>%
          mutate(OOD = 1:nrow(.)) %>%
          select(-c(1:2))
    )
  return(list(location = location, bind_loc = bind_loc))

}


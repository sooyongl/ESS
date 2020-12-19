library(shiny);
library(shinythemes);
library(shinyWidgets);
library(tidyverse);
library(glue);
library(foreach);
library(stringi);
library(DT);
library(knitr);
library(kableExtra);
library(readxl);
library(officer);
library(flextable);
library(shinycssloaders);
library(waiter)

root <- rprojroot::find_rstudio_root_file()

data_path <- file.path(root, "test/data")

for(i in fs::dir_ls("R")){
  source(i)
}

filePath <- fs::dir_ls(data_path)[1]
imported_data <- read_data(filePath)
data_list <- data_ready(imported_data)
data_information <-
  get_data_info(
    data_list,
    grade = c("M3","M4"),
    ald = "ALD",
    location = "Loc_RP67",
    wess = F,
    modal = F,
    threshold = F)

intials_cs_list <- get_initial_cs(data_information$split_data,data_information)
intials_cp_list <- get_initial_cp(data_information, intials_cs_list)
cs_table_list <- get_cs_table(data_information, intials_cs_list, intials_cp_list)

new_cp_list <- update_cp(intials_cp_list, c(1,2), list(c(1,2,5), c(1,6,10)))
cs_table_list <- get_cs_table(data_information, intials_cs_list, new_cp_list)

tab1_res <- organize_cs_table(data_information, cs_table_list, intials_cp_list)

gen_indi_table(tab1_res)


## Group output 여기서 부터 하면 됨.
# Mode
m_indi <- gen_median_table(tab1_res)
m_all  <- gen_median_table_all(tab1_res)
m_comb <- bind_rows(m_indi,m_all) %>% arrange(GCA)

median_table <- gen_median_output(m_comb)

# modal
est_cutscore <- cs_table_list
selected_CP <- new_cp_list

modal_est_cutscore <-
      estCutScore_mode(est_cutscore, data_information)

modal_est_cutscore_all <-
      estCutScore_mode_all(est_cutscore, data_information)

modal_est_cs <- map(modal_est_cutscore, ~ .x$est_cutscore)
modal_est_cp <- map(modal_est_cutscore, ~ .x$cut_point)
modal_selected_cp <- map(modal_est_cutscore, ~ .x$selected_CP)

cs_table_list <- get_cs_table(data_information, modal_est_cs, modal_est_cp)

modal_est_cs_all <- map(modal_est_cutscore_all, ~ .x$est_cutscore)
modal_est_cp_all <- map(modal_est_cutscore_all, ~ .x$cut_point)
modal_selected_cp_all <- map(modal_est_cutscore_all, ~ .x$selected_CP)

tab1$modal_res <-
  map2(modal_est_cs, modal_selected_cp,
    tab1_group_out, data_information) %>%
      map(., ~.x$res) %>% bind_rows() %>%
      select(-OOD)

    tab1$modal_res_all <-
      map2(tab1$modal_est_cs_all, tab1$modal_selected_cp_all,
           tab1_group_out_all, input$WESS, modal = T) %>%
      map(., ~.x$res) %>% bind_rows() %>%
      select(-OOD) %>%
      mutate(Table = 0, .after = GCA)

    tab1$modal_res_com <-
      bind_rows(tab1$modal_res, tab1$modal_res_all) %>%
      arrange(GCA)

  # modal cut score ouput ready
    tab1$modal_table <- gen_indi_table(tab1$modal_res_com)
    tab1$modal_table_all <- gen_indi_table(tab1$modal_res_all)

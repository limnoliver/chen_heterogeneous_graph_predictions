

munge_npy <- function(in_file) {
  site_ids <- c('1435', '1436', '1437', '1438', '1439', '1440', '1441', '1442', '1443', '1444', '1445', '1446', '1447', '1448', '1449', '1450', '1451', '1452', '1453', '1454', '1455', '1456', '1457', '1458', '1459', '1460', '1461', '1462', '1463', '1545', '1546', '1547', '1548', '1549', '1550', '1551', '1552', '1553', '1554', '1555', '1556', '1557', '1558', '1559', '1560', '1561', '1562', '1563', '1564', '1565', '1566', '1571', '1572', '1573', '1574', '1575')
  dates <- seq(as.Date('2006-10-31'), as.Date('2020-03-30'), 1)
  
  dat <- npyLoad(in_file) %>%
    as.data.frame()
  names(dat) <- dates
  
  dat_out <- dat %>%
    mutate(seg_id_nat = site_ids) %>%
    tidyr::pivot_longer(cols = -seg_id_nat, names_to = 'date', values_to = 'temp_deg_c') %>%
    mutate(source = in_file)
  
  return(dat_out)
}
munge_npy_ann <- function(in_file) {
  site_ids <- c('1435', '1436', '1437', '1438', '1439', '1440', '1441', '1442', '1443', '1444', '1445', '1446', '1447', '1448', '1449', '1450', '1451', '1452', '1453', '1454', '1455', '1456', '1457', '1458', '1459', '1460', '1461', '1462', '1463', '1545', '1546', '1547', '1548', '1549', '1550', '1551', '1552', '1553', '1554', '1555', '1556', '1557', '1558', '1559', '1560', '1561', '1562', '1563', '1564', '1565', '1566', '1571', '1572', '1573', '1574', '1575')
  dates <- seq(as.Date('2006-10-31'), as.Date('2020-03-30'), 1)

  dat <- npyLoad(in_file) %>%
    matrix(nrow = 56, ncol = 4900, byrow = TRUE) %>%
    as.data.frame()
  
  names(dat) <- dates
  
  dat_out <- dat %>%
    mutate(seg_id_nat = site_ids) %>%
    tidyr::pivot_longer(cols = -seg_id_nat, names_to = 'date', values_to = 'temp_deg_c') %>%
    mutate(source = in_file)
    
  return(dat_out)
}

gather_core_data <- function(out_file) {
  core_model_files <- list.files('in_data/USGS_outputs', full.names = TRUE)[grepl('\\.npy', list.files('in_data/USGS_outputs'))][-1]
  core_data <- core_model_files %>%
    purrr::map_dfr(munge_npy)
  
  ann_data <- munge_npy_ann('in_data/USGS_outputs/ANN_output.npy')
  
  out_data <- bind_rows(core_data, ann_data) %>%
    mutate(model = gsub('_.*', '', basename(source)),
           state_update = case_when(
             grepl('assimilation', source) ~ 'invertible network',
             grepl('KF', source) ~ 'Kalman filter',
             TRUE ~ 'NA'),
           site_withhold = grepl('hidingX', source)) %>%
    select(-source)
  
  write_csv(out_data, out_file)
  
    
}

gather_window_exp <- function(out_file) {
  window_exp_files <- list.files('in_data/USGS_outputs/window size', full.names = TRUE)
  window_data <- window_exp_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(window = stringr::str_extract_all(source, '\\d+', simplify = TRUE)) %>%
    select(-source)
  
  write_csv(window_data, out_file)
}

gather_assimilation_exp <- function(out_file) {
  assimilation_files <- list.files('in_data/USGS_outputs/assimilation data', full.names = TRUE)
  assimilation_data <- assimilation_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(prop_obs = stringr::str_extract_all(source, '\\d+', simplify = TRUE)) %>%
    mutate(prop_obs = as.numeric(prop_obs)/1000) %>%
    select(-source)
  
  write_csv(assimilation_data, out_file)
    
}

gather_pretraining_exp <- function(out_file){
  pretrain_exp_files <- list.files('in_data/USGS_outputs/pretrained HRGN', full.names = TRUE)
  pretrain_data <- pretrain_exp_files %>%
    purrr::map_dfr(munge_npy) %>%
    mutate(pretraining = grepl('_pretrain_', source),
           state_update = ifelse(grepl('assimilation', source), 'invertible network', NA),
           prop_training = as.numeric(stringr::str_extract_all(source, '\\d+', simplify = TRUE))/1000) %>%
    select(-source)
  
  readr::write_csv(pretrain_data, out_file)
}





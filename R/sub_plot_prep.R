# Apply created factor level lists, text, labels, select columns and convert df to sf

sub_plot_prep <- function(data, main_category){
  # Grab the list of subcategory factor levels for the specified main category 
  # from the large list
  cat_levels <- mc_levels_list[[main_category]]
  
  # Filter the programs belonging to each main category
  prepped_data <- data %>% filter(main_category == {{main_category}}) %>% 
    # select data columns that will be included in the dashboard
    select(program, agency, main_category, count_by_main, sub_category, 
           count_by_sub, intercept, intercept_code, count_by_int, 
           count_by_int_cat, zipcode, count_by_zip, count_by_zip_cat, lng, lat, 
           geometry) %>% 
    group_by(sub_category) %>% 
    mutate(
      # Order the subcategories based on the count of programs
      sub_category = factor(sub_category,
                            levels = rev(cat_levels))
    )
  # Get program counts for each subcategory
  text_labels <-table(prepped_data["sub_category"]) %>% as.data.frame() %>% rename(
    count = Freq
  ) %>% arrange(sub_category) %>%
    arrange(desc(count)) %>%
    # Create labels
    mutate(
      text_label = paste("Need sub-category: ", sub_category, "<br>Program: ", count) 
    )
  # Apply factor levels
  text_labels <- text_labels[match(cat_levels, text_labels[["sub_category"]]),] %>% 
    select(text_label) %>%
    as.vector() %>% unlist() %>% unname()
  
  # Combine text labels with prepped data
  prepped_data <- prepped_data %>% 
    mutate(
      sub_cat_text = factor(sub_category, levels = rev(cat_levels), 
                            labels = rev(text_labels))
    ) %>% sf::st_as_sf()
  
  prepped_data
}
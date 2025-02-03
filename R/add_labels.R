# Add column labels stored in another data frame to a data frame

add_labs <- function(label_df, df){
  labels_named_vec <- setNames(label_df[["label"]], label_df[["name"]]) %>% as.list() 
  result <- apply_labels(df, labels_named_vec)
}
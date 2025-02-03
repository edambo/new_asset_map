match_count <- function(keyword_list, string, negate_list = neg_keywords){
  # Initialize empty lists
  count_list <- c()
  negated_list <- c()
  neg_count_list <- c()
  
  
  for(k in keyword_list){
    # Create negated keywords list
    for(n in negate_list){
      neg_phrase  <- toString(paste(n, k, collapse = " "))
      negated_list <- append(negated_list, neg_phrase)
    }
    # Count presence of keywords and add counts to list
    count <- str_count(tolower(string), k)
    # count_u <- ifelse(count>0, 1,0)
    # count_list <- append(count_list, count_u)
    count_list <- append(count_list, count)
  }
  
  # Count presence of negated keywords and add counts to list
  for(nk in negated_list){
    neg_count <- str_count(tolower(string), nk)
    neg_count_list <- append(neg_count_list, neg_count)
  }
  
  total_count <- sum(count_list)
  total_neg_count <- sum(neg_count_list)
  adjusted_count <- total_count - total_neg_count
  adjusted_count
}
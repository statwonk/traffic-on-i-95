stack_data <- function(given_list = NULL) {
  index <- index_of_matching_data_columns(given_list)
  stacked_data_by_group <- list()
  for(i in 1:length(index)) {
    print(paste("Working on group", i))
    stacked_data_by_group[[i]] <- data.table::as.data.table(
      as.data.frame(
        do.call(
          rbind,
          lapply(if(i == 1) {
            index[i]:(index[i + 1] - 1) } else if(i == 2) {
              index[i]:length(given_list ) } else { stop("Opps, need to refactor indexing!")},
                 function(x) {
                   given_list[[x]]
                 }))))
  }
  return(stacked_data_by_group)
}

index_of_matching_data_columns <- function(x) {
  suppressWarnings({
    group_starting_points <- list()
    group_starting_points[[1]] <- 1
    sets <- data.frame(matches = rep(NA, length(x)))
    index <- 1:length(x)
    for(i in index) {
      sets$matches[i] <- all(names(x[[1]]) == names(x[[i]]))
    }
    # Now which is the first non-matching set?
    group_starting_points[[2]] <- index[-which(sets$matches)][[1]]
    as.vector(do.call(rbind, group_starting_points))
  })
}

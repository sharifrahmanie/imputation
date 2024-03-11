require(DMwR)
require(DescTools)
# By @biomedical_informatics

imputation <- function(data, 
                       thresh) {
  mode_imputation <- function(data) {
    for (i in 1:ncol(data)) {
      col_mode <- Mode(na.omit(data[, i]))
      data[is.na(data[, i]), i] <- col_mode
    }
    data <- as.data.frame(lapply(data,
                                 as.factor))
    return(data)
  }
  data[data == ""] <- "NA"
  na_per <- colMeans(is.na(data)) * 100
  cl_gr_thresh <- names(na_per[na_per > thresh])
  filter_df <- data[, !names(data) %in% cl_gr_thresh]
  
  dftype <- sapply(filter_df, class)
  isint <- grep("integer|numeric", dftype)
  ischar <- grep("character|factor|logical", dftype)
  list_nint <- colnames(filter_df[isint])
  list_nachar <- colnames(filter_df[ischar])
  imputed_factor <- mode_imputation(data = filter_df[list_nachar])
  imputed_integer <- knnImputation(filter_df[list_nint],
                                   k = 5,
                                   meth = 'median',
                                   scale = FALSE)
  imputed <- cbind(imputed_factor, imputed_integer)
  imputed <- imputed[,colnames(filter_df)]
  return(imputed)
}
load("clinical.RData")
my_clinic <- imputation(clinical,
                        50)

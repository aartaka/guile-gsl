(define-module (gsl stat)
  #:use-module (gsl utils)
  #:use-module (system foreign)
  #:export (mean
            variance
            standard-deviation))

(define* (mean data size #:key (stride 1))
  "This function returns the arithmetic mean of DATA, a dataset of SIZE with STRIDE."
  ((foreign-fn "gsl_stats_mean" `(* ,size_t ,size_t) double)
   data stride size))

(define* (variance data size #:key (stride 1) (mean #f))
  "This function returns the estimated, or sample, variance of DATA, a dataset of SIZE with STRIDE."
  (if mean
      ((foreign-fn "gsl_stats_variance_m" `(* ,size_t ,size_t ,double) double)
       data stride size mean)
      ((foreign-fn "gsl_stats_variance" `(* ,size_t ,size_t) double)
       data stride size)))

(define* (standard-deviation data size #:key (stride 1) (mean #f))
  "Standard deviation (sqrt of variance) for SIZEd DATA with STRIDE and MEAN."
  (if mean
      ((foreign-fn "gsl_stats_sd_m" `(* ,size_t ,size_t ,double) double)
       data stride size mean)
      ((foreign-fn "gsl_stats_sd" `(* ,size_t ,size_t) double)
       data stride size)))

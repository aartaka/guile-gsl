(define-module (gsl stat)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:export (mean
            variance
            standard-deviation))

(define* (mean data size #:key (stride 1))
  ((foreign-fn "gsl_stats_mean" `(* ,size_t ,size_t) double)
   data stride size))

(define* (variance data size #:key (stride 1) (mean #f))
  (if mean
      ((foreign-fn "gsl_stats_variance_m" `(* ,size_t ,size_t ,double) double)
       data stride size mean)
      ((foreign-fn "gsl_stats_variance" `(* ,size_t ,size_t) double)
       data stride size)))

(define* (standard-deviation data size #:key (stride 1) (mean #f))
  (if mean
      ((foreign-fn "gsl_stats_sd_m" `(* ,size_t ,size_t ,double) double)
       data stride size mean)
      ((foreign-fn "gsl_stats_sd" `(* ,size_t ,size_t) double)
       data stride size)))

;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode))
 (makefile-gmake-mode
  (indent-tabs-mode t))
 (scheme-mode
  (fill-column . 80)
  ;; TODO: Font lock define-record-type* and
  ;; define-public-record-type* correctly.
  (eval . (put 'call-with-container* 'scheme-indent-function 2))
  (eval . (put 'call-with-input-pipe 'scheme-indent-function 1))
  (eval . (put 'state-let* 'scheme-indent-function 1))))

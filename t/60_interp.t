; # -*-emacs-lisp-*-
; BEGIN { exec $^X, qw(--emacs --batch --no-site-file -l), $0, "I", @INC }

;;; Tests for `make-perl-interpreter' and related functions.
;;; (Only one test so far.)

(setq INC (cdr (member "I" command-line-args)))
(setq -I (mapcar
	  (lambda (dir) (concat "-I" dir))
	  INC))

(defun reperl (&rest argv)
  (condition-case nil
      (perl-destruct)
    (error t))
  (set-perl-interpreter
   (apply 'make-perl-interpreter "perl"
	  (append (copy-sequence -I) argv))))

(setq tests
      '(
	(reperl "-MEmacs::Lisp" "-e0")
	))

(setq standard-output t)
(princ (format "1..%d\n" (length tests)))
(setq test-number 1)
(mapcar
 (lambda (form)
   (princ (format "%sok %d\n"
		  (if (eval form) "" "not ")
		  test-number))
   (setq test-number (1+ test-number)))
 tests)

;; Don't try to process the rest of the command line.
(kill-emacs)


;; RECORD INTERVALS OF IDLENESS

;; For more detail about the purpose of this code, see
;; https://news.ycombinator.com/item?id=27765330

;; 2018 Jun 15. I deploy version two. Version one was deployed for many months
;; and is now in .retired.

;; Version one used an idle timer, but that design has the deficiency of not
;; being able to detect when emacs _stops_ being idle. We detect that below
;; by polling the return value of current-idle-time.

;; 2021 Jul 09. License: I am the sole author of this file and I hereby release
;; it into the public domain. Richard Hollerith <hruvulum@gmail.com>. I welcome
;; bug reports on it.

;; Do the right thing if this file is loaded more than once:

(dolist (timer timer-list) (when (eq 'record-idle (timer--function timer))
     (cancel-timer timer)))
(unless 

     ;; the log file would get very confusing if multiple emacs processes wrote
     ;; to it. (in fact, we might consider writing the PID of the emacs to the
     ;; log file.)

     (getenv "TEST55") 
     (run-at-time 

          ;; now:

          nil 3 'record-idle))

;; Only one function (namely, RECORD-IDLE) refers to these next 2 global
;; variables (so if Emacs Lisp had objects, these next 2 could be members of an
;; object instead).

(defvar gvcit nil)
(defvar gvnow nil)
(defun record-idle ()
     (let (getcit getnow getstart cit data entry)

          ;; It is unusual style to have both GETCIT and GVCIT. It makes more
          ;; sense when you understand that GVCIT is analogous to an IORef in
          ;; Haskell and GETCIT is analogous to the contents of that IORef at a
          ;; particular instant.

          (setq getcit gvcit)
          (setq getnow gvnow)
          (setq getstart (and getcit getnow (time-subtract getnow getcit)))
          (setq cit (current-idle-time))

          ;; The function being defined maintains 3 log files: one named
          ;; (concat "0" "idle") with a resolution of 1 minute (60 seconds),
          ;; one named (concat "" "idle") with a res of 7 minutes (420 seconds)
          ;; and one named "2idle" with a res of 7 * 7 minutes.

          (setq data '((60 "0") (420 "") (2940 "2")))
          (dolist (datum data) (setq resolution (car datum)) (when 
               (and getcit getnow 

                    ;; GETCIT is long enough to be interesting:

                    (time-less-p (seconds-to-time resolution) getcit)

                    ;; The interval whose length is GETCIT has ended:

                    (or (not cit) (time-less-p cit getcit)))
               
               ;; Record the fact that a period of idleness lasting at least
               ;; RESOLUTION seconds has ended.

               (setq entry
                    
                    ;; start. <duration> end.

                    (format "%s. <%s> %s.\n\n"
                         (format-time-string "%H:%M:%S" getstart)
                         (format-time-string "%H:%M:%S" getcit
                              
                              ;; Since it is the only sensible choice for
                              ;; formatting a relative time, use universal
                              ;; time:
                              
                              t)
                         (format-time-string "%H:%M:%S" getnow)
                         . nil))
               (write-region entry nil
                    (expand-file-name (format "%sidle" (cadr datum))
                         (expand-file-name "~/"))
                    
                    ;; append:
               
                    t 'quiet)))
          (setq gvnow (current-time)) 
          (setq gvcit cit)
          nil))

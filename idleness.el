
;; RECORD INTERVALS OF IDLENESS

;; For a description of the purpose of this code, see
;; https://news.ycombinator.com/item?id=27765330

;; Usage: arrange for this file to be loaded when Emacs starts, which will
;; cause 3 log files to be maintained (to be written to now and then). A line
;; of one of the 3 log files consists of 3 time stamps: the start of an
;; interval of idleness, its duration (in angle brackets), and its end. For the
;; names of the 3 log files, see below where it says `setq data`.

;; Whenever my computer is running, an Emacs process is running. Furthermore,
;; Emacs is how I interact with my Unix shell and how I submit queries to
;; engines that search the Web. (I don't use my browser's location bar for
;; that.) If you do not use Emacs for as much as I do, you might not benefit
;; from this code.

;; 2018 Jun 15. I deploy version two. Version one was deployed for many months
;; and is now in .retired. 2021 Jul 09. I publish this file: in particular, I
;; make a copy of this file to /d/pages_my, then I `git push` to Github.

;; Version one used an idle timer, but that design has the deficiency of not
;; being able to detect when emacs _stops_ being idle. We detect that below
;; by polling the return value of current-idle-time.

;; 2021 Jul 09. License: I hereby release this file into the public domain. I
;; welcome bug reports on it. Richard Hollerith <hruvulum@gmail.com>.

;; Do the right thing if this file is loaded more than once:

(dolist (timer timer-list) (when (eq 'record-idle (timer--function timer))
     (cancel-timer timer)))
(unless 

     ;; The log file would get very confusing if multiple Emacs processes wrote
     ;; to it. (In fact, we might consider writing the PID of the Emacs to the
     ;; log file.) Most of the time when I have multiple Emacsen running, it is
     ;; because I am testing new code before running the new code in my main
     ;; Emacs process. In that case, the command that starts the test Emacs
     ;; arranges the test Emacs's environment so that the variable TEST55 is
     ;; set.

     (getenv "TEST55") 
     (run-at-time 

          ;; now:

          nil 3 'record-idle))

;; Only the function defined in this file ever refers to these next 2 global
;; variables (so if Emacs Lisp had objects, these next 2 could be members of an
;; object instead).

(defvar gvcit nil)
(defvar gvnow nil)
(defun record-idle ()
     (let (data output-directory getcit getnow getstart cit resolution entry)

          ;; The function being defined maintains 3 log files: one named
          ;; (concat "0" "idle") with a resolution of 1 minute (60 seconds),
          ;; one named (concat "" "idle") with a res of 7 minutes (420 seconds)
          ;; and one named "2idle" with a res of 7 * 7 minutes.

          (setq data '((60 "0") (420 "") (2940 "2")))
          (setq output-directory (if 

               ;; The user is Richard, not someone using Richard's file of
               ;; code:

               (file-directory-p "/l")
               (expand-file-name "/g/")
               (expand-file-name "~/")))
          
          ;; It is unusual style to have both GETCIT and GVCIT. It makes more
          ;; sense when you understand that GVCIT is analogous to an IORef in
          ;; Haskell and GETCIT is analogous to the contents of that IORef at a
          ;; particular instant.

          (setq getcit gvcit)
          (setq getnow gvnow)
          (setq getstart (and getcit getnow (time-subtract getnow getcit)))
          (setq cit (current-idle-time))
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
                         (format-time-string "%Y %b %d %H:%M:%S" getstart)
                         (format-time-string "%H:%M:%S" getcit
                              
                              ;; Since it is the only sensible choice for
                              ;; formatting a relative time, use universal
                              ;; time:
                              
                              t)
                         (format-time-string "%H:%M:%S" getnow)
                         . nil))
               (write-region entry nil
                    (expand-file-name (format "%sidle" (cadr datum)) 
                         output-directory)
                    
                    ;; append:
               
                    t 'quiet)))
          (setq gvnow (current-time)) 
          (setq gvcit cit)
          nil))

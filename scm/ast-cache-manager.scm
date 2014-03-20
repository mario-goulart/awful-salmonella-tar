(module cache-manager ()

(import chicken scheme)

(use data-structures files posix srfi-1)

(define (manage-cache! cache-dir awful-pid max-items)
  ;; Sort files by modification time (older first) and remove
  ;; the oldest ones, keeping the max-items recent ones
  (let* ((all-files
          (sort (find-files cache-dir
                            test: (lambda (f) (not (directory? f))))
                (lambda (f1 f2)
                  (> (file-modification-time f1)
                     (file-modification-time f2)))))
         (length-all-files (length all-files))
         (to-delete
          (if (< length-all-files max-items)
              '()
              (drop-right all-files max-items))))

    ;; Stop awful
    (process-signal awful-pid signal/stop)

    ;; Delete oldest files
    (for-each delete-file* to-delete)

    ;; Delete empty directories
    (for-each delete-directory
              (find-files cache-dir
                          test: (lambda (f)
                                  (and (directory? f)
                                       (null? (directory f))))))

    ;; Resume awful
    (process-signal awful-pid signal/cont)

    ;; Print some statistics
    (print
     (if (null? to-delete)
         "Nothing to delete."
         (sprintf "Deleted ~a files." (length to-delete))))))

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage:
    #this [ -h | --help ]
    #this <cache dir> <awful pid> [ <max cache items> ]

EOF
))
  (when exit-code
    (exit exit-code)))


(let ((args (command-line-arguments)))
  (when (or (null? args)
            (null? (cdr args)))
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((cache-dir (car args))
        (awful-pid (string->number (cadr args)))
        (max-items (and (not (null? (cddr args)))
                        (string->number (caddr args)))))
    (assert awful-pid)
    (manage-cache! cache-dir awful-pid max-items)))

) ;; end module

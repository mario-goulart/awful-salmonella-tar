;;; TODO
;; * manage cache size
;; * handle "incomplete" request paths (e.g., /master/gcc)

;; Core units
(use data-structures extras files irregex posix srfi-1 srfi-13 utils)

;; Eggs
(use awful intarweb spiffy)


(define cache-dir (make-parameter "cache"))

(define salmonella-reports-dir (make-parameter "reports"))

(define report-compressor
  ;; #f specifies no compression (plain tar file)
  (make-parameter 'gzip
                  (lambda (v)
                    (unless (or (not v)
                                (member v '(gzip bzip2)))
                      (error 'report-compressor "Unsupported compressor" v)))))

(define (path-join parts)
  (string-intersperse parts "/"))

(define (path-split path)
  (string-split path "/"))

(define (requested-path->tar-file requested-file-path)
  (define (tar-file-path path-parts)
    (make-pathname (list (root-path)
                         (salmonella-reports-dir)
                         (path-join path-parts))
                   "salmonella-report.tar"))
  (let ((path-parts (path-split requested-file-path)))
    (if (or (null? path-parts)
            (null? (cdr path-parts)))
        #f
        (let ((dir-part (last (butlast path-parts))))
          (case (string->symbol dir-part)
            ((salmonella-report)
             (tar-file-path (butlast path-parts)))
            ((dep-graphs install ranks rev-dep-graphs test)
             (tar-file-path (drop-right path-parts 3)))
            (else #f))))))

(define (split-by-salmonella-report requested-file-path)
  ;; Given /<branch>/<c-compiler>/<os>/<arch>/<yyyy>/<mm>/<dd>/salmonella-report/<report-path>,
  ;; return (/<branch>/<c-compiler>/<os>/<arch>/<yyyy>/<mm>/<dd> . salmonella-report/<report-path>)
  (let ((parts (path-split requested-file-path)))
    (let-values (((pre post) (split-at parts
                                       (list-index (lambda (elt)
                                                     (equal? elt "salmonella-report"))
                                                   parts))))
      (cons (path-join pre)
            (path-join post)))))

(define (tar-get requested-file-path)
  (let* ((tar-file (requested-path->tar-file requested-file-path))
         (pre/post (split-by-salmonella-report requested-file-path))
         (pre (car pre/post))
         (post (cdr pre/post)))
    (and (file-exists? tar-file)
         (handle-exceptions exn
           #f
           (let* ((out-dir (make-pathname (cache-dir) pre))
                  ;; Ugly.  Maybe use some tar implementation in
                  ;; scheme (e.g., snowtar, or port the tar egg to
                  ;; chicken 4)
                  (cmd (sprintf "tar x~af ~a -C ~a ~a"
                                (case (report-compressor)
                                  ((gzip) "z")
                                  ((bzip2) "j")
                                  (else ""))
                                tar-file
                                out-dir
                                (qs post))))
             (create-directory out-dir 'with-parents)
             (system* cmd)
             (make-pathname out-dir post))))))

;; As I understand it, configuring mime-type-map shouldn't be
;; necessary.  However, it seems that (content-type #(text/html
;; ((charset . utf-8)))) is not being set by with-headers in
;; send-gzipped-file
(mime-type-map
 (append
  (mime-type-map)
  '(("logz"  . text/plain)
    ("svg"   . image/svg+xml)
    ("svgz"  . image/svg+xml)
    ("htmlz" . text/html))))

(define (send-gzipped-file file)
  (if (memq 'gzip (header-values 'accept-encoding
                                 (request-headers (current-request))))
        (with-headers '((content-type #(text/html ((charset . utf-8))))
                        (content-encoding gzip))
           (lambda ()
             (send-static-file file)))
      (send-response
       code: 406
       body: "<h1>406 - Only gzip-compressed content is available</h1>")))

(define (send-file file)
  (if (string-suffix? "z" file)
      (send-gzipped-file file)
      (send-static-file file)))


(define (awful-salmonella-tar base-path #!key (awful-settings (lambda (_) (_))))

  (define base-path-pattern
    (irregex (string-append (string-chomp base-path "/") "(/.*)*")))

  (define-app awful-salmonella-tar
    matcher: (lambda (path)
               (irregex-match base-path-pattern path))
    handler-hook: (lambda (handler)
                    (parameterize ((enable-sxml #t)
                                   (app-root-path base-path))
                      (awful-settings handler)))

    (create-directory (cache-dir) 'with-parents)

    (define-page (irregex (string-append base-path ".*"))
      (lambda (req-path)
        (lambda ()
          (let ((not-found (lambda ()
                             (send-status 'not-found))))
            (handle-exceptions exn
              (not-found)
              (cond ((tar-get req-path)
                     => (lambda (file)
                          (parameterize ((root-path (pathname-directory file)))
                            (let ((filename (pathname-strip-directory file)))
                              (send-file filename)))))
                    (else
                     (if (file-exists? (make-pathname (list (root-path)
                                                            (salmonella-reports-dir))
                                                      req-path))
                         (parameterize ((root-path (salmonella-reports-dir)))
                           (send-file req-path))
                         (not-found)))))))))
    ) ;; end awful-salmonella-tar
  ) ;; end module

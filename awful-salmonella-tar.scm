(module awful-salmonella-tar

  (awful-salmonella-tar
   cache-dir
   salmonella-reports-dir
   salmonella-report-dir
   report-tar-filename
   report-compressor
   report-tar-contains-compressed-files?)

(import chicken scheme)
(include "scm/awful-salmonella-tar.scm")
)

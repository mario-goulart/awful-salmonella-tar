(cond-expand
  (chicken-4
   (use awful-salmonella-tar))
  (chicken-5
   (import awful-salmonella-tar))
  (else
   (error "Unsupported CHICKEN version.")))

(awful-salmonella-tar "/")

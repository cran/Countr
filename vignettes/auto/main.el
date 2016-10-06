(TeX-add-style-hook
 "main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("jss" "article")))
   (TeX-run-style-hooks
    "latex2e"
    "jss"
    "jss10"
    "amsmath"
    "amsfonts"
    "xspace")
   (TeX-add-symbols
    '("rom" 1)
    "eg"
    "ie"
    "dee"
    "countdist")
   (LaTeX-add-labels
    "sec:models"
    "eq:rela"
    "eq:conv"
    "eq:generalSpec"
    "eq:linkFct"
    "sec:design"
    "sec:fitting-models"
    "sec:spec-distr"
    "sec:spec-covar"
    "sec:fitting-models-1"
    "ssec:InitialVals"
    "ssec:custom-dist"
    "sec:function-renewal"
    "sec:closingRem"
    "ssec:ProbabilityComp"
    "sec:cl")
   (LaTeX-add-bibliographies
    "REFERENCES")))


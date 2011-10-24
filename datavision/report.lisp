(defstruct report
  (name (i18n "Unnamed Report"))
  (title (i18n "Default Report Title"))
  author
  description
  start-formula
  data-source
  formulas
  parameters
  usercols
  headers
  footers
  page-headers
  page-footers
  groups
  details
  rset
  layout-engine
  paper-format
  aggregate-fields
 )

(defun run-report (report)
  "Run a report, using its layout engine to format output."
  (let ((layout-engine (report-layout-engine report)))
    (ask-for-data-source-file (report-data-source report))
    (ask-for-parameters (report-parameters report))

    (apply #'group-reset (report-groups report))
    (collect-aggregate-fields report)

    (if (not (nil-p (report-start-formula report)))
	(formula-eval (report-start-formula report)))

    (apply #'formula-use-cache (report-formulas report))
    (report-reset-formulas report)

    ;; Here we go
    (unless (wants-more-data (report-layout-engine report))
      return-from run)
    (let ((rset (execute (report-data-source report)))
	  (layout-started nil))
	  (while (wants-more-data layout-engine)
	    (unless (layout-engine-started layout-engine)
	      (layout-engine-start layout-engine)
	      (setf layout-started t))
	    (process-result-row))
	  )))

(defun ask-for-data-source-file (data-source))

(defun ask-for-parameters (parameters))

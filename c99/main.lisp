;;
;;  Test code LR(1), LALR(1).
;;
(load #p"parser.lisp")

;;
;;  Make Shift table -> save-table.lisp
;;
(read-parse #p"c99.lisp")
(make-table)
(save-table #p"save-table.lisp")


;;
;;  Make LR(1) -> save-lr.lisp
;;
;(load-table #p"save-table.lisp")
(make-reduce)
(save-table #p"save-lr.lisp")


;;
;;  Make LALR(1) -> save-lalr.lisp
;;
;(load-table #p"save-lr.lisp")
(make-lalr)
(save-table #p"save-lalr.lisp")


;;
;;  parse
;;
;(load-table #p"save-lalr.lisp")
(let ((*print-pretty* nil))
  (execute-parse '(int identifier #\( #\)
                       #\{
                       return constant #\;
                       #\})))


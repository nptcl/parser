;;
;;  Test code LR(1), LALR(1).
;;
(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it ,then ,else)))

(defmacro awhen (expr &body body)
  `(aif ,expr (progn ,@body)))

(defmacro aprog1 (expr &body body)
  `(let ((it ,expr))
     (prog1 it ,@body)))

(defmacro aif2 (expr then &optional else)
  (let ((g (gensym)))
    `(multiple-value-bind (it ,g) ,expr
       (if ,g ,then ,else))))

(defmacro awhen2 (expr &body body)
  `(aif2 ,expr (progn ,@body)))

(defmacro dobind (bind expr &body body)
  (let ((g (gensym)))
    `(dolist (,g ,expr)
       (destructuring-bind ,bind ,g
         ,@body))))


;;
;;  Stack
;;
(defvar *stack* nil)

(defun push-stack (x)
  (push x *stack*))

(defun pop-stack ()
  (unless *stack*
    (error "pop-stack error"))
  (pop *stack*))

(defun top-stack ()
  (unless *stack*
    (error "top-stack error"))
  (car *stack*))

(defun init-stack ()
  (setq *stack* nil))


;;
;;  Input
;;
(defvar *end* (make-symbol "END"))
(defvar *input*)

(defun set-input (x)
  (setq *input* x))

(defun pop-input ()
  (if *input*
    (pop *input*)
    *end*))

(defun top-input ()
  (if *input*
    (car *input*)
    *end*))


;;
;;  Define
;;

;;  terminal
(defvar *terminal* nil)

(defun read-terminal (cdr)
  (dolist (x cdr)
    (pushnew x *terminal*)))

(defmacro parse-terminal (&rest args)
  `(read-terminal ',args))

(defun terminalp (x)
  (and (member x *terminal*) t))


;;  rule
(defstruct rule index left right accept)
(defvar *rule-index* 0)
(defvar *rule* nil)

(defun rule-instance (left right &optional accept)
  (prog1 (make-rule :index *rule-index* :left left :right right :accept accept)
    (incf *rule-index*)))

(defun read-rule (cdr)
  (destructuring-bind (left check . right) cdr
    (unless (eq check '->)
      (error "Invalid rule, ~S." cdr))
    (unless right
      (error "right value error, ~S." cdr))
    (push (rule-instance left right) *rule*)))

(defmacro parse-rule (&rest args)
  `(read-rule ',args))

(defun non-terminal-p (x)
  (and (member x *rule* :key #'rule-left) t))


;;  start
(defvar *start-symbol* (make-symbol "START"))
(defvar *start* nil)

(defun read-start (cdr)
  (destructuring-bind (start) cdr
    (when *start*
      (error "start already exist, ~S." *start*))
    (push (rule-instance *start-symbol* (list start) t) *rule*)
    (setq *start* start)))

(defmacro parse-start (&rest args)
  `(read-start ',args))


;;
;;  Shift / Reduce
;;
(defun shift-test ()
  (push-stack
    (pop-input)))

(defun reduce-call (left right)
  (let (list)
    (dolist (ignore right)
      (push (pop-stack) list))
    (unless (equal right list)
      (error "reduce error, ~S /= ~S." right list)))
  (push-stack left))

(defun reduce-test (x)
  (destructuring-bind (left check . right) x
    (unless (and (eq check '->))
      (error "operator error, ~S." x))
    (unless right
      (error "right value error, ~S." x))
    (reduce-call left right)))


;;
;;  Test
;;
(defun output-stack-input ()
  (format t "~S~30T~S~%" (reverse *stack*) *input*))

(defun shift* ()
  (shift-test)
  (output-stack-input))

(defun reduce* (x)
  (reduce-test x)
  (output-stack-input))

(defun main-test ()
  (set-input '(int * [ int + int ]))
  (output-stack-input)

  (shift*)
  (reduce* '(fact -> int))
  (reduce* '(term -> fact))
  (shift*)
  (shift*)
  (shift*)
  (reduce* '(fact -> int))
  (reduce* '(term -> fact))
  (reduce* '(expr -> term))
  (shift*)
  (shift*)
  (reduce* '(fact -> int))
  (reduce* '(term -> fact))
  (reduce* '(expr -> expr + term))
  (shift*)
  (reduce* '(fact -> [ expr ]))
  (reduce* '(term -> term * fact))
  (reduce* '(expr -> term))
  )


;;
;;  FIRST(X)
;;
(declaim (ftype function first-symbol))

(defun first-right (x right)
  (dolist (y right)
    (when (eql x y)
      (return nil))
    (aif (first-symbol y)
      (return it))))

(defun first-rule (x)
  (let (list)
    (dolist (y *rule*)
      (when (eql x (rule-left y))
        (push (rule-right y) list)))
    list))

(defun first-nonterm (x)
  (let (root)
    (dolist (right (first-rule x))
      (dolist (y (first-right x right))
        (pushnew y root)))
    root))

(defun first-symbol (x)
  (cond ((terminalp x) (list x))
        ((non-terminal-p x) (first-nonterm x))
        (t (error "Invalid symbol, ~S." x))))

(defun first-parse (list)
  (when list
    (destructuring-bind (car . cdr) list
      (or (first-symbol car)
          (first-parse cdr)))))


;;
;;  CLOSURE(I)
;;    repeat
;;      foreach ([A -> alpha . B beta, term-a] in I)
;;        foreach ([B -> gamma] in *rule*)
;;          foreach (term-b in FIRST(beta term-a))
;;            add [B -> . gamma, term-b] to I;
;;    until no more items are added to I;
;;    return I;
;;
(defstruct grammar left alpha beta ahead)

(defun grammar-instance (left alpha beta ahead)
  (make-grammar :left left :alpha alpha :beta beta :ahead ahead))

(defun grammar-instance-rest (left alpha beta &rest ahead)
  (grammar-instance left alpha beta ahead))

(defun closure-filter (list)
  (let (root)
    (dolist (inst list)
      (awhen (grammar-beta inst)
        (destructuring-bind (b . beta) it
          (when (non-terminal-p b)
            (dolist (ahead (grammar-ahead inst))
              (push (list b beta ahead) root))))))
    root))

(defun closure-grammar (b)
  (let (root)
    (dolist (inst *rule*)
      (when (eql (rule-left inst) b)
        (push (rule-right inst) root)))
    root))

(defun closure-first (beta term)
  (or (first-parse beta)
      (list term)))

(defun closure-add-find (left beta list)
  (dolist (inst list)
    (and (equal (grammar-left inst) left)
         (equal (grammar-alpha inst) nil)
         (equal (grammar-beta inst) beta)
         (return inst))))

(defun closure-add-ahead (inst term list)
  (unless (member term (grammar-ahead inst))
    (push term (grammar-ahead inst))
    (values list t)))

(defun closure-add-list (left beta term list)
  (let ((v (grammar-instance-rest left nil beta term)))
    (values (cons v list) t)))

(defun closure-add (left beta term list)
  (aif (closure-add-find left beta list)
    (closure-add-ahead it term list)
    (closure-add-list left beta term list)))

(defun closure-foreach (list)
  (let (update)
    (dobind (b beta term-a) (closure-filter list)
      (dolist (gamma (closure-grammar b))
        (dolist (term-b (closure-first beta term-a))
          (awhen2 (closure-add b gamma term-b list)
            (setq list it update t)))))
    (values list update)))

(defun closure-parse (list)
  (aif2 (closure-foreach list)
    (closure-parse it)
    list))


;;
;;  GOTO(I, X)
;;    J = ();
;;    foreach ([A -> alpha . X beta, a] in I)
;;        add  [A -> alpha X . beta, a] to J;
;;    return CLOSURE(J);
;;
(defun goto-next (g x)
  (awhen (grammar-beta g)
    (destructuring-bind (y . beta) it
      (when (eql x y)
        (let ((left (grammar-left g))
              (alpha (grammar-alpha g))
              (ahead (grammar-ahead g)))
          (setq alpha (append alpha (list y)))
          (grammar-instance left alpha beta ahead))))))

(defun goto-parse (list x)
  (let (root)
    (dolist (g list)
      (awhen (goto-next g x)
        (push it root)))
    (closure-parse root)))


;;
;;  equalset
;;
(defun equalset-find (x right test)
  (dolist (y right)
    (when (funcall test x y)
      (return t))))

(defun equalset-call (left right test)
  (dolist (x left t)
    (unless (equalset-find x right test)
      (return nil))))

(defun equalset (left right &key (test #'eql))
  (and (listp left)
       (listp right)
       (= (length left) (length right))
       (equalset-call left right test)))


;;
;;  State
;;
(defstruct state index list action goto)
(defvar *state-index* 0)
(defvar *state* nil)

(defun get-state (index &optional (errorp t))
  (or (find index *state* :key #'state-index)
      (when errorp
        (error "Index error, ~S." index))))

(defun grammar-equal (x y)
  (and (equal (grammar-left x) (grammar-left y))
       (equal (grammar-alpha x) (grammar-alpha y))
       (equal (grammar-beta x) (grammar-beta y))
       (equalset (grammar-ahead x) (grammar-ahead y))))

(defun intern-find-state (list)
  (dolist (y *state*)
    (when (equalset list (state-list y) :test #'grammar-equal)
      (return y))))

(defun intern-make-state (list)
  (aprog1 (make-state :index *state-index* :list list)
    (push it *state*)
    (incf *state-index*)))

(defun intern-state (list)
  (or (intern-find-state list)
      (values (intern-make-state list) t)))


;;
;;  Table
;;
(defun start-table ()
  (let* ((left *start-symbol*)
         (beta (list *start*))
         (ahead *end*)
         (x (grammar-instance-rest left nil beta ahead)))
    (intern-state
      (closure-parse (list x)))))

(defun symbols-table (x)
  (let (root)
    (dolist (g (state-list x))
      (awhen (grammar-beta g)
        (pushnew (car it) root)))
    root))

(defun goto-table (x s)
  (intern-state
    (goto-parse (state-list x) s)))

(defun state-action-check-p (y z b c)
  (and (eq b y)
       (or (eql c z)
           (and (rule-p c)
                (rule-p z)
                (eql (rule-index c)
                     (rule-index z))))))

(defun state-action-check (list a b c)
  (destructuring-bind (y z) (cdr list)
    (unless (state-action-check-p y z b c)
      (error "~S/~S error, ~S, ~S." y b list (list a b c)))))

(defun state-goto-check (list a b)
  (destructuring-bind (y) (cdr list)
    (unless (eql b y)
      (error "goto error, ~S /= ~S." list (list a b)))))

(defun add-shift-table (s x next)
  (aif (find s (state-action x) :key #'car)
    (state-action-check it s 's next)
    (push (list s 's next) (state-action x))))

(defun add-goto-table (s x next)
  (aif (find s (state-goto x) :key #'car)
    (state-goto-check it s next)
    (push (list s next) (state-goto x))))

(defun arrow-table (s x y)
  (let ((next (state-index y)))
    (cond ((terminalp s) (add-shift-table s x next))
          ((non-terminal-p s) (add-goto-table s x next))
          (t (error "Invalid symbol, ~S." s)))))

(defun make-table-loop (x)
  (dolist (s (symbols-table x))
    (multiple-value-bind (y make) (goto-table x s)
      (arrow-table s x y)
      (when make
        (make-table-loop y)))))

(defun make-table ()
  (make-table-loop
    (start-table)))


;;
;;  Output
;;
(defun output-action-table (x)
  (destructuring-bind (a b c) x
    (if (rule-p c)
      (format nil "~A:~A:~A" a b (rule-index c))
      (format nil "~A:~A:~A" a b c))))

(defun output-goto-table (x)
  (destructuring-bind (a b) x
    (format nil "~A:~A" a b)))

(defun output-table ()
  (dotimes (index *state-index*)
    (awhen (get-state index nil)
      (let ((a (mapcar #'output-action-table (state-action it)))
            (g (mapcar #'output-goto-table (state-goto it))))
        (format t "~3A:~{ ~A~}~40T~{ ~A~}~%" index a g)))))


;;
;;  Reduce
;;
(defun find-reduce (g)
  (let ((left (grammar-left g))
        (alpha (grammar-alpha g)))
    (dolist (rule *rule*)
      (and (equal (rule-left rule) left)
           (equal (rule-right rule) alpha)
           (return rule)))))

(defun add-final-table (s x)
  (aif (find s (state-action x) :key #'car)
    (state-action-check it s 'a nil)
    (push (list s 'a nil) (state-action x))))

(defun add-reduce-table (s x rule)
  (aif (find s (state-action x) :key #'car)
    (state-action-check it s 'r rule)
    (push (list s 'r rule) (state-action x))))

(defun add-reduce (x g rule)
  (dolist (s (grammar-ahead g))
    (if (rule-accept rule)
      (add-final-table s x)
      (add-reduce-table s x rule))))

(defun make-reduce ()
  (dolist (x *state*)
    (dolist (g (state-list x))
      (awhen (find-reduce g)
        (add-reduce x g it)))))


;;
;;  Transit
;;
(defvar *transit* nil)

(defun push-transit (x)
  (declare (type unsigned-byte x))
  (push x *transit*))

(defun pop-transit ()
  (unless *transit*
    (error "pop-transit error."))
  (pop *transit*))

(defun top-transit ()
  (unless *transit*
    (error "top-transit error."))
  (car *transit*))

(defun init-transit ()
  (setq *transit* nil)
  (push-transit 0))


;;
;;  Execute
;;
(defun find-action-state (v state)
  (dolist (x (state-action state))
    (destructuring-bind (a b c) x
      (when (eql a v)
        (return (values b c))))))

(defun find-goto-state (v state)
  (dolist (x (state-goto state))
    (destructuring-bind (a b) x
      (when (eql a v)
        (return b)))))

(defun shift-execute (next)
  (shift-test)
  (push-transit next))

(defun goto-execute ()
  (let* ((index (top-transit))
         (state (get-state index))
         (sym (top-stack)))
    (aif (find-goto-state sym state)
      (push-transit it)
      (error "find-goto-state error, ~S, ~S." index sym))))

(defun reduce-execute (rule)
  (let ((left (rule-left rule))
        (right (rule-right rule)))
    (reduce-call left right)
    (dolist (ignore right)
      (pop-transit))
    (goto-execute)))

(defun accept-execute ()
  (format t "ACCEPT~%"))

(defun call-execute ()
  (output-stack-input)
  (let* ((index (top-transit))
         (state (get-state index))
         (ahead (top-input)))
    (multiple-value-bind (type next) (find-action-state ahead state)
      (ecase type
        (s (shift-execute next)
           (call-execute))
        (r (reduce-execute next)
           (call-execute))
        (a (accept-execute))))))

(defun execute-parse (x)
  (init-stack)
  (init-transit)
  (set-input x)
  (call-execute))


;;
;;  LALR(1)
;;
(defun equal-grammar-lalr (x y)
  (and (equal (grammar-left x) (grammar-left y))
       (equal (grammar-alpha x) (grammar-alpha y))
       (equal (grammar-beta x) (grammar-beta y))))

(defun equal-state-lalr (x y)
  (let ((x (state-list x))
        (y (state-list y)))
    (equalset x y :test #'equal-grammar-lalr)))

(defun find-others-lalr (x list)
  (dolist (y list)
    (and (not (eql x y))
         (equal-state-lalr x y)
         (return t))))

(defun split-lalr (x list)
  (when (find-others-lalr x list)
    (let (merge remove)
      (dolist (y list)
        (if (equal-state-lalr x y)
          (push y merge)
          (push y remove)))
      (values merge remove))))

(defun merge-grammar-lalr (x y)
  (grammar-instance
    (grammar-left x)
    (grammar-alpha x)
    (grammar-beta x)
    (union (grammar-ahead x)
           (grammar-ahead y))))

(defun merge-list-lalr (x y)
  (let ((listx (state-list x))
        (listy (state-list y))
        list)
    (dolist (v listx)
      (aif (find v listy :test #'equal-grammar-lalr)
        (push (merge-grammar-lalr v it) list)
        (error "Invalid state, ~S, ~S." x y)))
    list))

(defun merge-action-lalr (x y)
  (let ((x (state-action x))
        (y (state-action y)))
    (union x y :test 'equal)))

(defun merge-goto-lalr (x y)
  (let ((x (state-goto x))
        (y (state-goto y)))
    (union x y :test 'equal)))

(defun merge-state-lalr (x y)
  (let ((list (merge-list-lalr x y))
        (action (merge-action-lalr x y))
        (goto (merge-goto-lalr x y)))
    (make-state :list list :action action :goto goto)))

(defun merge-lalr (list)
  (aprog1 (reduce #'merge-state-lalr list)
    (setf (state-index it) *state-index*)
    (push it *state*)
    (incf *state-index*)))

(defun replace-action-lalr (inst a b)
  (let ((list (state-action inst)))
    (dolist (x list)
      (and (eq (cadr x) 's)
           (eql (caddr x) a)
           (setf (caddr x) b)))
    (setq list (delete-duplicates list :test 'equal))
    (setf (state-action inst) list)))

(defun replace-goto-lalr (inst a b)
  (let ((list (state-goto inst)))
    (dolist (x list)
      (when (eql (cadr x) a)
        (setf (cadr x) b)))
    (setq list (delete-duplicates list :test 'equal))
    (setf (state-goto inst) list)))

(defun replace-lalr (x merge list)
  (let (a b)
    (setq b (state-index x))
    (dolist (m merge)
      (setq a (state-index m))
      (dolist (inst list)
        (replace-action-lalr inst a b)
        (replace-goto-lalr inst a b)))))

(defun update-lalr (x)
  (multiple-value-bind (merge remove) (split-lalr x *state*)
    (when merge
      (setq *state* remove)
      (let ((x (merge-lalr merge)))
        (replace-lalr x merge remove)
        t))))

(defun make-lalr ()
  (dolist (x *state*)
    (when (update-lalr x)
      (return (make-lalr)))))


;;
;;  Main
;;
(parse-terminal + * [ ] int)
(parse-start expr)
(parse-rule expr -> expr + term)
(parse-rule expr -> term)
(parse-rule term -> term * fact)
(parse-rule term -> fact)
(parse-rule fact -> [ expr ])
(parse-rule fact -> int)

;(main-test)
(make-table)
(make-reduce)
(make-lalr)
(output-table)
(execute-parse '(int * [ int + int ]))


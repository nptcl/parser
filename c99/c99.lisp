;;
;;  c99 parser
;;
;;  JTC1/SC22/WG14 - C
;;  WG14/N1256 Committee Draft - Septermber 7, 2007 ISO/IEC 9899:TC3
;;  http://www.open-std.org/JTC1/SC22/WG14/
;;  http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
;;

;;
;;  A.2 Phrase structure grammar
;;  A.2.1 Expressions
;;

;; (6.5.1)
(terminal identifier constant string-literal #\( #\))
(rule primary-expression -> identifier)
(rule primary-expression -> constant)
(rule primary-expression -> string-literal)
(rule primary-expression -> #\( expression #\))

;; (6.5.2)
(terminal #\[ #\] #\( #\) #\. -> ++ -- #\{ #\} #\,)
(terminal identifier)
(rule postfix-expression -> primary-expression)
(rule postfix-expression -> postfix-expression #\[ expression #\])
(rule postfix-expression -> postfix-expression #\( #\))
(rule postfix-expression -> postfix-expression #\( argument-expression-list #\))
(rule postfix-expression -> postfix-expression #\. identifier)
(rule postfix-expression -> postfix-expression -> identifier)
(rule postfix-expression -> postfix-expression ++)
(rule postfix-expression -> postfix-expression --)
(rule postfix-expression -> #\( type-name #\) #\{ initializer-list #\})
(rule postfix-expression -> #\( type-name #\) #\{ initializer-list #\, #\})

;; (6.5.2)
(terminal #\,)
(rule argument-expression-list -> assignment-expression)
(rule argument-expression-list -> argument-expression-list #\, assignment-expression)

;; (6.5.3)
(terminal ++ -- sizeof #\( #\))
(rule unary-expression -> postfix-expression)
(rule unary-expression -> ++ unary-expression)
(rule unary-expression -> -- unary-expression)
(rule unary-expression -> unary-operator cast-expression)
(rule unary-expression -> sizeof unary-expression)
(rule unary-expression -> sizeof #\( type-name #\))

;; (6.5.3)
(terminal #\& #\* #\+ #\- #\~ #\!)
(rule unary-operator -> #\&)
(rule unary-operator -> #\*)
(rule unary-operator -> #\+)
(rule unary-operator -> #\-)
(rule unary-operator -> #\~)
(rule unary-operator -> #\!)

;; (6.5.4)
(terminal #\( #\))
(rule cast-expression -> unary-expression)
(rule cast-expression -> #\( type-name #\) cast-expression)

;; (6.5.5)
(terminal #\* #\/ #\%)
(rule multiplicative-expression -> cast-expression)
(rule multiplicative-expression -> multiplicative-expression #\* cast-expression)
(rule multiplicative-expression -> multiplicative-expression #\/ cast-expression)
(rule multiplicative-expression -> multiplicative-expression #\% cast-expression)

;; (6.5.6)
(terminal #\+ #\-)
(rule additive-expression -> multiplicative-expression)
(rule additive-expression -> additive-expression #\+ multiplicative-expression)
(rule additive-expression -> additive-expression #\- multiplicative-expression)

;; (6.5.7)
(terminal << >>)
(rule shift-expression -> additive-expression)
(rule shift-expression -> shift-expression << additive-expression)
(rule shift-expression -> shift-expression >> additive-expression)

;; (6.5.8)
(terminal #\< #\> <= >=)
(rule relational-expression -> shift-expression)
(rule relational-expression -> relational-expression #\< shift-expression)
(rule relational-expression -> relational-expression #\> shift-expression)
(rule relational-expression -> relational-expression <= shift-expression)
(rule relational-expression -> relational-expression >= shift-expression)

;; (6.5.9)
(terminal == !=)
(rule equality-expression -> relational-expression)
(rule equality-expression -> equality-expression == relational-expression)
(rule equality-expression -> equality-expression != relational-expression)

;; (6.5.10)
(terminal #\&)
(rule AND-expression -> equality-expression)
(rule AND-expression -> AND-expression #\& equality-expression)

;; (6.5.11)
(terminal #\^)
(rule exclusive-OR-expression -> AND-expression)
(rule exclusive-OR-expression -> exclusive-OR-expression #\^ AND-expression)

;; (6.5.12)
(terminal #\|)
(rule inclusive-OR-expression -> exclusive-OR-expression)
(rule inclusive-OR-expression -> inclusive-OR-expression #\| exclusive-OR-expression)

;; (6.5.13)
(terminal &&)
(rule logical-AND-expression -> inclusive-OR-expression)
(rule logical-AND-expression -> logical-AND-expression && inclusive-OR-expression)

;; (6.5.14)
(terminal or)  ;; ||
(rule logical-OR-expression -> logical-AND-expression)
(rule logical-OR-expression -> logical-OR-expression or logical-AND-expression)

;; (6.5.15)
(terminal #\? #\:)
(rule conditional-expression -> logical-OR-expression)
(rule conditional-expression ->
      logical-OR-expression #\? expression #\: conditional-expression)

;; (6.5.16)
(rule assignment-expression -> conditional-expression)
(rule assignment-expression ->
      unary-expression assignment-operator assignment-expression)

;; (6.5.16)
(terminal #\= *= /= %= += -= <<= >>= &= ^=)
(terminal or-assign)  ;; |=
(rule assignment-operator -> #\=)
(rule assignment-operator -> *=)
(rule assignment-operator -> /=)
(rule assignment-operator -> %=)
(rule assignment-operator -> +=)
(rule assignment-operator -> -=)
(rule assignment-operator -> <<=)
(rule assignment-operator -> >>=)
(rule assignment-operator -> &=)
(rule assignment-operator -> ^=)
(rule assignment-operator -> or-assign)

;; (6.5.17)
(terminal #\,)
(rule expression -> assignment-expression)
(rule expression -> expression #\, assignment-expression)

;; (6.6)
(rule constant-expression -> conditional-expression)


;;
;;  A.2.2 Declarations
;;

;; (6.7)
(terminal #\;)
(rule declaration -> declaration-specifiers #\;)
(rule declaration -> declaration-specifiers init-declarator-list #\;)

;; (6.7)
(rule declaration-specifiers -> storage-class-specifier)
(rule declaration-specifiers -> storage-class-specifier declaration-specifiers)
(rule declaration-specifiers -> type-specifier)
(rule declaration-specifiers -> type-specifier declaration-specifiers)
(rule declaration-specifiers -> type-qualifier)
(rule declaration-specifiers -> type-qualifier declaration-specifiers)
(rule declaration-specifiers -> function-specifier)
(rule declaration-specifiers -> function-specifier declaration-specifiers)

;; (6.7)
(rule init-declarator-list -> init-declarator)
(rule init-declarator-list -> init-declarator-list #\, init-declarator)

;; (6.7)
(rule init-declarator -> declarator)
(rule init-declarator -> declarator #\= initializer)

;; (6.7.1)
(terminal typedef extern static auto register)
(rule storage-class-specifier -> typedef)
(rule storage-class-specifier -> extern)
(rule storage-class-specifier -> static)
(rule storage-class-specifier -> auto)
(rule storage-class-specifier -> register)

;; (6.7.2)
(terminal void char short int long float double signed unsigned)
(terminal _Bool _Complex)
(rule type-specifier -> void)
(rule type-specifier -> char)
(rule type-specifier -> short)
(rule type-specifier -> int)
(rule type-specifier -> long)
(rule type-specifier -> float)
(rule type-specifier -> double)
(rule type-specifier -> signed)
(rule type-specifier -> unsigned)
(rule type-specifier -> _Bool)
(rule type-specifier -> _Complex)
(rule type-specifier -> struct-or-union-specifier)
(rule type-specifier -> enum-specifier)
(rule type-specifier -> typedef-name)

;; (6.7.2.1)
(terminal #\{ #\})
(terminal identifier)
(rule struct-or-union-specifier ->
      struct-or-union #\{ struct-declaration-list #\})
(rule struct-or-union-specifier ->
      struct-or-union identifier #\{ struct-declaration-list #\})
(rule struct-or-union-specifier ->
      struct-or-union identifier)

;; (6.7.2.1)
(terminal struct union)
(rule struct-or-union -> struct)
(rule struct-or-union -> union)

;; (6.7.2.1)
(rule struct-declaration-list -> struct-declaration)
(rule struct-declaration-list -> struct-declaration-list struct-declaration)

;; (6.7.2.1)
(terminal #\;)
(rule struct-declaration -> specifier-qualifier-list struct-declarator-list #\;)

;; (6.7.2.1)
(rule specifier-qualifier-list -> type-specifier)
(rule specifier-qualifier-list -> type-specifier specifier-qualifier-list)
(rule specifier-qualifier-list -> type-qualifier)
(rule specifier-qualifier-list -> type-qualifier specifier-qualifier-list)

;; (6.7.2.1)
(terminal #\,)
(rule struct-declarator-list -> struct-declarator)
(rule struct-declarator-list -> struct-declarator-list #\, struct-declarator)

;; (6.7.2.1)
(terminal #\:)
(rule struct-declarator -> declarator)
(rule struct-declarator -> #\: constant-expression)
(rule struct-declarator -> declarator #\: constant-expression)

;; (6.7.2.2)
(terminal enum #\{ #\} #\,)
(terminal identifier)
(rule enum-specifier -> enum #\{ enumerator-list #\})
(rule enum-specifier -> enum identifier #\{ enumerator-list #\})
(rule enum-specifier -> enum #\{ enumerator-list #\, #\})
(rule enum-specifier -> enum identifier #\{ enumerator-list #\, #\})
(rule enum-specifier -> enum identifier)

;; (6.7.2.2)
(terminal #\,)
(rule enumerator-list -> enumerator)
(rule enumerator-list -> enumerator-list #\, enumerator)

;; (6.7.2.2)
(terminal #\=)
(rule enumerator -> enumeration-constant)
(rule enumerator -> enumeration-constant #\= constant-expression)

;; (6.4.4.3)
(rule enumeration-constant -> identifier)

;; (6.7.3)
(terminal const restrict volatile)
(rule type-qualifier -> const)
(rule type-qualifier -> restrict)
(rule type-qualifier -> volatile)

;; (6.7.4)
(terminal inline)
(rule function-specifier -> inline)

;; (6.7.5)
(rule declarator -> direct-declarator)
(rule declarator -> pointer direct-declarator)

;; (6.7.5)
(terminal #\( #\) #\[ #\] #\* static)
(terminal identifier)
(rule direct-declarator ->
      identifier)
(rule direct-declarator ->
      #\( declarator #\))
(rule direct-declarator ->
      direct-declarator #\[ #\])
(rule direct-declarator ->
      direct-declarator #\[ type-qualifier-list #\])
(rule direct-declarator ->
      direct-declarator #\[ assignment-expression #\])
(rule direct-declarator ->
      direct-declarator #\[ type-qualifier-list assignment-expression #\])
(rule direct-declarator ->
      direct-declarator #\[ static assignment-expression #\])
(rule direct-declarator ->
      direct-declarator #\[ static type-qualifier-list assignment-expression #\])
(rule direct-declarator ->
      direct-declarator #\[ type-qualifier-list static assignment-expression #\])
(rule direct-declarator ->
      direct-declarator #\[ #\* #\])
(rule direct-declarator ->
      direct-declarator #\[ type-qualifier-list #\* #\])
(rule direct-declarator ->
      direct-declarator #\( parameter-type-list #\))
(rule direct-declarator ->
      direct-declarator #\( #\))
(rule direct-declarator ->
      direct-declarator #\( identifier-list #\))

;; (6.7.5)
(terminal #\*)
(rule pointer -> #\*)
(rule pointer -> #\* type-qualifier-list)
(rule pointer -> #\* pointer)
(rule pointer -> #\* type-qualifier-list pointer)

;; (6.7.5)
(rule type-qualifier-list -> type-qualifier)
(rule type-qualifier-list -> type-qualifier-list type-qualifier)

;; (6.7.5)
(terminal #\, ellipsis)  ;; ...
(rule parameter-type-list -> parameter-list)
(rule parameter-type-list -> parameter-list #\, ellipsis)

;; (6.7.5)
(terminal #\,)
(rule parameter-list -> parameter-declaration)
(rule parameter-list -> parameter-list #\, parameter-declaration)

;; (6.7.5)
(rule parameter-declaration -> declaration-specifiers declarator)
(rule parameter-declaration -> declaration-specifiers)
(rule parameter-declaration -> declaration-specifiers abstract-declarator)

;; (6.7.5)
(terminal #\,)
(terminal identifier)
(rule identifier-list -> identifier)
(rule identifier-list -> identifier-list #\, identifier)

;; (6.7.6)
(rule type-name -> specifier-qualifier-list)
(rule type-name -> specifier-qualifier-list abstract-declarator)

;; (6.7.6)
(rule abstract-declarator -> pointer)
(rule abstract-declarator -> direct-abstract-declarator)
(rule abstract-declarator -> pointer direct-abstract-declarator)

;; (6.7.6)
(terminal #\( #\) #\[ #\] #\* static)
(rule direct-abstract-declarator ->
      #\( abstract-declarator #\))
(rule direct-abstract-declarator ->
      #\[ #\])
(rule direct-abstract-declarator ->
      #\[ type-qualifier-list #\])
(rule direct-abstract-declarator ->
      #\[ assignment-expression #\])
(rule direct-abstract-declarator ->
      #\[ type-qualifier-list assignment-expression #\])
(rule direct-abstract-declarator ->
      #\[ static assignment-expression #\])
(rule direct-abstract-declarator ->
      #\[ static type-qualifier-list assignment-expression #\])
(rule direct-abstract-declarator ->
      #\[ type-qualifier-list static assignment-expression #\])
(rule direct-abstract-declarator ->
      #\[ #\* #\])
(rule direct-abstract-declarator ->
      #\( #\))
(rule direct-abstract-declarator ->
      #\( parameter-type-list #\))
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ type-qualifier-list #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ assignment-expression #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ type-qualifier-list assignment-expression #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ static assignment-expression #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ static type-qualifier-list assignment-expression #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ type-qualifier-list static assignment-expression #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\[ #\* #\])
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\( #\))
(rule direct-abstract-declarator ->
      direct-abstract-declarator #\( parameter-type-list #\))

;; (6.7.7)
;(terminal identifier)
;(rule typedef-name -> identifier)
(terminal typedef-identifier)
(rule typedef-name -> typedef-identifier)

;; (6.7.8)
(terminal #\{ #\} #\,)
(rule initializer -> assignment-expression)
(rule initializer -> #\{ initializer-list #\})
(rule initializer -> #\{ initializer-list #\, #\})

;; (6.7.8)
(terminal #\,)
(rule initializer-list -> initializer)
(rule initializer-list -> designation initializer)
(rule initializer-list -> initializer-list #\, initializer)
(rule initializer-list -> initializer-list #\, designation initializer)

;; (6.7.8)
(terminal #\=)
(rule designation -> designator-list #\=)

;; (6.7.8)
(rule designator-list -> designator)
(rule designator-list -> designator-list designator)

;; (6.7.8)
(terminal #\[ #\] #\.)
(terminal identifier)
(rule designator -> #\[ constant-expression #\])
(rule designator -> #\. identifier)


;;
;;  A.2.3 Statements
;;

;; (6.8)
(rule statement -> labeled-statement)
(rule statement -> compound-statement)
(rule statement -> expression-statement)
(rule statement -> selection-statement)
(rule statement -> iteration-statement)
(rule statement -> jump-statement)

;; (6.8.1)
(terminal case default #\:)
(terminal identifier)
(rule labeled-statement -> identifier #\: statement)
(rule labeled-statement -> case constant-expression #\: statement)
(rule labeled-statement -> default #\: statement)

;; (6.8.2)
(terminal #\{ #\})
(rule compound-statement -> #\{ #\})
(rule compound-statement -> #\{ block-item-list #\})

;; (6.8.2)
(rule block-item-list -> block-item)
(rule block-item-list -> block-item-list block-item)

;; (6.8.2)
(rule block-item -> declaration)
(rule block-item -> statement)

;; (6.8.3)
(terminal #\;)
(rule expression-statement -> #\;)
(rule expression-statement -> expression #\;)

;; (6.8.4)
(terminal if else switch #\( #\))
(rule selection-statement -> if #\( expression #\) statement)
(rule selection-statement -> if #\( expression #\) statement else statement)
(rule selection-statement -> switch #\( expression #\) statement)

;; (6.8.5)
(terminal while do for #\( #\) #\;)
(rule iteration-statement -> while #\( expression #\) statement)
(rule iteration-statement -> do statement while #\( expression #\) #\;)
(rule iteration-statement -> for #\( #\; #\; #\) statement)
(rule iteration-statement -> for #\( expression #\; #\; #\) statement)
(rule iteration-statement -> for #\( #\; expression #\; #\) statement)
(rule iteration-statement -> for #\( #\; #\; expression #\) statement)
(rule iteration-statement -> for #\( expression #\; expression #\; #\) statement)
(rule iteration-statement -> for #\( expression #\; #\; expression #\) statement)
(rule iteration-statement -> for #\( #\; expression #\; expression #\) statement)
(rule iteration-statement -> for #\( expression #\; expression #\; expression #\) statement)
(rule iteration-statement -> for #\( declaration expression #\; #\) statement)
(rule iteration-statement -> for #\( declaration expression #\; expression #\) statement)

;; (6.8.6)
(terminal goto continue break return #\;)
(terminal identifier)
(rule jump-statement -> goto identifier #\;)
(rule jump-statement -> continue #\;)
(rule jump-statement -> break #\;)
(rule jump-statement -> return #\;)
(rule jump-statement -> return expression #\;)


;;
;;  A.2.4 External definitions
;;

;; (6.9)
(rule translation-unit -> external-declaration)
(rule translation-unit -> translation-unit external-declaration)

;; (6.9)
(rule external-declaration -> function-definition)
(rule external-declaration -> declaration)

;; (6.9.1)
(rule function-definition ->
      declaration-specifiers declarator compound-statement)
(rule function-definition ->
      declaration-specifiers declarator declaration-list compound-statement)

;; (6.9.1)
(rule declaration-list -> declaration)
(rule declaration-list -> declaration-list declaration)


;;
;;  start
;;
(start translation-unit)


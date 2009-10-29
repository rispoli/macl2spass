(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(define-tokens value-tokens (PROP_ATOM MODE))
(define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT AMPERAMPER BARBAR MINUSGREATER BOX COMMA LIST_OF_FORMULAE DOT END_OF_LIST))

(define-lex-abbrevs
  ;(comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))) ; C style
  (comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")) ; OCaml style
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer-src-pos
    ((eof) 'EOF)
    ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
    ("(" 'LPAREN)
    (")" 'RPAREN)
    ("not" 'NOT)
    ("&&" 'AMPERAMPER)
    ("||" 'BARBAR)
    ("->" 'MINUSGREATER)
    ("box" 'BOX)
    ("," 'COMMA)
    ("list_of_formulae" 'LIST_OF_FORMULAE)
    ("axioms" (token-MODE "axioms"))
    ("conjectures" (token-MODE "conjectures"))
    ("." 'DOT)
    ("end_of_list" 'END_OF_LIST)
    ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-PROP_ATOM (string->symbol lexeme)))))

(define dslp
  (lambda (source-name)
    (parser
      (src-pos)
      (start start)
      (end EOF)
      (tokens value-tokens op-tokens)
      (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
               (raise-read-error
                 (format "unexpected ~a" tok-name)
                 source-name
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))))

      (precs (right MINUSGREATER)
             (left BARBAR)
             (left AMPERAMPER)
             (left NOT))

      (grammar
        (start (() #f)
               ((toplevel) $1))
        (expr ((simple-expr) $1)
              ((NOT expr) (format "not(~a)" $2))
              ((expr AMPERAMPER expr) (format "and(~a, ~a)" $1 $3))
              ((expr BARBAR expr) (format "or(~a, ~a)" $1 $3))
              ((expr MINUSGREATER expr) (format "implies(~a, ~a)" $1 $3))
              ((BOX LPAREN expr COMMA expr RPAREN) (format "box(~a, ~a)" $3 $5)))
        (expr-list ((expr-list DOT expr) (cons $3 $1))
                   ((expr) (list $1))
                   ((expr-list DOT) $1)
                   (() '()))
        (mode ((MODE) $1))
        (toplevel ((LIST_OF_FORMULAE LPAREN mode RPAREN expr-list END_OF_LIST ) (format "list_of_formulae(~a).~n~a~n~nend_of_list.~n"
                                                                                        $3
                                                                                        (foldl string-append ""
                                                                                               (map (lambda (s) (format "~n\tformula(~a)." s)) $5))))
                  )
        (simple-expr ((PROP_ATOM) (symbol->string $1)) ;; differentiate for true and false iff list_of_symbols must be generated automatically
                     ((LPAREN expr RPAREN) (format "~a" $2))
                     )
        ))))

(define translate
  (lambda (s #:src-name (src-name "current-input-port"))
    (let ((ois (open-input-string s)) (statements ""))
      (port-count-lines! ois)
      (letrec ((loop
                 (lambda ()
                   (let ((r ((dslp src-name) (lambda () (dsll ois)))))
                     (when r
                       (set! statements (string-append r statements))
                       (loop))))))
        (loop))
      ;(reverse statements))))
      statements)))

(define translate-file
  (lambda (path)
    (call-with-input-file path
                          (lambda (in)
                            (translate (port->string in) #:src-name path)))))

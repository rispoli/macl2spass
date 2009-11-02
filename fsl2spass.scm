(module fsl2spass scheme
        (provide translate-file pretty-print-spass)

        (require parser-tools/yacc
                 parser-tools/lex
                 (prefix-in : parser-tools/lex-sre)
                 syntax/readerr
                 scheme/string
                 scheme/cmdline)

        (define-tokens value-tokens (PROP_ATOM MODE BOOL))
        (define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT AMPERAMPER BARBAR MINUSGREATER BOX COMMA LIST_OF_FORMULAE DOT END_OF_LIST))

        (define-lex-abbrevs
          (comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))) ; C style
          ;(comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")) ; OCaml style
          (lower-letter (:/ "a" "z"))
          (upper-letter (:/ #\A #\Z))
          (digit (:/ "0" "9")))

        (define dsll
          (lexer-src-pos
            ((eof) 'EOF)
            ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
            ("true" (token-BOOL 'true))
            ("false" (token-BOOL 'false))
            ("(" 'LPAREN)
            (")" 'RPAREN)
            ("not" 'NOT)
            ("&&" 'AMPERAMPER)
            ("||" 'BARBAR)
            ("->" 'MINUSGREATER)
            ("[]" 'BOX)
            ("," 'COMMA)
            ("list_of_formulae" 'LIST_OF_FORMULAE)
            ("axioms" (token-MODE 'axioms))
            ("conjectures" (token-MODE 'conjectures))
            ("." 'DOT)
            ("end_of_list" 'END_OF_LIST)
            ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-PROP_ATOM (string->symbol lexeme)))))

        (define dslp
          (lambda (source-name assignment initial-world)
            (parser
              (src-pos)
              (start start)
              (end EOF END_OF_LIST)
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
                      ((NOT expr) (let ((new-world (gensym "w")))
                                    `(forall ,new-world (implies (leq) (not ,$2)))))
                      ((expr AMPERAMPER expr) `(and ,$1 ,$3))
                      ((expr BARBAR expr) `(or ,$1 ,$3))
                      ((expr MINUSGREATER expr) (let ((new-world (gensym "w")))
                                                  `(forall ,new-world (implies (and (leq) ,$1) ,$3))))
                      ((BOX LPAREN PROP_ATOM COMMA expr RPAREN) (let ((new-world (gensym "w")))
                                                                  `(forall ,new-world (implies (R ,$3) ,$5)))))
                (expr-list ((expr-list DOT expr) (cons $3 $1))
                           ((expr) (list $1))
                           ((expr-list DOT) $1)
                           (() '()))
                (mode ((MODE) $1))
                (toplevel ((LIST_OF_FORMULAE LPAREN mode RPAREN expr-list) `(,$3 ,(map (lambda (e)
                                                                                         `(formula ,(update-world initial-world initial-world assignment e)))
                                                                                       (reverse $5)))))
                (simple-expr ((PROP_ATOM) `(,assignment ,$1 ,initial-world))
                             ((BOOL) $1)
                             ((LPAREN expr RPAREN) $2))))))

        (define update-world
          (lambda (initial-world new-world assignment code)
            (letrec ((update-world-inner
                       (lambda (code)
                         (cond
                           ((or (eqv? code 'true) (eqv? code 'false)) code)
                           ((eqv? (car code) assignment) `(,assignment ,(cadr code) ,new-world))
                           (else
                             (case (car code)
                               ((forall) (let ((initial-world new-world) (new-world (cadr code)))
                                           `(forall ,new-world ,(update-world initial-world new-world assignment (caddr code)))))
                               ((leq) `(leq ,initial-world ,new-world))
                               ((not) `(not ,(update-world-inner (cadr code))))
                               ((R) `(R ,(cadr code) ,initial-world ,new-world))
                               (else `(,(car code) ,(update-world-inner (cadr code)) ,(update-world-inner (caddr code))))))))))
              (update-world-inner code))))

        (define translate
          (lambda (s assignment initial-world #:src-name (src-name "current-input-port"))
            (let ((ois (open-input-string s)) (statements '()))
              (port-count-lines! ois)
              (letrec ((loop
                         (lambda ()
                           (let ((r ((dslp src-name assignment initial-world) (lambda () (dsll ois)))))
                             (when r
                               (set! statements (cons r statements))
                               (loop))))))
                (loop))
              (reverse statements))))

        (define translate-file
          (lambda (path assignment initial-world)
            (call-with-input-file path
                                  (lambda (in)
                                    (translate (port->string in) assignment initial-world #:src-name path)))))

        (define counter
          (let ((c 0))
            (lambda ()
              (set! c (+ c 1))
              c)))

        (define pretty-print-spass
          (lambda (code)
            (cond
              ((symbol? code) (symbol->string code))
              ((number? code) (number->string code))
              ((or (eqv? (car code) 'conjectures) (eqv? (car code) 'axioms)) (format "list_of_formulae(~a).~n~a~n~nend_of_list.~n~n"
                                                                                     (car code)
                                                                                     (foldr string-append
                                                                                            ""
                                                                                            (map (lambda (e)
                                                                                                   (pretty-print-spass e)) (list-ref code 1)))))
              (else
                (case (car code)
                  ((formula) (format "~n\tformula(~a, ~a)." (pretty-print-spass (list-ref code 1)) (counter)))
                  ((forall) (format "forall([world(~a)], ~a)" (list-ref code 1) (pretty-print-spass (list-ref code 2))))
                  (else (format "~a(~a)" (car code) (string-join (map (lambda (e) (pretty-print-spass e)) (cdr code)) ", "))))))))

        (define main
          (let ((assignment (make-parameter 'I)) (initial-world (make-parameter '0)))
            (command-line
              #:once-each
              (("-a" "--assignment") a
                                     "Assignment function: defaults to 'I'"
                                     (assignment a))
              (("-i" "--initial-world") i
                                        "Initial world: defaults to '0'"
                                        (initial-world i))

              #:args (filename)
              (list filename (assignment) (initial-world)))))

        (for-each (lambda (e) (display (pretty-print-spass e))) (apply translate-file main)))

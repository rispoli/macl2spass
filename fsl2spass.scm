;;; Copyright 2009 Daniele Rispoli
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(module fsl2spass scheme
        (provide translate-file pretty-print-spass)

        (define *axioms-file* "axioms.dfg")
        (define *atoms* '())
        (define *principals* '())

        (require parser-tools/yacc
                 parser-tools/lex
                 (prefix-in : parser-tools/lex-sre)
                 syntax/readerr
                 scheme/string
                 scheme/cmdline
                 scheme/match)

        (define-tokens value-tokens (PROP_ATOM MODE BOOL))
        (define-empty-tokens op-tokens (EOF LPAREN RPAREN NOT AMPERAMPER BARBAR MINUSGREATER BOX COMMA CONTROLS EQUALSGREATER LIST_OF_FORMULAE DOT END_OF_LIST))

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
            ("C" 'CONTROLS)
            ("=>" 'EQUALSGREATER)
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
                     (left EQUALSGREATER)
                     (left BARBAR)
                     (left AMPERAMPER)
                     (left NOT))

              (grammar
                (start (() #f)
                       ((toplevel) $1))
                (expr ((simple-expr) $1)
                      ((NOT expr) (let ((new-world (get-world-name)))
                                    `(forall ,new-world (implies (leq) (not ,$2)))))
                      ((expr AMPERAMPER expr) `(and ,$1 ,$3))
                      ((expr BARBAR expr) `(or ,$1 ,$3))
                      ((expr MINUSGREATER expr) (let ((new-world (get-world-name)))
                                                  `(forall ,new-world (implies (and (leq) ,$1) ,$3))))
                      ((BOX LPAREN principal COMMA expr RPAREN) (let ((new-world (get-world-name)))
                                                                  `(forall ,new-world (implies (R ,$3) ,$5))))
                      ((CONTROLS LPAREN principal COMMA expr RPAREN) (let ((y (get-world-name)) (x (get-world-name)))
                                                                       `(forall ,x (implies (and (leq) (forall ,y (implies (R ,$3) ,$5))) ,$5))))
                      ((principal EQUALSGREATER principal) (let ((y (get-world-name)) (x (get-world-name)))
                                                             `(forall ,x (forall ,y (implies (R ,$3) (R ,$1)))))))
                (expr-list ((expr-list DOT expr) (cons $3 $1))
                           ((expr) (list $1))
                           ((expr-list DOT) $1)
                           (() '()))
                (mode ((MODE) $1))
                (toplevel ((LIST_OF_FORMULAE LPAREN mode RPAREN expr-list) `(,$3 ,(map (lambda (e)
                                                                                         `(formula ,(update-world initial-world initial-world assignment e)))
                                                                                       (reverse $5)))))
                (principal ((PROP_ATOM) (begin
                                          (set! *principals* (cons $1 *principals*))
                                          $1)))
                (simple-expr ((PROP_ATOM) (begin
                                            (set! *atoms* (cons $1 *atoms*))
                                            `(,assignment ,$1 ,initial-world)))
                             ((BOOL) $1)
                             ((LPAREN expr RPAREN) $2))))))

        (define get-world-name
          (lambda ()
            (gensym "w")))

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
              (list (reverse (remove-duplicates *atoms*)) (reverse (remove-duplicates *principals*)) (reverse statements)))))

        (define translate-file
          (lambda (path assignment initial-world)
            (call-with-input-file path
                                  (lambda (in)
                                    (translate (port->string in) assignment initial-world #:src-name path)))))

        (define counter
          (let ((c 7))
            (lambda ()
              (set! c (+ c 1))
              c)))

        (define pretty-print-spass
          (lambda (code)
            (cond
              ((symbol? code) (symbol->string code))
              ((number? code) (number->string code))
              ((or (eqv? (car code) 'conjectures) (eqv? (car code) 'axioms)) (format "list_of_formulae(~a).~n~a~a~n~nend_of_list.~n~n"
                                                                                     (car code)
                                                                                     (if (eqv? (car code) 'axioms)
                                                                                       (call-with-input-file *axioms-file*
                                                                                                             (lambda (in)
                                                                                                               (port->string in)))
                                                                                       "")
                                                                                     (foldr string-append
                                                                                            ""
                                                                                            (map (lambda (e)
                                                                                                   (pretty-print-spass e)) (list-ref code 1)))))
              (else
                (case (car code)
                  ((formula) (format "~n\tformula(~a, ~a)." (pretty-print-spass (list-ref code 1)) (counter)))
                  ((forall) (format "forall([world(~a)], ~a)" (list-ref code 1) (pretty-print-spass (list-ref code 2))))
                  (else (format "~a(~a)" (car code) (string-join (map (lambda (e) (pretty-print-spass e)) (cdr code)) ", "))))))))

        (define pretty-print-symbols
          (lambda (symbols assignment)
            (let ((listify (lambda (name ls)
                             (format "~a[~a]." name (string-join (map (lambda (e)
                                                                        (apply format "(~a, ~a)" (if (list? e)
                                                                                                   (list (car e) (cadr e))
                                                                                                   (list e 0)))) ls) ", ")))))
              (format "list_of_symbols.~n~n\t~a~n\t~a~n\tsorts[atom, principal, world].~n~nend_of_list.~n~n"
                      (listify "functions" symbols)
                      (listify "predicates" `((,assignment 2) (R 3) (leq 2)))))))

        (define pretty-print-declarations
          (lambda (atoms principals initial-world)
            (let ((listify (lambda (sort-name ls)
                             (foldr string-append
                                    ""
                                    (map (lambda (e)
                                           (format "~n\t~a(~a)." sort-name e)) ls)))))
              (format "list_of_declarations.~n~a~a~n\tworld(~a).~n~nend_of_list.~n~n"
                      (listify "atom" atoms)
                      (listify "principal" principals)
                      initial-world))))

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

        (match-let* (((list filename assignment initial-world) main) ((list atoms principals code) (translate-file filename assignment initial-world)))
                    (display (format "begin_problem(problem_name).~n~nlist_of_descriptions.~n~n\tname({*Problem's name*}).~n\tauthor({*Author*}).~n\tstatus(unsatisfiable). % or satisfiable or unknown~n\tdescription({*Description*}).~n~nend_of_list.~n~n~a~a~aend_problem."
                                     (pretty-print-symbols (append (list initial-world) atoms principals) assignment)
                                     (pretty-print-declarations atoms principals initial-world)
                                     (foldr string-append
                                            ""
                                            (map (lambda (e)
                                                   (pretty-print-spass e))
                                                 code))))))

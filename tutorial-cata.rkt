#lang nanopass

(define (foo? x)
  (eq? x 'Foo))

(define (bar? x)
  (eq? x 'Bar))

(define bv 'Bar)

(define program
  '(
   Foo
   Foo
   (fe (
        Foo
        Foo))
   (fe (
        Foo
        (fe (
             Foo
             (fe (
                  Foo
                  Foo))))))))

(define-language FooLang
  (terminals
   (foo (f))
   (bar (b)))
  (Expr (e)
   (e* ... e)
   FF)
  (Foo (FF)
   (fe e)
   f
   b))

(define-parser parser-Foo FooLang)

(define-pass removeFoo : FooLang (e) -> FooLang ()
  (Foo->Bar : Foo (e) -> Foo ()
        [,f `,bv]))

(removeFoo
 (parser-Foo program))

(define-pass removeFoo2 : FooLang (e) -> FooLang ()
  (Foo->Bar : Foo (e) -> Foo ()
        [,f `,bv]
        [(fe ,[e]) `(fe ,e)]))

(removeFoo2
 (parser-Foo program))

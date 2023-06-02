;; FIXME: loads from cwd, make load-rel or something
;; use real imports
(load "./src/langs/cpp.scm")
(load "./src/query.scm")
(load "./tests/support.scm")

(define workspace '("./tests/cpp/simple1.cpp"))

(test-group "cpp-body"

  (test-query
    (dedent "
    int foo() { return 5; return 10; }
    int foo() {
        int a = f();
        return a;
        return 10;
    }")
    (transform
      ((function_definition
         declarator: (_ (identifier) @name)
         body: (compound_statement (_) @body)
       ) @func)

      (@func
        body: (compound_statement* (append (@body) (return_statement* "10"))))
      workspace))

  ) ; end test-group "cpp-simple"


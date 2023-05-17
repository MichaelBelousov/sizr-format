#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-tree-stitcher (ffi-lib "libbindings"))


(define-cstruct _TSNode
  ((context1 _uint) ; intentionally inaccessible
   (unsigned-int _uint)
   (context3 _uint)
   (context4 _uint)
   (id (_pointer _void))
   (tree (_pointer _void))))

(define _TSNode-ptr (_cpointer _TSNode))

(define-cstruct _TSQueryCapture
  ((node _TSNode)
   (index _uint)))

(define-cstruct _TSQueryMatch
  ((id _uint)
   (pattern_index _ushort)
   (capture_count _ushort)
   ;; FIXME
   (captures (_array _TSQueryCapture 1))))

(define-tree-stitcher ts_node_string (_fun _TSNode -> _string))

;; opaque, hence stupid hidden int property
; (define-cstruct ExecQueryResult
;   (int _test))

(define (check v who)
  (unless (zero? v)
    (error who "failed: ~a" v)))

(define-tree-stitcher exec_query
  (_fun _string
        (_array _string)
          -> (r : _string)
          -> (check r 'exec_query)))

(define-tree-stitcher transform_ExecQueryResult
  (_fun 'ExecQueryResult
        _scheme
          -> (r : _string)
          -> (check r 'exec_query)))

; export those bad boys


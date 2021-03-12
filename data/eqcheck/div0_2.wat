;; Both these functions trap unconditionally.
(module
  (func $f1 (param $a i32) (result i32)
    (i32.div_u
      (i32.const 10)
      (i32.const 0)
    )
  )
  (func $f2 (param $a i32) (result i32)
    (i32.div_u
      (get_local $a)
      (i32.const 0)
    )
  )
)

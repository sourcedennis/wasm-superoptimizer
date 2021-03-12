(module
  (func $f1 (param $a i32) (result i32)
    (i32.mul
      (get_local $a)
      (i32.const 2)
    )
  )
  (func $f2 (param $a i32) (result i32)
    (i32.shl
      (get_local $a)
      (i32.const 1)
    )
  )
)

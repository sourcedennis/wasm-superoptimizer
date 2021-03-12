;; Both these functions trap when the parameter $a is 0. Otherwise, they produce
;; 10/$a.
(module
  (func $f1 (param $a i32) (result i32)
    (i32.div_u
      (i32.const 10)
      (get_local $a)
    )
  )
  (func $f2 (param $a i32) (result i32)
    (i32.eqz
      (get_local $a)
    )
    if (result i32)
      unreachable
    else
      (i32.div_u
        (i32.const 10)
        (get_local $a)
      )
    end
  )
)

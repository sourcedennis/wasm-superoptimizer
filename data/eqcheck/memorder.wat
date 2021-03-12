;; Two function which modify memory, but in a different order.
(module
  (func $f1 (param $a i32)
    (i32.store
      (i32.const 0)
      (get_local $a)
    )
    (i32.store
      (i32.const 4)
      (i32.add
        (get_local $a)
        (i32.const 99)
      )
    )
  )
  
  (func $f2 (param $a i32)
    (i32.store
      (i32.const 4)
      (i32.add
        (get_local $a)
        (i32.const 99)
      )
    )
    (i32.store
      (i32.const 0)
      (get_local $a)
    )
  )

  (memory $memory (export "mem") 1)
)

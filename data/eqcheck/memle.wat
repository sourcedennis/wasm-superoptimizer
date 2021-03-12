;; Two function which modify memory, but in a different order.
;;
;; These are /not/ actually equal. Can this be found?
;; The first program writes $a as a 32-bit word, whose most-significant 3 bytes
;; are 0. The second program writes $a as a 8-bit word, which leaves the 3
;; succeeding bytes unchanged.
(module
  (func $f1 (param $a i32)
    (i32.store
      (i32.const 0)
      (i32.and
        (get_local $a)
        (i32.const 0xFF)
      )
    )
  )
  
  (func $f2 (param $a i32)
    (i32.store8
      (i32.const 0)
      (get_local $a)
    )
  )

  (memory $memory (export "mem") 1)
)

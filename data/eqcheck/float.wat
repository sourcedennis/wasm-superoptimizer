;; Both these functions trap when the parameter $a is 0. Otherwise, they produce
;; 10/$a.
(module
  (func $f1 (param $a f32) (result f32)
    (f32.add
      (f32.mul
        (get_local $a)
        (get_local $a)
      )
      (f32.mul
        (get_local $a)
        (get_local $a)
      )
    )
  )
  (func $f2 (param $a f32) (result f32)
    (f32.add
      (tee_local $a
        (f32.mul
          (get_local $a)
          (get_local $a)
        )
      )
      (get_local $a)
    )
  )
)

(module
  ;; (a+1)**4
  (func $f1 (param $a i32) (result i32) (local i32)
    (i32.mul
      (tee_local $a
        (i32.mul
          (tee_local $a
            (i32.add
              (get_local $a)
              (i32.const 1)
            )
          )
          (get_local $a)
        )
      )
      (get_local $a)
    )
  )
  ;; a^4 + 4*a^3 + 6*a^2 + 4*a + 1
  (func $f2 (param $a i32) (result i32) (local $b i32)
    (i32.add
      (i32.add
        ;; 1 + 4 * a
        (i32.add
          (i32.const 1)
          (i32.mul
            (i32.const 4)
            (get_local $a)
          )
        )
        ;; 6*a*a + 4*a*a*a
        (i32.add
          ;; 6*a*a
          (i32.mul
            (i32.const 6)
            ;; b = a*a
            (tee_local $b
              (i32.mul
                (get_local $a)
                (get_local $a)
              )
            )
          )
          ;; 4*a*a*a
          (i32.mul
            (i32.const 4)
            (tee_local $b
              (i32.mul
                ;; b := b*a = a*a*a
                (get_local $b)
                (get_local $a)
              )
            )
          )
        )
      )
      ;; a*a*a*a
      (i32.mul
        (get_local $a)
        (get_local $b)
      )
    )
  )

  ;; a*a*a*a + 4*a*a*a + 6*a*a + 4*a + 1
  ;;(func $f2 (param $a i32) (result i32) (local $b i32)
  ;;  (i32.add
  ;;    (i32.add
  ;;      ;; 1 + 4 * a
  ;;      (i32.add
  ;;        (i32.const 1)
  ;;        (i32.shl
  ;;          (get_local $a)
  ;;          (i32.const 2)
  ;;        )
  ;;      )
  ;;      ;; 6*a*a + 4*a*a*a
  ;;      (i32.add
  ;;        ;; 6*a*a
  ;;        (i32.add
  ;;          ;; 4*a*a
  ;;          (i32.shl
  ;;            ;; b = a*a
  ;;            (tee_local $b
  ;;              (i32.mul
  ;;                (get_local $a)
  ;;                (get_local $a)
  ;;              )
  ;;            )
  ;;            (i32.const 2)
  ;;          )
  ;;          ;; 2*a*a
  ;;          (i32.shl
  ;;            (get_local $b)
  ;;            (i32.const 1)
  ;;          )
  ;;        )
  ;;        ;; 4*a*a*a
  ;;        (i32.shl
  ;;          (tee_local $b
  ;;            (i32.mul
  ;;              ;; b := b*a = a*a*a
  ;;              (get_local $b)
  ;;              (get_local $a)
  ;;            )
  ;;          )
  ;;          (i32.const 2)
  ;;        )
  ;;      )
  ;;    )
  ;;    ;; a*a*a*a
  ;;    (i32.mul
  ;;      (get_local $a)
  ;;      (get_local $b)
  ;;    )
  ;;  )
  ;;)
)

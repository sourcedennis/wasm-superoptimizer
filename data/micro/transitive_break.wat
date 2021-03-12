(module
  (type $t0 (func (param i32 i32 i32) (result i32)))
  (func $run (export "run") (type $t0) (param $a i32) (param $b i32) (param $c i32) (result i32)
    (i32.gt_u
      (get_local $a)
      (get_local $b)
    )
    if (result i32)
      (i32.gt_u
        (get_local $b)
        (get_local $c)
      )
      if (result i32)
        (i32.le_u
          (get_local $a)
          (get_local $c)
        )
        if (result i32)
          (i32.const 2)
        else
          (i32.const 1)
        end
      else
        (i32.const 1)
      end
    else
      (i32.const 1)
    end
  )
)

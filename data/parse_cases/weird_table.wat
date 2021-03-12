(module
  (func $run (param $x i32) (result i32)
    (block $b0 (result i32)
      (block $b1 (result i32)
        (block $b2
          (block $b3 (result i32)
            i32.const 42
            local.get $x
            br_table $b0 $b1 $b3
          )
          i32.const 43
          br $b0
        )
        unreachable
      )
      i32.const 44
      br $b0
    )
  )
  (export "run" (func $run))
)
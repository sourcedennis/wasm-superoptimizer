(module
  (func (param $x i32) (result i32)
    i32.const 4
    i32.const 5
    (block $b0 (param i32) (result i32)
      local.get $x
      br_if $b0
      drop
      i32.const 7
    )
    (loop $l0 (param i32 i32) (result i32 i32)
      i32.add
      i32.const 6
      local.get $x
      br_if $l0
    )
  )
)
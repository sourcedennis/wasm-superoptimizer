(module
  (func $f0 (export "f0") (param $cond i32) (result i32)
    block $b0 (result i32)
      i32.const 5
      i32.const 6
      local.get $cond
      br_if $b0 ;; Keep 6, drop 5
      i32.add ;; 5+6
    end
  )

  (func $f1 (export "f1") (param $cond i32) (result i32)
    i32.const 5
    i32.const 6
    local.get $cond
    if $if0 (param i32 i32) (result i32)
      i32.add ;; 5+6
    else
      br $if0 ;; Keep 6, drop 5
    end
  )

  (func $f2 (export "f2") (param $cond i32) (result i32)
    i32.const 11 ;; if true
    i32.const 6  ;; if false
    local.get $cond
    select
  )
)
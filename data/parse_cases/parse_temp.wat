(module
  (func)
  (func)
  (func)
  (func)
  
  (table anyfunc (elem 0 1 2 3))
  (type $t0 (func (param f32 f64) (result i64)))
  (type $t1 (func (param f32 f64) (result f32 i64)))


  (func $multireturn (result f32 f64)
    (block (result i32)
      i64.const 4599384
      f32.const 453
      f64.const 4545
      return
    )
    drop
    f32.const 453
    f64.const 4545
  )
 )

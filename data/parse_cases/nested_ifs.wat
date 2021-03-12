(module
  (func (param $x i32) (result i32)
    (i32.const 43)
    (i32.const 44)
    if (param i32) (result i32)
      if (result i32)
        i32.const 45
      else
        i32.const 46
      end
    else
      if (result i32)
        i32.const 47
      else
        i32.const 48
      end
    end
  )
)
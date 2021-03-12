(module
  (func $i32add (result i32)
    i32.const 1
    i32.const 2
    i32.add
  )
  (func $i32popcnt (result i32)
    i32.const 67
    i32.clz
  )
  (func $i32eqz (result i32)
    i32.const 56
    i32.eqz
  )
  (func $i32ne (result i32)
    i32.const 567
    i32.const 412376
    i32.ne
  )
  (func $i64clz (result i64)
    i64.const 55
    i64.clz
  )
  (func $i64div (result i64)
    i64.const 1
    i64.const 2
    i64.div_s
  )
  (func $i64eqz (result i32)
    i64.const 12342
    i64.eqz
  )
  (func $i64ge_u (result i32)
    i32.const 346
    i32.const 451
    i32.ge_u
  )
  (func $f32abs (result f32)
    f32.const 834.45
    f32.abs
  )
  (func $f32copysign (result f32)
    f32.const 834.45
    f32.const -834.45
    f32.copysign
  )
  (func $f32lt (result i32)
    f32.const 567.3
    f32.const 8743
    f32.lt
  )
  (func $f64abs (result f64)
    f64.const 834.45
    f64.trunc
  )
  (func $f64div (result f64)
    f64.const 64.0
    f64.const -2.0
    f64.div
  )
  (func $f64lt (result i32)
    f64.const 753.1221
    f64.const -78.64
    f64.lt
  )

  (func $i32extend8 (result i32)
    i32.const 74
    i32.extend8_s
  )
  (func $i32extend16 (result i32)
    i32.const 3249
    i32.extend16_s
  )
  (func $i64extend8 (result i64)
    i64.const 723
    i64.extend8_s
  )
  (func $i64extend16 (result i64)
    i64.const 358
    i64.extend16_s
  )
  (func $i64extend32 (result i64)
    i64.const 358
    i64.extend32_s
  )
  (func $i32wrapi64 (result i32)
    i64.const 358
    i32.wrap/i64
  )
  (func $i64extendi32s (result i64)
    i32.const 42135
    i64.extend_s/i32
  )
  (func $i64extendi32u (result i64)
    i32.const 42135
    i64.extend_u/i32
  )
  (func $i32truncf32s (result i32)
    f32.const 4545.6
    i32.trunc_s/f32
  )
  (func $i32truncf64s (result i32)
    f64.const 4545.6
    i32.trunc_s/f64
  )
  (func $i64truncf32s (result i64)
    f32.const 4545.6
    i64.trunc_s/f32
  )
  (func $i64truncf64s (result i64)
    f64.const 4545.6
    i64.trunc_s/f64
  )
  (func $i32truncf32u (result i32)
    f32.const 4545.6
    i32.trunc_u/f32
  )
  (func $i32truncf64u (result i32)
    f64.const 4545.6
    i32.trunc_u/f64
  )
  (func $i64truncf32u (result i64)
    f32.const 4545.6
    i64.trunc_u/f32
  )
  (func $i64truncf64u (result i64)
    f64.const 4545.6
    i64.trunc_u/f64
  )
  (func $f32demotef64 (result f32)
    f64.const 3124.34
    f32.demote/f64
  )
  (func $f64promotef32 (result f64)
    f32.const 3124.34
    f64.promote/f32
  )

  (func $f32converti32s (result f32)
    i32.const 324132
    f32.convert_s/i32
  )
  (func $f32converti64s (result f32)
    i64.const 324132
    f32.convert_s/i64
  )
  (func $f64converti32u (result f64)
    i32.const 324132
    f64.convert_s/i32
  )
  (func $f64converti64u (result f64)
    i64.const 324132
    f64.convert_s/i64
  )
  (func $i32reinterpretf32 (result i32)
    f32.const 454
    i32.reinterpret/f32
  )
  (func $i64reinterpretf64 (result i64)
    f64.const 454
    i64.reinterpret/f64
  )

  (func $dropi32
    i32.const 132
    drop
  )
  (func $dropi64
    i64.const 321423
    drop
  )
  (func $dropf32
    f32.const 321423.342
    drop
  )
  (func $dropf64
    f64.const 321423.342
    drop
  )

  (func $selecti32 (result i32)
    i32.const 132
    i32.const 2314123
    i32.const 1
    select
  )
  (func $selecti64 (result i64)
    i64.const 132
    i64.const 2314123
    i32.const 1
    select
  )
  (func $selectf32 (result f32)
    f32.const 132
    f32.const 2314123
    i32.const 1
    select
  )
  (func $selectf64 (result f64)
    f64.const 132
    f64.const 2314123
    i32.const 1
    select
  )

  (func $localgeti32 (result i32) (local i32)
    get_local 0
  )
  (func $localgeti64 (result i64) (local i64)
    get_local 0
  )
  (func $localgetf32 (result f32) (local f32)
    get_local 0
  )
  (func $localgetf64 (result f64) (local f64)
    get_local 0
  )

  (func $localseti32 (local i32)
    i32.const 1234123
    set_local 0
  )
  (func $localseti64 (local i64)
    i64.const 1234123
    set_local 0
  )
  (func $localsetf32 (local f32)
    f32.const -12343.6
    set_local 0
  )
  (func $localsetf64 (local f64)
    f64.const 652.6732
    set_local 0
  )

  (global $gi32mut (mut i32) (i32.const 164))
  (global $gi64mut (mut i64) (i64.const 671))
  (global $gf32mut (mut f32) (f32.const 674.6))
  (global $gf64mut (mut f64) (f64.const 846.63))
  (global $gi32const i32 (i32.const 164))
  (global $gi64const i64 (i64.const 671))
  (global $gf32const f32 (f32.const 674.6))
  (global $gf64const f64 (f64.const 846.63))

  (func $globalgeti32const (result i32)
    get_global $gi32const
  )
  (func $globalgeti64const (result i64)
    get_global $gi64const
  )
  (func $globalgetf32const (result f32)
    get_global $gf32const
  )
  (func $globalgetf64const (result f64)
    get_global $gf64const
  )
  (func $globalgeti32mut (result i32)
    get_global $gi32mut
  )
  (func $globalgeti64mut (result i64)
    get_global $gi64mut
  )
  (func $globalgetf32mut (result f32)
    get_global $gf32mut
  )
  (func $globalgetf64mut (result f64)
    get_global $gf64mut
  )
  (func $globalseti32
    i32.const 1234123
    set_global $gi32mut
  )
  (func $globalseti64
    i64.const 1234123
    set_global $gi64mut
  )
  (func $globalsetf32
    f32.const -12343.6
    set_global $gf32mut
  )
  (func $globalsetf64
    f64.const 652.6732
    set_global $gf64mut
  )

  (memory 10 100)

  (func $i32load (result i32)
    i32.const 3242
    i32.load
  )
  (func $i64load (result i64)
    i32.const 3242
    i64.load
  )
  (func $f32load (result f32)
    i32.const 3242
    f32.load
  )
  (func $f64load (result f64)
    i32.const 3242
    f64.load
  )

  (func $i32store
    i32.const 3242
    i32.const 2314123
    i32.store
  )
  (func $i64store
    i32.const 3242
    i64.const 31432
    i64.store
  )
  (func $f32store
    i32.const 3242
    f32.const 3123
    f32.store
  )
  (func $f64store
    i32.const 3242
    f64.const 3123
    f64.store
  )

  (func $i32store8
    i32.const 3242
    i32.const 45
    i32.store8
  )
  (func $i32store16
    i32.const 3242
    i32.const 45
    i32.store16
  )
  (func $i64store8
    i32.const 3242
    i64.const 45
    i64.store8
  )
  (func $i64store16
    i32.const 3242
    i64.const 45
    i64.store16
  )
  (func $i64store32
    i32.const 3242
    i64.const 45
    i64.store32
  )

  (func $memorysize (result i32)
    current_memory
  )
  (func $memorygrow (result i32)
    i32.const 5
    grow_memory
  )

  (func $nop
    nop
  )
  (func $unreachable1 (result i32)
    unreachable
  )
  (func $unreachable2 (result f32)
    unreachable
  )
  (func $unreachable3 (result f32)
    i32.const 56
    i64.const 4134
    unreachable
  )

  (func $block1 (result f32)
    (block (result f32)
      f32.const 4
    )
  )

  (func $block2 (result f32)
    (block (result f32)
      f32.const 45143
      f32.const 76
      i32.const 5
      br_if 0
      drop
    )
  )

  (func $block3 (result f32)
    (block (result f32)
      f64.const 45143
      i64.const 3142
      f32.const 76
      br 0
    )
  )

  (func $block4 (result f32)
    (block (result f32)
      f64.const 45143
      f32.const 384387
      i32.const 3487
      i64.const 3142
      f32.const 76
      br 0
      i32.const 34123
      drop
      f32.const 344132
    )
  )

  (func $block5 (result f32)
    (block (result f32)
      f64.const 45143
      f32.const 384387
      i32.const 3487
      i64.const 3142
      f32.const 76
      i32.const 3433
      br_if 0
      drop
      drop
      drop
      drop
      drop
      f32.const 344132
    )
  )
  
  (func $multiparam (result f32 f64)
    f32.const 31432.3
    f64.const 3433.2
  )
  
  (func $multiparamblock1 (result i32 f32)
    i32.const 31432
    f64.const 3433.2
    (block (param i32 f64) (result i32 f32)
      drop
      f32.const 343.32
    )
  )
  
  (func $multiparamblock2 (result i64 f32 f64)
    i64.const 343
    i32.const 3413
    i64.const 35641
    (block (param i32 i64) (result f32 f64 f64)
      drop
      f32.reinterpret/i32
      f64.const 3432.3
      f64.const 35613.3
    )
    f64.add
  )

  (func $if1 (result f32)
    f32.const 33243.34
    i32.const 31432
    if (result f32)
      f32.const 4
    else
      f32.const 76
    end
    f32.add
  )

  (func $if2 (result f32)
    f32.const 33243.34
    i32.const 31432
    if (result f32)
      f32.const 4
    else
      f32.const 83
      f32.const 76
      i32.const 31432
      br_if 0
      drop
    end
    f32.add
  )
  
  (func $multiparamif1 (result f32 f64)
    i32.const 435
    i32.const 2
    if (param i32) (result f32 f64)
      drop
      f32.const 343.56
      f64.const 391
    else
      drop
      f32.const 343.56
      f64.const 391
    end
  )
  
  (func $multiparamif2 (result i64 f32 f64)
    i64.const 343
    i32.const 3413
    i64.const 35641
    i32.const 1
    if (param i32 i64) (result f32 f64 f64)
      drop
      f32.reinterpret/i32
      f64.const 3432.3
      f64.const 35613.3
    else
      i32.wrap/i64
      i32.add
      f32.convert_u/i32
      f64.const 34123.3456
      f64.const 349028.3
    end
    f64.add
  )

  (func $loop1 (result f32)
    (loop (result f32)
      f32.const 12432
    )
  )

  (func $loop2 (result f32)
    (loop (result f32)
      f32.const 324
      i32.const 0
      br_if 0
    )
  )

  (func $loop3 (result f32)
    (loop (result f32)
      f32.const 324
      f64.const 31432
      f32.const 3414523
      i32.const 0
      br_if 0
      drop
      drop
    )
  )

  (func $loop4 (result f32)
    (loop (result f32)
      f32.const 3414523
      br 0
      f64.const 1234132
      drop
      f32.const 4
    )
  )
  
  (func $multiparamloop (result f64 f32)
    f64.const 3435.5
    i64.const 343
    loop (param i64) (result f32)
      i64.const 1
      i64.add
      i32.const 5
      br_if 0
      f32.convert_u/i64
    end
  )
  
  (func $multiparamloop2 (result f64 f32)
    f64.const 3435.5
    i64.const 343
    loop (param i64) (result f32)
      i32.const 5
      br_if 0
      f32.convert_u/i64
    end
  )

  (func $brtable (result f32)
    (block (result f32)
      (block (result f32)
        (block (result f32)
          f32.const 313
          i32.const 3
          br_table 0 1 2
          f32.const 34123
        )
      )
    )
  )

  (func $return (result f32)
    (block (result i32)
      f32.const 453
      return
    )
    drop
    f32.const 4653
  )

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

  (func $called (param f32 i64))
  (func $multicalled (param f32 f64) (result f64 f32)
    get_local 1
    get_local 0
  )

  (func $call1
    f32.const 34123
    i64.const 3143
    call $called
  )

  (func $callmulti (result i64 f64)
    i64.const 8347
    f32.const 341324
    f64.const 3493
    call $multicalled
    f64.promote/f32
    f64.mul
  )
  
  (table anyfunc (elem 0 1 2 3))
  (type $t0 (func (param f32 f64) (result i64)))
  (type $t1 (func (param f32 f64) (result f32 i64)))

  (func $callindirect (result i64)
    f32.const 3143.3
    f64.const 3214132.7
    i32.const 5
    call_indirect (type $t0)
  )
  
  (func $callindirectmulti (result f32)
    f32.const 3143.3
    f64.const 3214132.7
    i32.const 5
    call_indirect (type $t1)
    drop
  )
)

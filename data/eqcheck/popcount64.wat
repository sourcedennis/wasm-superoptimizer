(module
  (func $f1 (param $p0 i64) (result i64)
    (local $l0 i64)
    get_local $p0
    i64.eqz
    i32.eqz
    if $I0
      loop $L1
        get_local $p0
        i64.const 1
        i64.and
        get_local $l0
        i64.add
        set_local $l0
        get_local $p0
        i64.const 1
        i64.shr_u
        tee_local $p0
        i64.const 0
        i64.ne
        br_if $L1
      end
      get_local $l0
      return
    end
    i64.const 0
  )
  (func $f2 (param $p0 i64) (result i64)
    (local $l0 i64)
    (i64.popcnt
      (get_local $p0)
    )
  )
)
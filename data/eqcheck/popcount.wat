(module
  (func $f1 (param $p0 i32) (result i32)
    (local $l0 i32) (local $l1 i32)
    get_local $p0
    if $I0
      loop $L1
        get_local $p0
        i32.const 1
        i32.and
        get_local $l0
        i32.add
        set_local $l0
        get_local $p0
        i32.const 1
        i32.shr_u
        tee_local $l1
        set_local $p0
        get_local $l1
        br_if $L1
      end
      get_local $l0
      return
    end
    i32.const 0
  )
  (func $f2 (param $p0 i32) (result i32)
    (local $l0 i32) (local $l1 i32)
    (i32.popcnt
      (get_local $p0)
    )
  )
)
(module
  (func $f1 (result i32)
    (local $l0 i32)
    i32.const -1
    set_local $l0
    loop $L0
      get_local $l0
      i32.const 1
      i32.add
      tee_local $l0
      get_local $l0
      i32.mul
      i32.const 1000000
      i32.rem_s
      i32.const 269696
      i32.ne
      br_if $L0
    end
    get_local $l0
  )
  (func $f2 (result i32)
    (local $l0 i32)
    (i32.const 25264)
  )
)
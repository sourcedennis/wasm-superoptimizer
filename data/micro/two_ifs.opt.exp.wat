(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (func $run (type 0) (param i32) (result i32)
    i32.const 42
    local.get 0
    if (param i32) (result i32)  ;; label = @1
	  drop
	  i32.const 294
    else
	  drop
	  i32.const 141
    end
  )
  (func $runall (export "runall") (type 0) (param $a i32) (result i32)
    (local $l0 i32) (local $l1 i32)
    i32.const 100000
    set_local $l0
    loop $L0 (result i32)
      get_local $a
      call $run
      set_local $l1
      get_local $l0
      i32.const 1
      i32.sub
      tee_local $l0
      br_if $L0
      get_local $l1
    end
  )
)

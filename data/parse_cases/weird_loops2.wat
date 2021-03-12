;; A nonsensical module. Don't run this. It was written to analyse nested-loops.
(module
  (func $run (export "run") (result i32)
    i32.const 44
    loop $l0
      i32.const 45
      loop $l1
        i32.const 46
        br_if $l0
        i32.const 47
        br_if $l1
      end
      block (param i32)
        br 0
      end
    end
    block (param i32)
      br 0
    end
    i32.const 48
  )

  (func $run2 (export "run2") (result i32)
    i32.const 44
    loop $l0 (result i32)
      i32.const 45
      loop $l1
        i32.const 46
        br_if $l0
        i32.const 47
        br_if $l1
      end
    end
    drop
    drop
    i32.const 48
  )
)

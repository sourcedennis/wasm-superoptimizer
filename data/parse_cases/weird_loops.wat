;; A nonsensical module. Don't run this. It was written to analyse nested-loops.
(module
  (func $run (export "run") (result i32)
    i32.const 42
    loop $l0 (param i32) (result i32 i32)
      i32.const 11
      i32.add
      block $b0 (result i32)
        i32.const 99
        i32.const 42
        br_if $b0
        i32.const 44
        loop $l1 (param i32 i32) (result f32)
          br_if $l0 ;; repeat the outer loop
          i32.const 44
          i32.const 32
          br_if $l1 ;; repeat the inner loop
          br_if $b0 ;; go to the end of the block
          f32.reinterpret_i32
        end
        i32.reinterpret_f32
        i32.const 33
        br_if $l0 ;; repeat the outer loop
      end
    end
    i32.add
  )
)

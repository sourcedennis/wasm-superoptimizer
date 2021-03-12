;; static mut DATA: [u32; 5] = [0; 5];

;; #[no_mangle]
;; pub extern "C" fn data( ) -> *mut u32 {
;;   unsafe {
;;     DATA.as_mut_ptr( )
;;   }
;; }


;; #[no_mangle]
;; pub extern "C" fn run( ) -> u32 {
;;   unsafe {
;;     let mut sum = 0;

;;     for x in &DATA {
;;       sum += x;
;;     }
;;     for x in &DATA {
;;       sum += x;
;;     }

;;     sum
;;   }
;; }

(module
  (func $f1 (export "run1") (result i32)
    (local $l0 i32) (local $l1 i32) (local $l2 i32)
    i32.const 0
    set_local $l0
    i32.const -20
    set_local $l1
    loop $L0
      get_local $l1
      i32.const 1044
      i32.add
      i32.load
      get_local $l0
      i32.add
      set_local $l0
      get_local $l1
      i32.const 4
      i32.add
      tee_local $l2
      set_local $l1
      get_local $l2
      br_if $L0
    end
    i32.const -20
    set_local $l1
    loop $L1
      get_local $l1
      i32.const 1044
      i32.add
      i32.load
      get_local $l0
      i32.add
      set_local $l0
      get_local $l1
      i32.const 4
      i32.add
      tee_local $l2
      set_local $l1
      get_local $l2
      br_if $L1
    end
    get_local $l0)
  (func $f2 (export "run") (result i32)
    (local $idx i32) (local $unknown0 i32) (local $unknown1 i32)
    (i32.const 1020)
    (set_local $idx)
    (i32.const 0)
    loop $L0 (param i32) (result i32)
      (i32.load
        (tee_local $idx
          (i32.add
            (get_local $idx)
            (i32.const 4)
          )
        )
      )
      (i32.add)
      (get_local $idx)
      (i32.const 1040)
      (i32.lt_u)
      (br_if $L0)
    end
    i32.const 1
    i32.shl
  )
  (func $data (export "data") (result i32)
    i32.const 1024
  )
  (memory $memory (export "memory") 17)
  (data (i32.const 1024) "\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00")
)

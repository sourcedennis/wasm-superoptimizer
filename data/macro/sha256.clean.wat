(module
  (type (;0;) (func))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32 i32 i32)))
  (type (;3;) (func (param i32 i32 i32) (result i32)))
  (func (;0;) (type 1) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 256
    i32.sub
    local.tee 2
    global.set 0
    i32.const 0
    local.set 3
    loop  ;; label = @1
      local.get 2
      local.get 3
      i32.add
      local.get 1
      local.get 3
      i32.add
      i32.load align=1
      local.tee 4
      i32.const 24
      i32.shl
      local.get 4
      i32.const 8
      i32.shl
      i32.const 16711680
      i32.and
      i32.or
      local.get 4
      i32.const 8
      i32.shr_u
      i32.const 65280
      i32.and
      local.get 4
      i32.const 24
      i32.shr_u
      i32.or
      i32.or
      i32.store
      local.get 3
      i32.const 4
      i32.add
      local.tee 3
      i32.const 64
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 1
    local.get 2
    i32.load
    local.set 5
    loop  ;; label = @1
      local.get 2
      local.get 1
      i32.add
      local.tee 3
      i32.const 64
      i32.add
      local.get 3
      i32.const 56
      i32.add
      i32.load
      local.tee 4
      i32.const 13
      i32.rotl
      local.get 4
      i32.const 15
      i32.rotl
      i32.xor
      local.get 4
      i32.const 10
      i32.shr_u
      i32.xor
      local.get 3
      i32.const 4
      i32.add
      i32.load
      local.tee 4
      i32.const 14
      i32.rotl
      local.get 4
      i32.const 25
      i32.rotl
      i32.xor
      local.get 4
      i32.const 3
      i32.shr_u
      i32.xor
      i32.add
      local.get 5
      local.get 3
      i32.const 36
      i32.add
      i32.load
      i32.add
      i32.add
      i32.store
      local.get 4
      local.set 5
      local.get 1
      i32.const 4
      i32.add
      local.tee 1
      i32.const 192
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 5
    local.get 0
    i32.load offset=80
    local.tee 6
    local.set 7
    local.get 0
    i32.const 84
    i32.add
    i32.load
    local.tee 8
    local.set 4
    local.get 0
    i32.const 108
    i32.add
    i32.load
    local.tee 9
    local.set 10
    local.get 0
    i32.const 104
    i32.add
    i32.load
    local.tee 11
    local.set 12
    local.get 0
    i32.const 100
    i32.add
    i32.load
    local.tee 13
    local.set 14
    local.get 0
    i32.const 96
    i32.add
    i32.load
    local.tee 15
    local.set 16
    local.get 0
    i32.const 92
    i32.add
    i32.load
    local.tee 17
    local.set 18
    local.get 0
    i32.const 88
    i32.add
    i32.load
    local.tee 19
    local.set 1
    loop  ;; label = @1
      local.get 1
      local.set 20
      local.get 4
      local.set 1
      local.get 7
      local.tee 4
      i32.const 30
      i32.rotl
      local.get 4
      i32.const 19
      i32.rotl
      i32.xor
      local.get 4
      i32.const 10
      i32.rotl
      i32.xor
      local.get 4
      local.get 1
      local.get 20
      i32.xor
      i32.and
      local.get 1
      local.get 20
      i32.and
      i32.xor
      i32.add
      local.get 16
      local.tee 3
      i32.const 26
      i32.rotl
      local.get 3
      i32.const 21
      i32.rotl
      i32.xor
      local.get 3
      i32.const 7
      i32.rotl
      i32.xor
      local.get 10
      i32.add
      local.get 12
      local.tee 21
      local.get 3
      i32.const -1
      i32.xor
      i32.and
      local.get 14
      local.tee 22
      local.get 3
      i32.and
      i32.or
      i32.add
      local.get 5
      i32.const 1024
      i32.add
      i32.load
      i32.add
      local.get 2
      local.get 5
      i32.add
      i32.load
      i32.add
      local.tee 16
      i32.add
      local.set 7
      local.get 16
      local.get 18
      i32.add
      local.set 16
      local.get 21
      local.set 10
      local.get 22
      local.set 12
      local.get 3
      local.set 14
      local.get 20
      local.set 18
      local.get 5
      i32.const 4
      i32.add
      local.tee 5
      i32.const 256
      i32.ne
      br_if 0 (;@1;)
    end
    local.get 0
    i32.const 84
    i32.add
    local.get 4
    local.get 8
    i32.add
    i32.store
    local.get 0
    i32.const 80
    i32.add
    local.get 7
    local.get 6
    i32.add
    i32.store
    local.get 0
    i32.const 88
    i32.add
    local.get 1
    local.get 19
    i32.add
    i32.store
    local.get 0
    i32.const 92
    i32.add
    local.get 20
    local.get 17
    i32.add
    i32.store
    local.get 0
    i32.const 96
    i32.add
    local.get 16
    local.get 15
    i32.add
    i32.store
    local.get 0
    i32.const 100
    i32.add
    local.get 3
    local.get 13
    i32.add
    i32.store
    local.get 0
    i32.const 104
    i32.add
    local.get 22
    local.get 11
    i32.add
    i32.store
    local.get 0
    i32.const 108
    i32.add
    local.get 21
    local.get 9
    i32.add
    i32.store
    local.get 2
    i32.const 256
    i32.add
    global.set 0)
  (func (;1;) (type 1) (param i32 i32)
    (local i32 i32 i64 i32 i32 i32 i32 i32)
    local.get 0
    local.get 0
    i32.load offset=64
    local.tee 2
    i32.add
    i32.const 128
    i32.store8
    local.get 2
    i32.const 1
    i32.add
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.const 56
        i32.ge_u
        br_if 0 (;@2;)
        local.get 3
        i32.const 55
        i32.gt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 3
        i32.add
        i32.const 0
        i32.const 55
        local.get 2
        i32.sub
        call 3
        drop
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 3
        i32.const 63
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.get 3
        i32.add
        i32.const 0
        i32.const 63
        local.get 2
        i32.sub
        call 3
        drop
      end
      local.get 0
      local.get 0
      call 0
      local.get 0
      i32.const 48
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i32.const 40
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i32.const 32
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i32.const 24
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i32.const 16
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i32.const 8
      i32.add
      i64.const 0
      i64.store
      local.get 0
      i64.const 0
      i64.store
    end
    local.get 0
    local.get 0
    i64.load offset=72
    local.get 0
    i32.const 64
    i32.add
    i32.load
    i32.const 3
    i32.shl
    i64.extend_i32_u
    i64.add
    local.tee 4
    i64.store offset=72
    local.get 0
    local.get 4
    i64.store8 offset=63
    local.get 0
    local.get 4
    i64.const 8
    i64.shr_u
    i64.store8 offset=62
    local.get 0
    local.get 4
    i64.const 16
    i64.shr_u
    i64.store8 offset=61
    local.get 0
    local.get 4
    i64.const 24
    i64.shr_u
    i64.store8 offset=60
    local.get 0
    local.get 4
    i64.const 32
    i64.shr_u
    i64.store8 offset=59
    local.get 0
    local.get 4
    i64.const 40
    i64.shr_u
    i64.store8 offset=58
    local.get 0
    local.get 4
    i64.const 48
    i64.shr_u
    i64.store8 offset=57
    local.get 0
    local.get 4
    i64.const 56
    i64.shr_u
    i64.store8 offset=56
    local.get 0
    local.get 0
    call 0
    local.get 1
    local.get 0
    i32.const 83
    i32.add
    i32.load8_u
    i32.store8
    local.get 1
    local.get 0
    i32.const 87
    i32.add
    i32.load8_u
    i32.store8 offset=4
    local.get 1
    local.get 0
    i32.const 91
    i32.add
    i32.load8_u
    i32.store8 offset=8
    local.get 1
    local.get 0
    i32.const 95
    i32.add
    i32.load8_u
    i32.store8 offset=12
    local.get 1
    local.get 0
    i32.const 99
    i32.add
    i32.load8_u
    i32.store8 offset=16
    local.get 1
    local.get 0
    i32.const 103
    i32.add
    i32.load8_u
    i32.store8 offset=20
    local.get 1
    local.get 0
    i32.const 107
    i32.add
    i32.load8_u
    i32.store8 offset=24
    local.get 1
    local.get 0
    i32.const 111
    i32.add
    i32.load8_u
    i32.store8 offset=28
    local.get 1
    local.get 0
    i32.const 82
    i32.add
    i32.load16_u
    i32.store8 offset=1
    local.get 1
    local.get 0
    i32.const 86
    i32.add
    i32.load16_u
    i32.store8 offset=5
    local.get 1
    local.get 0
    i32.const 90
    i32.add
    i32.load16_u
    i32.store8 offset=9
    local.get 1
    local.get 0
    i32.const 94
    i32.add
    i32.load16_u
    i32.store8 offset=13
    local.get 1
    local.get 0
    i32.const 98
    i32.add
    i32.load16_u
    i32.store8 offset=17
    local.get 1
    local.get 0
    i32.const 102
    i32.add
    i32.load16_u
    i32.store8 offset=21
    local.get 1
    local.get 0
    i32.const 106
    i32.add
    i32.load16_u
    i32.store8 offset=25
    local.get 1
    local.get 0
    i32.const 110
    i32.add
    i32.load16_u
    i32.store8 offset=29
    local.get 1
    local.get 0
    i32.load offset=80
    i32.const 8
    i32.shr_u
    i32.store8 offset=2
    local.get 1
    local.get 0
    i32.const 84
    i32.add
    local.tee 2
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=6
    local.get 1
    local.get 0
    i32.const 88
    i32.add
    local.tee 3
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=10
    local.get 1
    local.get 0
    i32.const 92
    i32.add
    local.tee 5
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=14
    local.get 1
    local.get 0
    i32.const 96
    i32.add
    local.tee 6
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=18
    local.get 1
    local.get 0
    i32.const 100
    i32.add
    local.tee 7
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=22
    local.get 1
    local.get 0
    i32.const 104
    i32.add
    local.tee 8
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=26
    local.get 1
    local.get 0
    i32.const 108
    i32.add
    local.tee 9
    i32.load
    i32.const 8
    i32.shr_u
    i32.store8 offset=30
    local.get 1
    local.get 0
    i32.load offset=80
    i32.store8 offset=3
    local.get 1
    local.get 2
    i32.load
    i32.store8 offset=7
    local.get 1
    local.get 3
    i32.load
    i32.store8 offset=11
    local.get 1
    local.get 5
    i32.load
    i32.store8 offset=15
    local.get 1
    local.get 6
    i32.load
    i32.store8 offset=19
    local.get 1
    local.get 7
    i32.load
    i32.store8 offset=23
    local.get 1
    local.get 8
    i32.load
    i32.store8 offset=27
    local.get 1
    local.get 9
    i32.load
    i32.store8 offset=31)
  (func (;2;) (type 2) (param i32 i32 i32)
    (local i32 i32 i32 i32)
    global.get 0
    i32.const 112
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    i32.const 88
    i32.add
    i64.const -6534734903820487822
    i64.store
    local.get 3
    i32.const 96
    i32.add
    i64.const -7276294671082564993
    i64.store
    local.get 3
    i32.const 104
    i32.add
    i64.const 6620516960021240235
    i64.store
    local.get 3
    i64.const 0
    i64.store offset=72
    i32.const 0
    local.set 4
    local.get 3
    i32.const 0
    i32.store offset=64
    local.get 3
    i64.const -4942790177982912921
    i64.store offset=80
    block  ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.const 72
      i32.add
      local.set 5
      loop  ;; label = @2
        local.get 3
        local.get 4
        i32.add
        local.get 0
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 64
        i32.add
        local.tee 6
        local.get 6
        i32.load
        i32.const 1
        i32.add
        local.tee 4
        i32.store
        block  ;; label = @3
          local.get 4
          i32.const 64
          i32.ne
          br_if 0 (;@3;)
          local.get 3
          local.get 3
          call 0
          i32.const 0
          local.set 4
          local.get 6
          i32.const 0
          i32.store
          local.get 5
          local.get 5
          i64.load
          i64.const 512
          i64.add
          i64.store
        end
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 1
        i32.const -1
        i32.add
        local.tee 1
        br_if 0 (;@2;)
      end
    end
    local.get 3
    local.get 2
    call 1
    local.get 3
    i32.const 112
    i32.add
    global.set 0)
  (func (;3;) (type 3) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i64)
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8
      local.get 0
      local.get 2
      i32.add
      local.tee 3
      i32.const -1
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 3
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=2
      local.get 0
      local.get 1
      i32.store8 offset=1
      local.get 3
      i32.const -3
      i32.add
      local.get 1
      i32.store8
      local.get 3
      i32.const -2
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 7
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=3
      local.get 3
      i32.const -4
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i32.const 0
      local.get 0
      i32.sub
      i32.const 3
      i32.and
      local.tee 4
      i32.add
      local.tee 3
      local.get 1
      i32.const 255
      i32.and
      i32.const 16843009
      i32.mul
      local.tee 1
      i32.store
      local.get 3
      local.get 2
      local.get 4
      i32.sub
      i32.const -4
      i32.and
      local.tee 4
      i32.add
      local.tee 2
      i32.const -4
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=8
      local.get 3
      local.get 1
      i32.store offset=4
      local.get 2
      i32.const -8
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -12
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 25
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=16
      local.get 3
      local.get 1
      i32.store offset=12
      local.get 3
      local.get 1
      i32.store offset=20
      local.get 3
      local.get 1
      i32.store offset=24
      local.get 2
      i32.const -24
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -28
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -20
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -16
      i32.add
      local.get 1
      i32.store
      local.get 4
      local.get 3
      i32.const 4
      i32.and
      i32.const 24
      i32.or
      local.tee 5
      i32.sub
      local.tee 2
      i32.const 32
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      i64.extend_i32_u
      local.tee 6
      i64.const 32
      i64.shl
      local.get 6
      i64.or
      local.set 6
      local.get 3
      local.get 5
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 6
        i64.store
        local.get 1
        i32.const 8
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 16
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 24
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 32
        i32.add
        local.set 1
        local.get 2
        i32.const -32
        i32.add
        local.tee 2
        i32.const 31
        i32.gt_u
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (table (;0;) 1 1 anyfunc)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66816))
  (global (;1;) i32 (i32.const 66816))
  (global (;2;) i32 (i32.const 1280))
  (export "memory" (memory 0))
  (export "__heap_base" (global 1))
  (export "__data_end" (global 2))
  (export "run" (func 2))
  (data (;0;) (i32.const 1024) "\98/\8aB\91D7q\cf\fb\c0\b5\a5\db\b5\e9[\c2V9\f1\11\f1Y\a4\82?\92\d5^\1c\ab\98\aa\07\d8\01[\83\12\be\851$\c3}\0cUt]\ber\fe\b1\de\80\a7\06\dc\9bt\f1\9b\c1\c1i\9b\e4\86G\be\ef\c6\9d\c1\0f\cc\a1\0c$o,\e9-\aa\84tJ\dc\a9\b0\5c\da\88\f9vRQ>\98m\c61\a8\c8'\03\b0\c7\7fY\bf\f3\0b\e0\c6G\91\a7\d5Qc\ca\06g))\14\85\0a\b7'8!\1b.\fcm,M\13\0d8STs\0ae\bb\0ajv.\c9\c2\81\85,r\92\a1\e8\bf\a2Kf\1a\a8p\8bK\c2\a3Ql\c7\19\e8\92\d1$\06\99\d6\855\0e\f4p\a0j\10\16\c1\a4\19\08l7\1eLwH'\b5\bc\b04\b3\0c\1c9J\aa\d8NO\ca\9c[\f3o.h\ee\82\8ftoc\a5x\14x\c8\84\08\02\c7\8c\fa\ff\be\90\eblP\a4\f7\a3\f9\be\f2xq\c6"))

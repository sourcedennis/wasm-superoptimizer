(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func (param i32) (result i32)))
  (type (;4;) (func (param i32 i32)))
  (type (;5;) (func (param i32 i32) (result i32)))
  (type (;6;) (func (param i32 i32 i32)))
  (type (;7;) (func (param i32 i32 i32) (result i32)))
  (type (;8;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;9;) (func (param i32 i64)))
  (import "env" "luaL_newstate" (func (;0;) (type 1)))
  (import "env" "lua_pushcclosure" (func (;1;) (type 6)))
  (import "env" "lua_pushinteger" (func (;2;) (type 9)))
  (import "env" "lua_pushlightuserdata" (func (;3;) (type 4)))
  (import "env" "lua_pcallk" (func (;4;) (type 8)))
  (import "env" "lua_toboolean" (func (;5;) (type 5)))
  (import "env" "lua_close" (func (;6;) (type 2)))
  (import "env" "fprintf" (func (;7;) (type 7)))
  (import "env" "fflush" (func (;8;) (type 3)))
  (import "env" "lua_settop" (func (;9;) (type 4)))
  (import "env" "lua_tolstring" (func (;10;) (type 7)))
  (func (;11;) (type 0))
  (func (;12;) (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    i32.const 0
    i32.store offset=28
    local.get 2
    local.get 0
    i32.store offset=24
    local.get 2
    local.get 1
    i32.store offset=20
    local.get 2
    call 0
    i32.store offset=8
    block  ;; label = @1
      local.get 2
      i32.load offset=8
      i32.eqz
      if  ;; label = @2
        local.get 2
        i32.load offset=20
        i32.load
        i32.const 1024
        call 13
        local.get 2
        i32.const 1
        i32.store offset=28
        br 1 (;@1;)
      end
      i32.const 0
      local.set 1
      local.get 2
      i32.load offset=8
      i32.const 1
      i32.const 0
      call 1
      local.get 2
      i32.load offset=8
      local.get 2
      i32.load offset=24
      i64.extend_i32_s
      call 2
      local.get 2
      i32.load offset=8
      local.get 2
      i32.load offset=20
      call 3
      local.get 2
      local.get 2
      i32.load offset=8
      i32.const 2
      i32.const 1
      i32.const 0
      i32.const 0
      i32.const 0
      call 4
      i32.store offset=16
      local.get 2
      local.get 2
      i32.load offset=8
      i32.const -1
      call 5
      i32.store offset=12
      local.get 2
      i32.load offset=8
      local.get 2
      i32.load offset=16
      call 14
      drop
      local.get 2
      i32.load offset=8
      call 6
      local.get 2
      i32.const 0
      i32.const 1
      local.get 2
      i32.load offset=12
      if (result i32)  ;; label = @2
        local.get 2
        i32.load offset=16
        i32.eqz
      else
        i32.const 0
      end
      i32.const 1
      i32.and
      select
      i32.store offset=28
    end
    local.get 2
    i32.load offset=28
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;13;) (type 4) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.store offset=28
    local.get 2
    local.get 1
    i32.store offset=24
    local.get 2
    i32.load offset=28
    if  ;; label = @1
      local.get 2
      local.get 2
      i32.load offset=28
      i32.store offset=16
      i32.const 0
      i32.load
      i32.const 1412
      local.get 2
      i32.const 16
      i32.add
      call 7
      drop
      i32.const 0
      i32.load
      call 8
      drop
    end
    local.get 2
    local.get 2
    i32.load offset=24
    i32.store
    i32.const 0
    i32.load
    i32.const 1857
    local.get 2
    call 7
    drop
    i32.const 0
    i32.load
    call 8
    drop
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;14;) (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.store offset=12
    local.get 2
    local.get 1
    i32.store offset=8
    local.get 2
    i32.load offset=8
    if  ;; label = @1
      local.get 2
      local.get 2
      i32.load offset=12
      i32.const -1
      i32.const 0
      call 10
      i32.store offset=4
      i32.const 1864
      i32.load
      local.get 2
      i32.load offset=4
      call 13
      local.get 2
      i32.load offset=12
      i32.const -2
      call 9
    end
    local.get 2
    i32.load offset=8
    local.get 2
    i32.const 16
    i32.add
    global.set 0)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 67408))
  (global (;1;) i32 (i32.const 67408))
  (global (;2;) i32 (i32.const 1872))
  (global (;3;) i32 (i32.const 1024))
  (export "memory" (memory 0))
  (export "__wasm_call_ctors" (func 11))
  (export "__heap_base" (global 1))
  (export "__data_end" (global 2))
  (export "__dso_handle" (global 3))
  (export "main" (func 12))
  (data (;0;) (i32.const 1024) "cannot create state: not enough memory\00lua\00LUA_NOENV\00\0a\00too many results to print\00print\00error calling 'print' (%s)\00interrupted!\00__tostring\00(error object is a %s value)\00=stdin\00<eof>\00return %s;\00return %s\00_PROMPT\00_PROMPT2\00> \00>> \00-\00--\00arg\00'arg' is not a table\00too many arguments to script\00=(command line)\00@on\00require\00=LUA_INIT_5_4\00=LUA_INIT\00Lua 5.4.0  Copyright (C) 1994-2019 Lua.org, PUC-Rio\00%s: \00'%s' needs argument\0a\00unrecognized option '%s'\0a\00usage: %s [options] [script [args]]\0aAvailable options are:\0a  -e stat  execute string 'stat'\0a  -i       enter interactive mode after executing 'script'\0a  -l name  require library 'name' into global 'name'\0a  -v       show version information\0a  -E       ignore environment variables\0a  -W       turn warnings on\0a  --       stop handling options\0a  -        stop handling options and execute stdin\0a\00%s\0a")
  (data (;1;) (i32.const 1864) "'\04"))

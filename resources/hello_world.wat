(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (param i32)))
  (type (;4;) (func))
  (import "env" "puts" (func (;0;) (type 0)))
  (func (;1;) (type 1) (result i32)
    i32.const 1024
    call 3
    i32.const 1037
    call 3
    i32.const 0)
  (func (;2;) (type 2) (param i32 i32) (result i32)
    call 1)
  (func (;3;) (type 3) (param i32)
    local.get 0
    call 0
    drop)
  (func (;4;) (type 4) 
        i32.const 0
        i32.const 1
        call 2
        drop)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 5152))
  (start 4)
  (export "memory" (memory 0))
  (export "main" (func 2))
  (data (;0;) (i32.const 1024) "Hello World!\00Wasm Micro Runtime\00"))

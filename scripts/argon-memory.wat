;; Small bridge module for the two standalone WASM modules used by Argon.
;;
;; The host must instantiate this module with the exported Z3 memory:
;;
;;   { z3: { memory: z3Memory } }
;;
;; The argonc memory is a dummy import while this source is compiled. The
;; distribution post-processing step replaces that import with a declared and
;; exported memory, using argonc's original memory limits. Each copy function
;; takes a source offset, destination offset, and byte length.
;;
;; This module uses the WebAssembly multi-memory and bulk-memory features.

(module
  (import "argonc" "memory" (memory $argonc 0))
  (import "z3" "memory" (memory $z3 0))

  (func $copy_argonc_to_z3
    (param $source i32)
    (param $destination i32)
    (param $length i32)
    (memory.copy $z3 $argonc
      (local.get $destination)
      (local.get $source)
      (local.get $length)))

  (func $copy_z3_to_argonc
    (param $source i32)
    (param $destination i32)
    (param $length i32)
    (memory.copy $argonc $z3
      (local.get $destination)
      (local.get $source)
      (local.get $length)))

  ;; Return the byte length of a NUL-terminated string in Z3 memory.
  (func $z3_string_length
    (param $source i32)
    (result i32)
    (local $length i32)
    (local.set $length (i32.const 0))
    (block $done
      (loop $scan
        (br_if $done
          (i32.eqz
            (i32.load8_u $z3
              (i32.add (local.get $source) (local.get $length)))))
        (local.set $length
          (i32.add (local.get $length) (i32.const 1)))
        (br $scan)))
    (local.get $length))

  ;; Copy a NUL-terminated string from Z3 memory, including its terminator.
  (func $copy_z3_string_to_argonc
    (param $source i32)
    (param $destination i32)
    (param $capacity i32)
    (result i32)
    (local $offset i32)
    (local $byte i32)
    (local.set $offset (i32.const 0))
    (block $done
      (loop $copy
        (br_if $done
          (i32.ge_u (local.get $offset) (local.get $capacity)))
        (local.set $byte
          (i32.load8_u $z3
            (i32.add (local.get $source) (local.get $offset))))
        (i32.store8 $argonc
          (i32.add (local.get $destination) (local.get $offset))
          (local.get $byte))
        (local.set $offset
          (i32.add (local.get $offset) (i32.const 1)))
        (br_if $done (i32.eqz (local.get $byte)))
        (br $copy)))
    (local.get $offset))

  (export "copy_argonc_to_z3" (func $copy_argonc_to_z3))
  (export "copy_z3_to_argonc" (func $copy_z3_to_argonc))
  (export "z3_string_length" (func $z3_string_length))
  (export "copy_z3_string_to_argonc" (func $copy_z3_string_to_argonc))
  (export "memory" (memory $argonc))
)

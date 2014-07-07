# glkit

This is a utility kit for functionality related to OpenGL.  Right now,
it provides the following:

* `kit.glm`: This re-exports [sb-cga](https://github.com/nikodemus/sb-cga)
  and [mathkit](https://github.com/lispgames/mathkit) for convenience.

* `kit.gl.shader`: This provides the shader dictionary and compilation
  functionality originally found in
  [sdl2kit](https://github.com/lispgames/sdl2kit).

## Shaders

Often you want to compile and check shaders, maintain the programs,
and set various uniforms as parameters.  `kit.gl.shader` provides a
way to do this simply:

```lisp
(defvar *my-programs*
  `((:program-name
     (:uniforms :v1 :v2 ...)
     (:shaders :vertex-shader ,shader-text
               :fragment-shader "..."
               ...))

    (:another-program ...)))
```

This is a simple structured list containing the specification for
creating a dictionary.  You may specify any legal combination of
shaders in the shaders section.  Reusing text between shaders is
relatively easy by using separate definitions and including them, as
per above.

To actually compile and use these, call the following; it will attempt
to compile and link all the specified programs, reporting any errors
along the way to `*error-output*`:

```lisp
(compile-shader-dictionary *my-programs*)
  ;; => DICTIONARY
```

This **requires** a valid GL context, will only work when it is
otherwise legal to make GL calls.  As well, the returned
`SHADER-DICTIONARY` object is only valid in the GL context in which it
was compiled.  It will **not** work in others.

Once you have this object, you may do interesting things with it:

```lisp
(kit.gl.shader:use-program DICTIONARY :name)
(kit.gl.shader:uniformi DICTIONARY :v1 0)
(kit.gl.shader:uniformf DICTIONARY :v2 x y)

;; etc
```

Note these are different functions than the `cl-opengl` variety; they
take the *dictionary* object, as well as symbolic names, rather than
IDs.

# glkit

This is a utility kit for functionality related to OpenGL.  Right now,
it provides the following:

* `kit.glm`: This re-exports [sb-cga](https://github.com/nikodemus/sb-cga)
  and [mathkit](https://github.com/lispgames/mathkit) for convenience.

* `kit.gl.shader`: This provides shader dictionary and compilation
  functionality similar to what was originally found in
  [sdl2kit](https://github.com/lispgames/sdl2kit).

## Shaders

`kit.gl.shader` provides a thin, extensible, program-oriented model
for GL shaders.  This closely mimics how GL works:

```lisp
(defdict (shaders-3.3 :shader-path #P"...")
   (shader basic-vertex :vertex-shader (:file "vertex.glsl"))
   (program :solid (:color)
     (:vertex-shader basic-vertex)
     (:fragment-shader "..."))
   (program :sprite ((:texture "texture_id"))
     (:vertex-shader basic-vertex)
     (:fragment-shader (:file "..."))))
```

This defines a series of shader programs grouped into a "dictionary".
Once in a GL context, one or more dictionaries may be compiled,
programs activated, and uniforms set, all symbolically.

You may specify any legal combination of shaders in the shaders
section.  Reusing text between shaders is easy by defining named
shaders with the `SHADER` directive.  Text may also be loaded from a
file using `(:file PATHNAME)` as above.  Additional handling may be
defined; see below.

To actually compile and use a dictionary, call the following; it will
attempt to compile and link all the specified programs, reporting any
errors along the way to `*error-output*`:

```lisp
(compile-shader-dictionary (dict shaders-3.3))
  ;; => DICTIONARY
```

This **requires** a valid GL context, will only work when it is
otherwise legal to make GL calls.  As well, the returned
`SHADER-DICTIONARY` object is only valid in the GL context in which it
was compiled.  It will **not** work in others.  If desired, more than
one dictionary may be compiled in a context; nothing besides
convenience groups programs in a dictionary.

Once you have this object, you may do interesting things with it:

```lisp
(kit.gl.shader:use-program DICTIONARY :name)

;; Note these apply only to the *current program*,
;; different programs have different sets of uniforms
(kit.gl.shader:uniformi DICTIONARY :v1 0)
(kit.gl.shader:uniformf DICTIONARY :v2 x y)

;; etc
```

Note these are different functions than the `cl-opengl` variety; they
take the *dictionary* object, as well as symbolic names, rather than
IDs.

### Customizing

It's also possible to define other ways to produce shader strings, by
specializing either of the following generic functions:

```lisp
(parse-shader-source SOURCE SHADER-TYPE SHADER-LIST)
(parse-shader-source-complex KEY PARAMS SHADER-TYPE SHADER-LIST)
```

The first specializes on a few `SOURCE` types by default; do not alter
these:

* `string`: A simple string which is used directly as source
* `list`: A list which is processed further by
  `PARSE-SHADER-SOURCE-COMPLEX`
* `symbol`: A symbol which is looked up in the current list of "named"
  shaders

The `SHADER-TYPE` parameter is any valid shader type,
e.g. `:vertex-shader`; `SHADER-LIST` is the current list of "named"
shaders, in the form `(NAME . (TYPE VALUE))`.  Notably, `VALUE` is not
processed, and should be passed recursively to `PARSE-SHADER-SOURCE`
if used.

To process forms like `(:file PATHNAME)`,
`PARSE-SHADER-SOURCE-COMPLEX` takes the CAR and CDR or that list, as
well other parameters similar to `PARSE-SHADER-SOURCE`.

These are mostly useful for projects which desire to add extended
shader capability, such as a shader DSL, or loading in some other
manner.

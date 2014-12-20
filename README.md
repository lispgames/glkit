# glkit

This is a utility kit for functionality related to OpenGL.  Right now,
it provides the following:

* `kit.glm`: This re-exports [sb-cga](https://github.com/nikodemus/sb-cga)
  and [mathkit](https://github.com/lispgames/mathkit) for convenience.

* `kit.gl.shader`: This provides shader dictionary and compilation
  functionality similar to what was originally found in
  [sdl2kit](https://github.com/lispgames/sdl2kit).

* `kit.gl.vao`: This provides an interface for Vertex Array Objects.

## Shaders

`kit.gl.shader` provides a thin, extensible, program-oriented model
for GL shaders.  This closely mimics how GL works:

```lisp
(defdict shaders-3.3 (:shader-path #P"...")
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

## VAOs

`kit.gl.vao` provides an easy way to define VAO layouts, as well as
instantiate, bind, and draw them.  It aims to provide complete VAO
functionality.

To use, first one defines a VAO:

```lisp
(defvao NAME ()
  (LAYOUT-TYPE (OPTS)
    (ATTR :type COUNT)
    ...)
  (LAYOUT-TYPE
    ...))
```

For example:

```lisp
(defvao vertex-color ()
  (:separate ()
    (vertex :float 3)
    (color :float 3)))
```

This defines a VAO with `VERTEX` and `COLOR` attributes, which are
each 3 `:float` values.  This uses a separate VBO for each.  (The
`LAYOUT-TYPE` will be covered below.)

Using a VAO is just as easy:

```lisp
(let ((vao (make-instance 'vao :type 'vertex-color)))
  (vao-buffer-data vao 0 (* 4 VERTEX-FLOAT-COUNT) POINTER-TO-VERTEX-FLOATS)
  (vao-buffer-data vao 1 (* 4 COLOR-FLOAT-COUNT) POINTER-TO-COLOR-FLOATS)

  ;; Establish more stuff here.. active shaders, uniforms, etc, then:
  (vao-draw vao :count VERTEX-COUNT))
```

**This requires a valid, active GL context,** just like other GL
functions.  `DEFVAO` does not, but everything else, *including the
`make-instance`*, does.

Alternatively, you can use `VAO-BUFFER-VECTOR` (and `VAO-BUFFER-SUB-VECTOR`), and supply a vector of `:element-type 'single-float` or `:element-type 'double-float` instead of a pointer.  This is only available if your implementation supports *static-vectors* (most do).  This is for convenience; managing the data yourself can reduce copying and consing considerably.

Note the numbers above require you fill in a few specific things:

* `vao-buffer-*` (and the `-sub` variants) take the *total byte count*.  So for a `:float` attribute with 3 members, that's `(* 4 3 COUNT)`.
* `vao-buffer-*` also takes the *VBO index* rather than an attribute name, because an attribute might not have a unique VBO.  See [Layouts](#Layouts) below.
* `vao-draw` takes the *vertex* count; e.g., triangles have 3 vertices, and if you have 10 triangles, that's 30 vertices.

The pointer data you must supply pre-formatted.  However, for separate
VBOs, this is reasonably easy to accomplish with something like
[static-vectors](https://github.com/sionescu/static-vectors), or you can use the less-efficient `-vector` variants which do this for you.

### Dictionary

* `defvao NAME OPTIONS &body GROUPS`<br> Define a VAO called `NAME`. Currently, there are no options.  See below for group definition.
* `vao-buffer-data VAO VBO BYTE-COUNT POINTER &optional (USAGE :dynamic-draw)`<br> Copy data to the VBO specified.  The VBO is specified as a number.  `BYTE-COUNT` is the total number of bytes to be copied.  `POINTER` is a (foreign) pointer to the data.  `USAGE` may be any valid usage constant for `glBufferData`.
* `vao-buffer-sub-data VAO VBO OFFSET BYTE-COUNT POINTER`<br> The `glBufferSubData` variant.
* `vao-buffer-vector VAO VBO BYTE-COUNT VECTOR &optional (USAGE :dynamic-draw)`<br> Copy data to the VBO specified.  The VBO is specified as a number.  `BYTE-COUNT` is the total number of bytes to be copied.  `POINTER` is a (foreign) pointer to the data.  `USAGE` may be any valid usage constant for `glBufferData`.  `VECTOR` must be a vector of specializable type for *static-vectors*.  Calling this will produce an error if *static-vectors* is not supported.
* `vao-buffer-sub-vector VAO VBO OFFSET BYTE-COUNT VECTOR`<br> The `glBufferSubData` variant.
* `vao-bind VAO`<br> Bind the VAO.  This is not necessary for calling the provided VAO functions, since the VAO is bound automatically.  However, it may be useful to ensure the VAO is bound if you wish to make GL calls manually.
* `vao-unbind`<br> Unbind the current-bound VAO.  Not done automatically.
* `vao-draw VAO &key primitive (first 0) count`<br> Bind the VAO and call `glDrawArrays`.  `count` is optional only if the vertex count has been supplied to the VAO, e.g. during `make-instance`.  `primitive` defaults to triangles, but may be specified explicitly here, overriding the VAO's configuration.

### Layouts

There are three layout types:

* `:separate`: This uses a separate VBO for each attribute supplied.
* `:interleave`:  This uses a single VBO for each attribute specified, and *interleaves* the attributes, e.g.: `Vert0 Color0 Vert1 Color1 ...`, where each attribute (such as "Vert0") has `:count` values.
* `:block`: This uses a single VBO for each attribute specified, arranged in *blocks*, e.g.: `Vert0 Vert1 Vert2 ... Color0 Color1 Color2 ...`.  This *requires* you specify `:vertex-count` up front to `make-instance`.  **This is currently not fully implemented.**

You may specify one or more groups to a VAO definition:

```lisp
(defvao NAME
  (:separate () ...)
  (:separate () ...)
  (:interleave () ...)
  ...)
```

You must be aware of the underlying VBO layout to the extent that you
must specify the correct index to `vao-buffer-data`.  In the future,
you will be able to specify a valid symbolic name, though this may not
be as efficient.

You may also specify a `:divisor` option to the *group*, which
corresponds to the DIVISOR parameter to `glVertexAttribDivisor`,
allowing one attribute for multiple vertices.  Note that because this
is *per-group*, if you wish to have separate divisors per attribute,
they must be in separate groups.

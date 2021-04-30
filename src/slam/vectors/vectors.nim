import std/[hashes, macros, strutils, strformat, algorithm]
import ../sse3, ../thosefloats, vectorbase

type
  Vec2* {.borrow: `.`.} = distinct VectorBase
  Vec3* {.borrow: `.`.} = distinct VectorBase
  Vec4* {.borrow: `.`.} = distinct VectorBase

  AnyVec = Vec2 or Vec3 or Vec4

  Real = float32 or int

# Creating Vectors with no value, sets to 0
template vec2*(): Vec2 = cast[Vec2](mm_setzero_ps()) ## Creates a `Vec2`
template vec3*(): Vec3 = cast[Vec3](mm_setzero_ps()) ## Creates a `Vec3`
template vec4*(): Vec4 = cast[Vec4](mm_setzero_ps()) ## Creates a `Vec4`

# Creating Vectors with a single value.
# Unsafe to use mm_set_ps1 as casting to another type of Vector will have unwanted values in the unused components
template vec2*(n: Real): Vec2 = cast[Vec2](mm_set_ps(0f, 0f, float32 n, float32 n)) ## Creates a `Vec2`
template vec3*(n: Real): Vec3 = cast[Vec3](mm_set_ps(0f, float32 n, float32 n, float32 n)) ## Creates a `Vec3`
template vec4*(n: Real): Vec4 = cast[Vec4](mm_set_ps(n, float32 n, float32 n, float32 n)) ## Creates a `Vec4`

# Creating a vec2
template vec2*(x: Real, y: Real): Vec2 = cast[Vec2](mm_set_ps(0f, 0f, y, float32 x)) ## Creates a `Vec2`

# Creating a vec3
template vec3*(x: Real, y: Real, z: Real): Vec3 = cast[Vec3](mm_set_ps(0f, float32 z, float32 y, float32 x)) ## Creates a `Vec3`
# Creating a vec3 with vec2 and number
template vec3*(v2: Vec2, z: Real): Vec3 = cast[Vec3](mm_set_ps(0f, float32 z, v2.g, v2.r)) ## Creates a `Vec3`
template vec3*(x: Real, v2: Vec2): Vec3 = cast[Vec3](mm_set_ps(0f, v2.g, v2.r, float32 x)) ## Creates a `Vec3`

template vec4*(x: Real, y: Real, z: Real, w: Real): Vec4 = cast[Vec4](mm_set_ps(float32 w, float32 z, float32 y, float32 x)) ## Creates a `Vec4`

# Vec4 with Vec2 and numbers
template vec4*(v2: Vec2, z: Real, w: Real): Vec4 = cast[Vec4](mm_set_ps(float32 w, float32 z, v2.g, v2.r)) ## Creates a `Vec4`
template vec4*(x: Real, v2: Vec2, w: Real): Vec4 = cast[Vec4](mm_set_ps(float32 w, v2.g, v2.r, float32 x)) ## Creates a `Vec4`
template vec4*(x: Real, y: Real, v2: Vec2): Vec4 = cast[Vec4](mm_set_ps(v2.g, v2.r, float32 y, float32 x)) ## Creates a `Vec4`
template vec4*(v2a, v2b: Vec2): Vec4 = cast[Vec4](mm_set_ps(v2b.g, v2b.r, v2a.g, v2a.r)) ## Creates a `Vec4`
# Vec4 with Vec3 and a number
template vec4*(v2: Vec3, w: Real): Vec4 = cast[Vec4](mm_set_ps(float32 w, v2.b, v2.g, v2.r)) ## Creates a `Vec4`
template vec4*(x: Real, v2: Vec3): Vec4 = cast[Vec4](mm_set_ps(v2.b, v2.g, v2.r, float32 x)) ## Creates a `Vec4`

# Simple method of interpreting an M128 as another vector
# Not used outside this module as the user should not worry about sse types or instructions
template vec2(v4: M128): Vec2 = cast[Vec2](v4)
template vec3(v4: M128): Vec3 = cast[Vec3](v4)
template vec4(v4: M128): Vec4 = cast[Vec4](v4)

# can't use const because ffi is unavailable at compile time for M128 types.
let
  XDir* = vec3(1.0f, 0.0f, 0.0f)
  YDir* = vec3(0.0f, 1.0f, 0.0f)
  ZDir* = vec3(0.0f, 0.0f, 1.0f)

{.experimental: "dotOperators".}

macro `.=`*(vb: AnyVec, fields: untyped, rv: AnyVec or Real): untyped =
  let swizzle = $fields
  assert swizzle.len in 1..4, "Incorrect swizzle length."
  assert swizzle.allCharsInSet({'x', 'y', 'z', 'w', 'r', 'g', 'b', 'a', 'u', 'v'}), "Incorrect swizzle characters"

  let t = $vb.getTypeInst

  let
    temp = genSym(ident = "shadowedVar")


  result = newStmtList()
  result.add newLetStmt(temp, rv)
  for i, c in $fields:
    case c:
    of 'x', 'r':
      result.add quote do:
        when `temp` is Real:
          `vb`.arr[0] = `temp`
        else:
          `vb`.arr[0] = `temp`.arr[`i`]
    of 'y', 'g':
        result.add quote do:
          when `temp` is Real:
            `vb`.arr[1] = `temp`
          else:
            `vb`.arr[1] = `temp`.arr[`i`]
    of 'z', 'b':
      assert not t.startsWith("Vec2"), fmt"Attempting to access `{c}` on a `Vec2`"
      result.add quote do:
        when `temp` is Real:
          `vb`.arr[2] = `temp`
        else:
          `vb`.arr[2] = `temp`.arr[`i`]
    of 'w', 'a':
      assert not t.startsWith("Vec2") and not t.startsWith("Vec3"), fmt"Attempting to access `{c}` on a `Vec2`"
      result.add quote do:
        when `temp` is Real:
          `vb`.arr[3] = `temp`
        else:
          `vb`.arr[3] = `temp`.arr[`i`]
    else: discard

# template wmask(): M128 = mm_set_ps(0, 1, 1, 1)
# template zwmask(): M128 = mm_set_ps(0, 0, 1, 1)

macro `.`*(vb: AnyVec, fields: untyped): untyped =
  ## Swizzle operator

  let swizzle = $fields

  assert swizzle.len in 1..4, "Incorrect swizzle length."
  assert swizzle.allCharsInSet({'x', 'y', 'z', 'w', 'r', 'g', 'b', 'a', 'u', 'v'}), "Incorrect swizzle characters"

  let t = $vb.getTypeInst

  var dotOps: seq[NimNode]
  var swizzleIndex: seq[int]
  for c in swizzle:
    case c:
    of 'x', 'r':
      dotOps.add quote do:
        `vb`.arr[0]
      swizzleIndex.add 0
    of 'y', 'g':
      dotOps.add quote do:
        `vb`.arr[1]
      swizzleIndex.add 1
    of 'z', 'b':
      assert not t.startsWith("Vec2"), fmt"Attempting to access `{c}` on a `Vec2`"
      dotOps.add quote do:
        `vb`.arr[2]
      swizzleIndex.add 2
    of 'w', 'a':
      assert not t.startsWith("Vec2") and not t.startsWith("Vec3"), fmt"Attempting to access `{c}` on a `Vec2` or `Vec3`"
      dotOps.add quote do:
        `vb`.arr[3]
      swizzleIndex.add 3
    else: discard
  case dotOps.len:
  of 1:
    result = dotOps[0]
  of 2:
    let
      x = dotOps[0]
      y = dotOps[1]
    result = quote do: vec2(`x`,`y`)
  of 3:
    let
      x = dotOps[0]
      y = dotOps[1]
      z = dotOps[2]
    result = quote do: vec3(`x`,`y`,`z`)
  of 4:
    let
      x = dotOps[0]
      y = dotOps[1]
      z = dotOps[2]
      w = dotOps[3]
    result = quote do: vec4(`x`,`y`,`z`, `w`)
  else: discard

template scale*(a, b: Vec2): Vec2 = vec2(a.v4 * b.v4) ## Multiply 2 Vectors component-wise
template scale*(a, b: Vec3): Vec3 = vec3(a.v4 * b.v4) ## Multiply 2 Vectors component-wise
template scale*(a, b: Vec4): Vec4 = vec4(a.v4 * b.v4) ## Multiply 2 Vectors component-wise

template clear*(a: var Vec3) = a.v4 = mm_setzero_ps() ## Set all components in a vector to 0 in place

template `*`*(a: Vec2, b: float32): Vec2 = vec2(a.v4 * mm_set1_ps(b))
template `*`*(a: Vec3, b: float32): Vec3 = vec3(a.v4 * mm_set1_ps(b))
template `*`*(a: Vec4, b: float32): Vec4 = vec4(a.v4 * mm_set1_ps(b))
template `*`*(a: float32, b: Vec2): Vec2 = vec2(b.v4 * mm_set1_ps(a))
template `*`*(a: float32, b: Vec3): Vec3 = vec3(b.v4 * mm_set1_ps(a))
template `*`*(a: float32, b: Vec4): Vec4 = vec4(b.v4 * mm_set1_ps(a))
template `*=`*(a: var Vec2, b: float32) = a.v4 *= mm_set_ps(1, 1, b, b)
template `*=`*(a: var Vec3, b: float32) = a.v4 *= mm_set_ps(1, b, b, b)
template `*=`*(a: var Vec4, b: float32) = a.v4 *= mm_set1_ps(b)
# template `*=`*(a: var AnyVec, b: float) = a.v4 = a.v4 * mm_set1_ps(b)

template zero128(): M128 = mm_setzero_ps()

# Some additional sse3/4.1 tests that aren't working very well
# proc hadd*(a: Vec3): float32 {.inline.} = mm_hadd_ps(a.v4, a.v4).mm_hadd_ps(zero128).mm_cvtss_f32
# proc dot*(a, b: Vec3): float32 {.inline.} = mm_dp_ps(a.v4, b.v4, 0xff).mm_cvtss_f32

template `+`*(a, b: Vec2): Vec2 = vec2(a.v4 + b.v4)
template `+`*(a, b: Vec3): Vec3 = vec3(a.v4 + b.v4)
template `+`*(a, b: Vec4): Vec4 = vec4(a.v4 + b.v4)
template `+=`*(a: var Vec2, b: float32) = a.v4 += mm_set_ps(0f, 0f, b, b)
template `+=`*(a: var Vec3, b: float32) = a.v4 += mm_set_ps(0f, b, b, b)
template `+=`*(a: var Vec4, b: float32) = a.v4 += mm_set1_ps(b)
template `+=`*(a: var AnyVec, b: AnyVec) = a.v4 += b.v4

template `/`*(a: Vec2, b: float32): Vec2 = vec2(a.v4 / mm_set1_ps(b))
template `/`*(a: Vec3, b: float32): Vec3 = vec3(a.v4 / mm_set1_ps(b))
template `/`*(a: Vec4, b: float32): Vec4 = vec4(a.v4 / mm_set1_ps(b))
template `/=`*(a: var AnyVec, b: float32) = a.v4 = a.v4 / mm_set1_ps(b)

template `-`*(a: Vec2): Vec2 = vec2(zero128() - a.v4)
template `-`*(a: Vec3): Vec3 = vec3(zero128() - a.v4)
template `-`*(a: Vec4): Vec4 = vec4(zero128() - a.v4)

template `-`*(a, b: Vec2): Vec2 = vec2(a.v4 - b.v4)
template `-`*(a, b: Vec3): Vec3 = vec3(a.v4 - b.v4)
template `-`*(a, b: Vec4): Vec4 = vec4(a.v4 - b.v4)
template `-=`*(a: var Vec2, b: float32) = a.v4 -= mm_set_ps(0f, 0f, b, b)
template `-=`*(a: var Vec3, b: float32) = a.v4 -= mm_set_ps(0f, b, b, b)
template `-=`*(a: var Vec4, b: float32) = a.v4 -= mm_set1_ps(b)
template `-=`*(a: var AnyVec, b: AnyVec) = a.v4 -= b.v4

template `!=`*[T: AnyVec](a, b: T): bool = (mm_movemask_ps(mm_cmpneq_ps(a.v4, b.v4)) and 0xffffffff'i32).bool
template `==`*[T: AnyVec](a, b: T): bool = not (a != b)

template `~=`*(a, b: Vec2): bool = ((a.x ~= b.x) and (a.y ~= b.y))
template `~=`*(a, b: Vec3): bool = ((a.x ~= b.x) and (a.y ~= b.y) and (a.z ~= b.z))
template `~=`*(a, b: Vec4): bool = ((a.x ~= b.x) and (a.y ~= b.y) and (a.z ~= b.z) and (a.w ~= b.w))

template `$`*(a: Vec2): string = $cast[(float32, float32)](a)
template `$`*(a: Vec3): string = $cast[(float32, float32, float32)](a)
template `$`*(a: Vec4): string = $cast[(float32, float32, float32, float32)](a)

template unpack*(a: Vec2): (float32, float32) = cast[(float32, float32)](a) ## Unpack a vector into a tuple
template unpack*(a: Vec3): (float32, float32, float32) = cast[(float32, float32, float32)](a) ## Unpack a vector into a tuple
template unpack*(a: Vec4): (float32, float32, float32, float32) = cast[(float32, float32, float32, float32)](a) ## Unpack a vector into a tuple

proc pretty*(a: Vec2): string {.inline.} =
  ## Pretty string representation of a Vec2
  ## Has limited precision and rounding to look nicer.
  let prettyobj = cast[(float32, float32)](a)
  let x = fmt"{prettyobj[0]:25}".strip
  let y = fmt"{prettyobj[1]:25}".strip
  (fmt"Vec2({x}, {y})")

proc pretty*(a: Vec3): string {.inline.} =
  ## Pretty string representation of a Vec3
  ## Has limited precision and rounding to look nicer.
  let prettyobj = cast[(float32, float32, float32)](a)
  let x = fmt"{prettyobj[0]:25}".strip
  let y = fmt"{prettyobj[1]:25}".strip
  let z = fmt"{prettyobj[2]:25}".strip
  (fmt"Vec3({x}, {y}, {z})")

proc pretty*(a: Vec4): string {.inline.} =
  ## Pretty string representation of a Vec4
  ## Has limited precision and rounding to look nicer.
  let prettyobj = cast[(float32, float32, float32, float32)](a)
  let x = fmt"{prettyobj[0]:25}".strip
  let y = fmt"{prettyobj[1]:25}".strip
  let z = fmt"{prettyobj[2]:25}".strip
  let w = fmt"{prettyobj[3]:25}".strip
  (fmt"Vec4({x}, {y}, {z}, {w})")

template hash*(a: Vec2): Hash =
  hash((a.x, a.y))

template hash*(a: Vec3): Hash =
  hash((a.x, a.y, a.z))

template hash*(a: Vec4): Hash =
  hash((a.x, a.y, a.z, a.w))

template size*(a: Vec2): int =
  ## Number of components this vector has\
  ## Because all Vectors in this library are aligned to 128 bits it is harder to reason about how many floats they are representing.
  2

template size*(a: Vec3): int =
  ## Number of components this vector has\
  ## Because all Vectors in this library are aligned to 128 bits it is harder to reason about how many floats they are representing.
  3

template size*(a: Vec4): int =
  ## Number of components this vector has\
  ## Because all Vectors in this library are aligned to 128 bits it is harder to reason about how many floats they are representing.
  4
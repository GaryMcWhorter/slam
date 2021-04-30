import std/[strformat, strutils]
import ../sse3, ../vectors/vectorbase, ../vectors/vectors

type
  Quaternion* {.borrow: `.`.} = distinct VectorBase
  Real = float32 or int

template quat*(x: Real, y: Real, z: Real, w: Real): Quaternion = cast[Quaternion](mm_set_ps(float32 w, float32 z, float32 y, float32 x)) ## Creates a Quaternion
template identity*():Quaternion = quat(0f, 0f, 0f, 1f) ## Returns a Quaternion that represents no rotation

template `*`*(rotation: Quaternion, point: Vec3): Vec3 =
  let first = cast[Vec3](rotation) * 2f
  let xyz = cast[Vec3](rotation).scale(first)
  let yzx = cast[Vec3](rotation).xxy.scale(first.yzz)
  let w = cast[Vec4](rotation).www.scale(first)

  vec3((1f - (xyz.y + xyz.z)) * point.x + (yzx.x - w.z) * point.y + (yzx.y + w.y) * point.z,
       (yzx.x + w.z) * point.x + (1f - (xyz.x + xyz.z)) * point.y + (yzx.z - w.x) * point.z,
       (yzx.y - w.y) * point.x + (yzx.z + w.x) * point.y + (1f - (xyz.x + xyz.y)) * point.z)

template `$`*(a: Quaternion): string = $cast[(float32, float32, float32, float32)](a)

proc pretty*(a: Quaternion): string {.inline.} =
  ## Pretty string representation of a Vec2
  ## Has limited precision and rounding to look nicer.
  let prettyobj = cast[(float32, float32, float32, float32)](a)
  let x = fmt"{prettyobj[0]:25}".strip
  let y = fmt"{prettyobj[1]:25}".strip
  let z = fmt"{prettyobj[2]:25}".strip
  let w = fmt"{prettyobj[3]:25}".strip
  (fmt"Quat({x}, {y}, {z}, {w})")
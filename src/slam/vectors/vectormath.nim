# import std/[math, macros]
import vectors, ../thosefloats

const kEpsilon* = 0.00001f
const kEpsilonNormalSqrt* = 1e-15f

type
  Vec2or3 = Vec2 or Vec3
  AnyVec = Vec2 or Vec3 or Vec4

template lerp*[T: AnyVec](a, b: T, t: float32): T =
  ## lerp between two vectors
  (a + (b - a) * fclamp01(t))

template lerpUnclamped*[T: AnyVec](a, b: T, t: float32): T =
  ## lerp between two vectors, `t` is not Clamped
  (a + (b - a) * t)

template perpendicular*(a: Vec2): Vec2 =
  vec2(-a.y, a.x)

template moveTowards*(current, target: AnyVec, maxStepDelta: float32): AnyVec =
  ## Move a Vec3 `current` to `target`
  block:
    let dist = target - current
    let scaled = dist.scale(dist)
    when target is Vec2:
      let sqrDist = scaled.x + scaled.y
    elif target is Vec3:
      let sqrDist = scaled.x + scaled.y + scaled.z
    elif target is Vec4:
      let sqrDist = scaled.x + scaled.y + scaled.z + scaled.w

    if sqrDist == 0 or (maxStepDelta >= 0 and sqrDist <= maxStepDelta * maxStepDelta):
      target
    else:
      let rtDist = sqrt(sqrDist)
      (current + dist / rtDist * maxStepDelta)

template cross*(lhs, rhs: Vec3): Vec3 =
  ## Cross product of 2 vectors
  (lhs.yzx.scale(rhs.zxy) - lhs.zxy.scale(rhs.yzx))

template dot*(lhs, rhs: AnyVec): float32 =
  ## Dot product of 2 vectors
  block:
    let scaled = lhs.scale(rhs)
    when lhs is Vec2:
      (scaled.x + scaled.y)
    elif lhs is Vec3:
      (scaled.x + scaled.y + scaled.z)
    else:
      (scaled.x + scaled.y + scaled.z + scaled.w)

template reflect*(direction, normal: Vec2): Vec2 =
  ## Reflects a vector from normal
  block:
    let factor = -2f * dot(normal, direction)
    vec2(factor * normal.x + direction.x, factor * normal.y + direction.y)

# Maybe slower. Need a real application to test
# template reflect2*(direction, normal: Vec3): Vec3 =
#   block:
#     let factor = -2f * dot(normal, direction)
#     (factor * normal + direction)

template reflect*(direction, normal: Vec3): Vec3 =
  ## Reflects a vector from normal
  block:
    let factor = -2f * dot(normal, direction)
    vec3(factor * normal.x + direction.x, factor * normal.y + direction.y, factor * normal.z + direction.z)

template len*(v: AnyVec): float32 =
  ## The length of a vector
  block:
    let first = v.scale(v)
    when v is Vec2:
      sqrt(first.x + first.y)
    elif v is Vec3:
      sqrt(first.x + first.y + first.z)
    else:
      sqrt(v.dot(v))

template sqrLen*(v: AnyVec): float32 =
  ## the squared length of a vector
  block:
    let first = v.scale(v)
    when v is Vec2:
      (first.x + first.y)
    elif v is Vec3:
      (first.x + first.y + first.z)
    else:
      v.dot(v)

template clampLen*(v: Vec2or3, maxLength: float32): Vec2or3 =
  ## Returns a clamped copy of a vector
  block:
    let sqrLen = v.sqrLen
    if sqrLen > maxLength * maxLength:
      let clampmag = sqrt(sqrLen)
      (v / clampmag * maxLength)
    else:
      v

template distance*(a, b: AnyVec): float32 =
  ## Returns the distance between `a` and `b`
  block:
    let diff = a - b
    # sqrt(diff.x * diff.x + diff.y * diff.y + diff.z * diff.z)
    diff.len

template normalize*(v: AnyVec): AnyVec =
  ## Returns this vector with a magnitude of 1
  block:
    let mag = len(v)
    if mag > kEpsilon:
      v / mag
    else:
      when v is Vec2: vec2()
      elif v is Vec3: vec3()
      else: vec4()

template project*(v, normal: Vec3 or Vec4): Vec3 or Vec4 =
  ## Projects a vector onto another vector
  block:
    when v is Vec3:
      let sqrMag = dot(normal, normal)
      if sqrMag < epsilon:
        vec3()
      else:
        let vDotN = v.dot(normal)
        (normal * vDotN / sqrMag)
    else:
      (normal * (v.dot(normal) / normal.dot(normal)))

template projectOnPlane*(v, planeNormal: Vec3): Vec3 =
  ## Projects a `Vec3` onto a plane
  block:
    let sqrMag = dot(planeNormal, planeNormal)
    if sqrMag < epsilon:
      vec3()
    else:
      let vDotN = v.dot(planeNormal)
      (v - planeNormal * vDotN / sqrMag)

template angle*(a, b: Vec2or3): float32 =
  ## Returns angle in degrees between 2 vectors
  block:
    let denominator = sqrt(a.sqrLen * b.sqrLen)
    if denominator < kEpsilonNormalSqrt:
      0f
    else:
      let aDotB = fclamp(dot(a,b) / denominator, -1f, 1f)
      (arccos(aDotB) * RadiansToDegrees)

template signedAngle*(a, b, axis: Vec3): float32 =
  ## Returns the signed angle between -180 and 180
  block:
    let unsignedAngled = angle(a, b)
    # let crossAxis = (a.yzx.scale(b.zxy)) - (a.zxy.scale(b.yzx))
    let crossAxis = axis.scale(a.cross(b))
    # echo axis.x * crossAxis.x +  axis.y * crossAxis.y + axis.z * crossAxis.z
    (unsignedAngled * sign(crossAxis.x + crossAxis.y + crossAxis.z))

template angleAroundAxis*(a, b, axis: Vec3, clockwise: bool = false): float32 =
  ## Returns a signed angle from two vectors projected on a plane
  when clockwise:
    let right = b.normalize().cross(axis)
    let forward = axis.cross(right)
  else:
    let right = axis.cross(b.normalize())
    let forward = right.cross(axis)
  (arctan2(a.dot(right), a.dot(forward)) * RadiansToDegrees)

template computeNormal*(a, b, c: Vec3): Vec3 =
  ## Get the normal of 3 Vec3's
  cross(c - b, b - a).normalize()

template dir*(at, to: Vec3): Vec3 =
  ## Direction between two vectors
  (at - to).normalize()

template sign*(v: Vec3): Vec3 =
  ## Returns a `Vec3` with the `sign` of each component\
  ## sign(x), sign(y), sign(z)
  vec3(sign(v.x), sign(v.y), sign(v.z))

template floor*(v: Vec3): Vec3 =
  ## Returns a `Vec3` with the `floor` of each component\
  ## floor(x), floor(y), floor(z)
  vec3(floor(v.x), floor(v.y), floor(v.z))

template abs*(v: Vec3): Vec3 =
  ## Returns a `Vec3` with the `abs` of each component\
  ## floor(x), floor(y), floor(z)
  vec3(abs(v.x), abs(v.y), abs(v.z))

template quantize*(v: AnyVec, n: float32): AnyVec =
  ## Quantizes a Vector
  (sign(v) * floor(abs(v) / n) * n)
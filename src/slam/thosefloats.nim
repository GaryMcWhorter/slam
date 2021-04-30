import std/math

export math

const DegreesToRadians* = PI * 2f / 360f ## approx 0.0174532925
const RadiansToDegrees* = 1f / DegreesToRadians ## approx 57.2957795
const epsilon* = 1.192092896e-07f # Same as FLT_EPSILON

template `fabs`(x: float32): float32 = 
  if x < 0.0: -x else: x

# https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
template `~=`*(a, b: float32): bool =
  ## `a` and `b` almost equal?

  # if diff <= largest * epsilon
  # diff = fabs(a-b) 
  # largest = (if fabs(b) > fabs(a): fabs(b) else: fabs(a))
  (if fabs(a - b) <= (if fabs(b) > fabs(a): fabs(b) else: fabs(a)) * epsilon: true else: false)

template radians*(x: float32): float32 =
  ## degrees to radians
  ## `x`: Angle in degrees
  (x * 0.0174532925f)

template degrees*(x: float32): float32 =
  ## radians to degrees
  ## `x`: Angle in radians
  (x * 57.295779513f)

template isBetween*(a, min, max: float32): bool =
  ## `a` is between `min` and `max`?\
  ## `min` <= `a` <= `max` 
  (a >= min) and (a <= max)

template sign*(x: float32): float32 =
  ## Returns 1 when `x` is positive or zero, -1 when `x` is negative
  (if x >= 0: 1.0f else: -1.0f)

template quantize*(x, n: float32): float32 =
  ## Quantize `x` to a multiple of `n` as an integer.
  (sign(x) * floor(fabs(x) / n) * n)

template repeat*(v, length: float32): float32 =
  # Loops `v`, clamped between 0 and `length`
  (clamp(v - floor(v / length) * length, 0.0f, length))

template fclamp*(x, a, b: float32): float32 =
  ## Limits the float ``x`` within the interval [a, b].
  (if x < a: a elif x > b: b else: x)

template fclamp01*(v: float32): float32 =
  fclamp(v, 0f, 1f)

template lerp*(a, b, v: float32): float32 =
  ## The interpolated float `v` between `a` and `b`\
  ## `v` is clamped 0 to 1
  (a + (b - a) * fclamp01(v))

template lerpUnclamped*(a, b, v: float32): float32 =
  ## The interpolated float `v` between `a` and `b`\
  ## `v` is not clamped
  (a + (b - a) * v)

proc lerpAngleP*(a, b, v: float32): float32 {.inline.} =
  ## Lerps between angles `a`, `b` with value `v`\
  ## Angle wraps at 360; 360 == 0 and 720 == 0 etc
  let delta = repeat((b-a), 360f)
  result = a + (if delta < 180: delta else: delta - 360) * fclamp01(v)

template lerpAngle*(a, b, v: float32): float32 =
  ## Lerps between angles `a`, `b` with value `v`\
  ## Angle wraps at 360; 360 == 0 and 720 == 0 etc
  let delta = repeat((b-a), 360f)
  (a + (if delta < 180: delta else: delta - 360) * fclamp01(v))

template deltaAngle*(current, target: float32): float32 =
  let delta = repeat((target - current), 360f)
  (if delta < 180: delta else: delta - 360f)

template moveTowards*(current, target, maxDelta: float32): float32 =
  ## Moves a `current` towards `target` by `maxDelta` step
  if fabs(target - current) <= maxDelta:
    target
  else:
    (current + sign(target - current) * maxDelta)

template moveTowardsAngle*(current, target, maxDelta: float32): float32 =
  ## Moves a `current` towards `target` by `maxDelta` step with 360 degree wrapping
  let dAngle = deltaAngle(current, target)
  if -maxDelta < dAngle and dAngle < maxDelta:
    target
  else:
    moveTowards(current, target, maxDelta)


template smoothStep*(start, finish, t: float32): float32 =
  ## Interploate with smoothing at the ends
  let tt = fclamp01(t)
  let finalt = ((3 - (tt + tt)) * (tt * tt))
  start + (finish - start) * finalt

proc smoothDamp*(current, target: float32, currentVelocity: var float32, smoothTime, maxSpeed, deltaTime: float32): float32 =
  ## Smoothly eases value over time
  ## Game Programming Gems 4 Chapter 1.10
  let smoothTime = max(0.0001f, smoothTime)
  let omega = 2f / smoothTime
  let x = omega * deltaTime
  let exp = 1f / (1f + x + 0.48f * x * x + 0.235f * x * x * x)
  let change = current - target
  let originalTo = target

  let temp = (currentVelocity + omega * change) * deltaTime
  currentVelocity = (currentVelocity - omega * temp) * exp
  result = target + (change + temp) * exp

  if (originalTo - current > 0.0f) == (result > originalTo):
    result = originalTo
    currentVelocity = (result - originalTo) / deltaTime

template smoothDampAngle*(current, target: float32, currentVelocity: var float32, smoothTime, maxSpeed, deltaTime: float32): float32 =
  ## Smoothly eases angle in degrees over time
  let target = current + deltaAngle(current, target)
  return SmoothDamp(current, target, currentVelocity, smoothTime, maxSpeed, deltaTime)

template pingPong*(length, t: float32): float32 =
  fclamp(t - floor(t/length) * length, 0.0f, length)

template inverseLerp*(a, b, t: float32): float32 =
  ## Gets the lerp parameter of the values
  (if a != b: fclamp01((t - a) / (b - a)) else: 0.0f)
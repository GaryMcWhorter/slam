import ../vectors/vectors

type
  Real = float32 or int

# float "swizzling"
template xx*(n: Real): Vec2 = vec2(n)
template xxx*(n: Real): Vec3 = vec3(n)
template xxxx*(n: Real): Vec4 = vec4(n)

# vector shorthand
{.push comment:"## vector shorthand"}
template v2*(n: Real): Vec2 = vec2(n)
template v3*(n: Real): Vec3 = vec3(n)
template v4*(n: Real): Vec4 = vec4(n)
{.pop.}
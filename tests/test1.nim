# To run these tests, simply execute `nimble test`.

import unittest

import slam
test "can create Vec2, Vec3, Vec4":
  check vec2(0) == vec2()
  check vec3(0) == vec3()
  check vec4(0) == vec4()

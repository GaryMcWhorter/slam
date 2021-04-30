import ../sse3

type
  VectorBase* {.union.} = object
    v4*: M128
    arr*: array[4, float32]
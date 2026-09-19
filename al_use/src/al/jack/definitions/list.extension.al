Extension {
  #name : :list
}

:list >> :max, [xs, head, body, val] [
  min(xs, [x, k], [{:call, head, body, [x, v]}, {:compare, :eq, k, 0 - v}], val)
]

:list >> :min, [[], _, _, :none] [

]

:list >> :min, [[h | t], head, body, val] [
  implies do
    [call(head, body, [h, k])] -> min_from(t, head, body, h, k, val)
    :else -> min(t, head, body, val)
  end
]
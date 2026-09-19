Extension {
  #name : :list
}

:list >> :find_min, [[], _, :none] [

]

:list >> :find_min, [[h | t], pred, val] [
  not [send(h, pred, [vh])]
  find_min(t, pred, val)
]

:list >> :find_min, [[h | t], pred, h] [
  find_min(t, pred, :none)
  send(h, pred, [_])
]

:list >> :find_min, [[h | t], pred, val] [
  find_min(t, pred, rest_val)
  dif(rest_val, :none)
  send(h, pred, [pred_head])
  send(rest_val, pred, [pred_rest])

  implies do
    [pred_rest > pred_head] -> unify(val, h)
    :else -> unify(val, rest_val)
  end
]

:list >> :find_min, [[], _, :none] [

]

:list >> :find_min, [[h | t], pred, val] [
  not [send(h, pred, [vh])]
  find_min(t, pred, val)
]

:list >> :find_min, [[h | t], pred, h] [
  find_min(t, pred, :none)
  send(h, pred, [_])
]

:list >> :find_min, [[h | t], pred, val] [
  find_min(t, pred, rest_val)
  dif(rest_val, :none)
  send(h, pred, [pred_head])
  send(rest_val, pred, [pred_rest])

  implies do
    [pred_rest > pred_head] -> unify(val, h)
    :else -> unify(val, rest_val)
  end
]
Extension {
  #name : :list
}

:list >> :remove, [[x | t], x, t] [

]

:list >> :remove, [[h | t], x, [h | r]] [
  dif(h, x)
  remove(t, x, r)
]

:list >> :duplicates, [xs, dups] [
  findall(c, dups) do
    member(xs, c)
    value(c, v)
    remove(xs, c, rest)
    member(rest, d)
    value(d, v)
  end
]

:list >> :duplicates, [xs, dups] [
  group_by_value(xs, %{}, groups)

  findall(c, dups) do
    get(groups, _v, [a, b | rest])
    member([a, b | rest], c)
  end
]

:list >> :group_by_value, [[], groups, groups] [

]

:list >> :group_by_value, [[c | t], acc, groups] [
  value(c, v)
  get(acc, v, [], cs)
  put(acc, v, [c | cs], next)
  group_by_value(t, next, groups)
]
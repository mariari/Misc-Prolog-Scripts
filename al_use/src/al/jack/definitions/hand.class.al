Class {
  #name : :hand,
  #superclass : [:object],
  #metaclass : :class,
  #ivars : [cards: [default: []]]
}

:hand >> :tier, [self, val] [
  fail()
]

:hand >> :value, [self, val] [
  fail()
]

:hand >> :determine_hand, [self, hand] [
  findall(
    hand,
    [
      super(class, :hand),
      get(self, :cards, cards),
      new(class, %{cards: cards}, hand),
      valid(hand)
    ],
    hands
  )

  find_min(hands, :tier, hand)
]

:hand >> :valid, [self] [
  fail()
]
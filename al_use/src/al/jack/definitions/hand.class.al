Class {
  #name : :hand,
  #superclass : [:object],
  #metaclass : :class,
  #ivars : [%{default: [], name: :cards}]
}

:hand >> :determine_hand, [self, hand] [
  findall(hand, hands) do
    super(class, :hand)
    get(self, :cards, cards)
    new(class, %{cards: cards}, hand)
  end

  min_by(hands, :tier, hand)
]

:hand >> :value, [self, val] [
  fail()
]

:hand >> :tier, [self, val] [
  fail()
]
Class {
  #name : :high_card,
  #superclass : [:hand],
  #metaclass : :class,
  #ivars : [:high, :high2]
}

:high_card >> :tier, [self, 10] [

]

:high_card >> :init, [self, args, self] [
  call_next_method(self, args, self)
  get(self, :cards, cards)
  find_high(self, cards, high)
  remove(cards, high, cards_removed)
  find_high(self, cards_removed, high2)
  set_slot(self, :high, high)
  set_slot(self, :high2, high2)
]

:high_card >> :value, [self, value] [
  get(self, :high, card)
  value(card, value)
]

:high_card >> :find_high, [_, cards, pair] [
  max_by(cards, :value, pair)
]
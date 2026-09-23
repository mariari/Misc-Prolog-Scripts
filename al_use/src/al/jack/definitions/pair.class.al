Class {
  #name : :pair,
  #superclass : [:hand],
  #metaclass : :class,
  #ivars : [:pair]
}

:pair >> :tier, [self, 9] [

]

:pair >> :init, [self, args, self] [
  call_next_method(self, args, self)
  get(self, :cards, cards)
  duplicates(cards, duped_cards)
  find_pair(self, duped_cards, pair)
  set_slot(self, :pair, pair)
]

:pair >> :value, [self, value] [
  get(self, :pair, card)
  value(card, value)
]

:pair >> :find_pair, [_, cards, pair] [
  max_by(cards, :value, pair)
]
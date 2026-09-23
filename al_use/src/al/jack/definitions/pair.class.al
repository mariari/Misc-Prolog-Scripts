Class {
  #name : :pair,
  #superclass : [:hand],
  #metaclass : :class,
  #ivars : [:pair]
}

:pair >> :tier, [self, 9] [

]

:pair >> :init, [self, args, self] [
  get(self, :cards, cards)
  duplicates(cards, duped_cardrs)
  find_pair(self, duped_cardrs, pair)
  set_slot(self, :pair, pair)
]

:pair >> :valid, [self] [

]

:pair >> :value, [self, value] [
  get(self, :pair, card)
  value(card, value)
]

:pair >> :find_pair, [_, cards, pair] [
  max_by(cards, :value, pair)
]
Class {
  #name : :card,
  #superclass : [:value],
  #metaclass : :class,
  #ivars : [value: [], suit: []]
}

:card >> :suit, [self, suit] [
  get(self, :suit, suit)
]

:card >> :value, [self, value] [
  get(self, :value, value)
]
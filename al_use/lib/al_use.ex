defmodule AlUse do
  @moduledoc """
  Documentation for `AlUse`.
  """
  use AL

  @doc "beginning state, will reset everything"
  def beginning do
    run do
      new(:package, %{name: :jack, version: 1, deps: [], redef: true}, :jack)

      active_build(:jack, build)

      defclass :card, super: :value, redef: true, ivars: [:value, :suit] do
        defmethod :value, [self, value] do
          get(self, :value, value)
        end

        defmethod :suit, [self, suit] do
          get(self, :suit, suit)
        end
      end

      # We are the super class of all hands
      defclass :hand, super: :object, redef: true, ivars: [%{name: :cards, default: []}] do
        # ideally on the class side
        defmethod :determine_hand, [self, hand] do
          findall(hand, hands) do
            super(class, :hand)
            get(self, :cards, cards)
            new(class, %{cards: cards}, hand)
            # consider removing for new enforcing validity
            valid(hand)
          end

          min_by(hands, :tier, hand)
        end

        # subclass responsibility
        defmethod :valid, [self] do
          fail
        end

        # Value within a tier, subclass responsibility
        defmethod :value, [self, val] do
          fail
        end

        # Tier of the, subclass responsibility
        defmethod :tier, [self, val] do
          fail
        end
      end

      defclass :pair, super: :hand, redef: true, ivars: [:pair] do
        defmethod(:tier, [self, 9])

        defmethod :init, [self, args, self] do
          get(self, :cards, cards)
          duplicates(cards, duped_cardrs)
          find_pair(self, duped_cardrs, pair)
          set_slot(self, :pair, pair)
        end

        defmethod(:valid, [self])

        defmethod(:value, [self, value]) do
          get(self, :pair, card)
          value(card, value)
        end

        defmethod :find_pair, [_, cards, pair] do
          max_by(cards, :value, pair)
        end
      end

      defclass :high_card, super: :hand, redef: true, ivars: [:high, :high2] do
        defmethod(:tier, [self, 10])

        defmethod :init, [self, args, self] do
          get(self, :cards, cards)
          find_high(self, cards, high)
          find_high(self, cards_removed, high2)
          remove(cards, high, cards_removed)
          set_slot(self, :high, high)
          set_slot(self, :high2, high2)
        end

        defmethod(:valid, [self])

        defmethod :value, [self, value] do
          get(self, :high, card)
          value(card, value)
        end

        defmethod :find_high, [_, cards, pair] do
          max_by(cards, :value, pair)
        end
      end

      include_class(build, :card)
      include_class(build, :hand)
      include_class(build, :high_card)
      include_class(build, :pair)
    end
  end

  @doc "Export image changes to disk for persistence"
  def export do
    AL.Package.export(:jack, to: "src/al/jack")
  end

  @doc "import image changes to disk for persistence"
  def import_this do
    AL.Package.import("src/al/jack", [])
  end

  def hello do
    :world
  end
end

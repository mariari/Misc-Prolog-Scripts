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

      defclass :card, super: :value, redef: true, ivars: [value: [], suit: []] do
        defmethod :value, [self, value] do
          get(self, :value, value)
        end

        defmethod :suit, [self, suit] do
          get(self, :suit, suit)
        end
      end

      defmethod :list, :max, [xs, head, body, val] do
        min(xs, [x, k], [call(head, body, [x, v]), eq(k, 0 - v)], val)
      end

      defmethod(:list, :min, [[], _, _, :none])

      defmethod :list, :min, [[h | t], head, body, val] do
        implies do
          [call(head, body, [h, k])] -> min_from(t, head, body, h, k, val)
          :else -> min(t, head, body, val)
        end
      end

      defmethod(:list, :min_from, [[], _, _, best, _, best])

      defmethod :list, :min_from, [[h | t], head, body, best, bk, val] do
        implies do
          [call(head, body, [h, k]), k < bk] -> min_from(t, head, body, h, k, val)
          :else -> min_from(t, head, body, best, bk, val)
        end
      end

      # We are the super class of all hands
      defclass :hand, super: :object, redef: true, ivars: [cards: [default: []]] do
        # ideally on the class side
        defmethod :determine_hand, [self, hand] do
          findall(
            hand,
            [
              super(class, :hand),
              get(self, :cards, cards),
              new(class, %{cards: cards}, hand),
              # consider removing for new enforcing validity
              valid(hand)
            ],
            hands
          )

          find_min(hands, :tier, hand)
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

      defclass :pair, super: :hand, redef: true, ivars: [pair: []] do
        defmethod(:tier, [self, 9])

        defmethod(:valid, [self]) do
        end

        defmethod(:value, [self, value]) do
          get(self, pair, card)
          value(card, value)
        end
      end

      defclass :high_card, super: :hand, redef: true, ivars: [high: []] do
        defmethod(:tier, [self, 10])
        defmethod(:valid, [self])

        defmethod :value, [self, value] do
          get(self, high, card)
          value(card, value)
        end
      end

      include_method(build, :list, :find_min)
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

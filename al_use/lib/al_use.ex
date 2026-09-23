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

      defmethod(:list, :remove, [[x | t], x, t])

      defmethod :list, :remove, [[h | t], x, [h | r]] do
        dif(h, x)
        remove(t, x, r)
      end

      # value => cards with that value, one value send per card
      defmethod(:list, :group_by_value, [[], groups, groups])

      defmethod :list, :group_by_value, [[c | t], acc, groups] do
        value(c, v)
        get(acc, v, [], cs)
        put(acc, v, [c | cs], next)
        group_by_value(t, next, groups)
      end

      # cards whose value is shared with another card in the list
      defmethod :list, :duplicates, [xs, dups] do
        group_by_value(xs, %{}, groups)

        findall(c, dups) do
          get(groups, _v, [a, b | rest])
          member([a, b | rest], c)
        end
      end

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
            # new fails if criteria isn't met
            new(class, %{cards: cards}, hand)
          end

          min_by(hands, :tier, hand)
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
          call_next_method(self, args, self)
          get(self, :cards, cards)
          duplicates(cards, duped_cards)
          find_pair(self, duped_cards, pair)
          set_slot(self, :pair, pair)
        end

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
          call_next_method(self, args, self)
          get(self, :cards, cards)
          find_high(self, cards, high)
          remove(cards, high, cards_removed)
          find_high(self, cards_removed, high2)
          set_slot(self, :high, high)
          set_slot(self, :high2, high2)
        end

        defmethod :value, [self, value] do
          get(self, :high, card)
          value(card, value)
        end

        defmethod :find_high, [_, cards, pair] do
          max_by(cards, :value, pair)
        end
      end

      include_method(build, :list, :remove)
      include_method(build, :list, :group_by_value)
      include_method(build, :list, :duplicates)
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

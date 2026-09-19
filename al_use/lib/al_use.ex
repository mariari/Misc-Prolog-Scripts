defmodule AlUse do
  @moduledoc """
  Documentation for `AlUse`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> AlUse.hello()
      :world

  """
  use AL

  @doc "beginning state, will reset everything"
  def beginning do
    run do
      new(:package, %{name: :jack, version: 1, deps: [], redef: true}, :jack)

      active_build(:jack, build)

      defclass :card, super: :value, redef: true, ivars: [value: [], suit: []] do
      end

      defclass :hand, super: :object, redef: true, ivars: [cards: []] do
      end

      include_class(build, :card)
      include_class(build, :hand)
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

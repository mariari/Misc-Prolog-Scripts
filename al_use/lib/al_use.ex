defmodule AlUse do
  @moduledoc """
  Documentation for `AlUse`.
  """
  use AL

  @doc "bootstrap the system"
  def bootstrap do
    run do
      new(:package, %{name: :jack, version: 1, deps: [], redef: false}, :jack)
    end
  end

  def include do
    run do
      active_build(:jack, build)

      include_method(build, :object, :yourself)
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

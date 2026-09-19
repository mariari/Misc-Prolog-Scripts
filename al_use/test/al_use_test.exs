defmodule AlUseTest do
  use ExUnit.Case
  doctest AlUse

  test "greets the world" do
    assert AlUse.hello() == :world
  end
end

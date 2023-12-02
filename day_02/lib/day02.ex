defmodule Day02 do
  def part1 do
    case File.read("input/input.txt") do
      {:ok, body} ->
        body
        |> String.split("\n", trim: true)
        |> Enum.map(fn x ->
          [id, content] =
            x
            |> String.replace("Game ", "")
            |> String.split(": ", trim: true)

          data =
            content
            |> String.split("; ", trim: true)
            |> Enum.map(fn x ->
              x
              |> String.split(", ", trim: true)
              |> Enum.map(fn x ->
                case Integer.parse(x) do
                  {x, " red"} when x <= 12 ->
                    true

                  {x, " green"} when x <= 13 ->
                    true

                  {x, " blue"} when x <= 14 ->
                    true

                  _ ->
                    false
                end
              end)
            end)
            |> List.flatten()

          {id, data}
        end)
        |> Enum.filter(fn x ->
          x |> elem(1) |> Enum.all?()
        end)
        |> Enum.map(fn x ->
          x |> elem(0) |> Integer.parse() |> elem(0)
        end)
        |> Enum.sum()
        |> IO.inspect()
    end
  end

  def part2 do
    case File.read("input/input.txt") do
      {:ok, body} ->
        body
        |> String.split("\n", trim: true)
        |> Enum.map(fn x ->
          [_id, content] =
            x
            |> String.replace("Game ", "")
            |> String.split(": ", trim: true)

          content
          |> String.split("; ", trim: true)
          |> Enum.map(fn x ->
            x
            |> String.split(", ", trim: true)
            |> Enum.map(&Integer.parse/1)
          end)
          |> List.flatten()
          |> Enum.sort(fn x, y -> elem(x, 1) > elem(y, 1) end)
          |> Enum.chunk_by(&elem(&1, 1))
          |> Enum.map(fn x ->
            x
            |> Enum.max(&(elem(&1, 0) > elem(&2, 0)))
            |> elem(0)
          end)
          |> List.foldl(1, &(&1 * &2))
        end)
        |> Enum.sum()
        |> IO.inspect()
    end
  end
end

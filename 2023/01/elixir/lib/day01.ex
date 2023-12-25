defmodule Day01 do
  def part1 do
    case(File.read("input/day01.txt")) do
      {:ok, body} ->
        body
        |> String.split("\n", trim: true)
        |> Enum.map(fn e ->
          first =
            e |> String.graphemes() |> Enum.find(fn e -> Integer.parse(e) != :error end)

          second =
            e
            |> String.graphemes()
            |> Enum.reverse()
            |> Enum.find(fn e -> Integer.parse(e) != :error end)

          (first <> second) |> Integer.parse() |> elem(0)
        end)
        |> Enum.sum()
        |> IO.inspect()
    end
  end

  def part2 do
    case(File.read("input/day01.txt")) do
      {:ok, body} ->
        body
        |> String.split("\n", trim: true)
        |> Enum.map(fn e ->
          nums =
            Regex.scan(~r/(?=(\d|one|two|three|four|five|six|seven|eight|nine))/, e)
            |> List.flatten()
            |> Enum.filter(&(&1 != ""))

          [List.first(nums), List.last(nums)]
          |> Enum.map(fn e ->
            case e do
              "one" ->
                "1"

              "two" ->
                "2"

              "three" ->
                "3"

              "four" ->
                "4"

              "five" ->
                "5"

              "six" ->
                "6"

              "seven" ->
                "7"

              "eight" ->
                "8"

              "nine" ->
                "9"

              _ ->
                e
            end
          end)
          |> Enum.join("")
          |> Integer.parse()
          |> elem(0)
        end)
        |> Enum.sum()
        |> IO.inspect()
    end
  end
end

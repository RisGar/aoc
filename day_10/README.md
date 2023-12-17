# Day 10 in Rust

## Part 1

- Uses complex numbers to save position $(x, y)$ as $z = x + yi$.

## Part 2

- [Pick's theorem](https://www.wikiwand.com/en/Pick%27s_theorem) says that a polygon's area $A$ is $i + \frac b 2 - 1$ where $i$ is the number of integer points in the polygon (our solution) and $\frac b 2$ half of the number of integer points on its edge (part 1 solution), which means we can get $i$ through $A - \frac b 2 + 1$
- We can calculate the area $A$ through the [Shoelace formula](https://www.wikiwand.com/en/Shoelace_formula) for polygons $\frac 1 2 \sum*{i=1}^{n} (\mathbf{Im}(z_i) + \mathbf{Im}(z*{i+1})) * (\mathbf{Re}(z_i) - \mathbf{Re}(z*{i+1}))$ where each position $z$ is a corner of the polygon

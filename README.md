
# Qu Script
![alt text](https://github.com/GsLogiMaker/qu-script/blob/e4c8fddc010feb76daadbda53f2bdfd27078d6ee/Logo.png)

![example workflow](https://github.com/GsLogiMaker/qu-script/actions/workflows/rust.yml/badge.svg)
```
fn main():
  print("Hello world!")
```

> __Warning__ 'Qu' related puns lie ahead!

## What is Qu?
Good **qu**estion, glad you asked! Qu is both a dynamicly and staticly typed interpreted language that utilizes traits rather than object inheritance. Qu is geared toward being lightweight and fast for the purpose of embedding into games and game engines as a main programming language and/or as modding scripts. Qu was inspired by GDScript, Rust, Python, Nim, Wren, Lua, and C# in that order.

## What Does it Look Like?
``` GDScript
# A single line comment

#/ A multi
	line comment /#

import math.Add
import math.length
import math.sqrt

fn main():
	# Variables can be static or dynamic. Still working
	# on auto typing.
	var point1 = Point2D(100, 100)
	var point2 Point2D = Point2D()

	assert(point1.x == point2.x)
	assert(point1.y == point2.y)

	# Uniform function call syntax.
	assert(point1.length() == length(point1))

	# Access the functions and variables of implemented traits
	# and classes (or mixins if you are so inclined) like
	# they were native to the parent class.
	assert(point1.data == point1.debug_info.data)


class Point2D:
	# Implement traits.
	impl Sync
	impl Add as add:
		fn add(other Point2D) Point2D:
			return Point2D(x+other.x, y+other.y)
	
	# Implement other classes as variables.
	impl DebugInfo as debug_info
	
	var x int = 0
	var y int = 0

	fn init(x int, y int):
		# The 'self' keyword can be used, but is not required.
		self.x = x
		self.y = y
	

	fn init():
		# Implement multiple versions of the same
		# function for different contexts
		x = 100
		y = 100


	fn length():
		return sqrt(x*x + y*y)


class DebugInfo:
	var data
```

## Want to Contribute?
You are more than welcome to contribute whatever you have, wheather that be an idea or code!
### Contributing Ideas
If you want to contribute your thoughts and ideas to the project navigate over to `issues` and create an new issue with your thoughts, even criticism is welcome!
### Contributing Code
Qu is written in Rust and I'll assume you are already familiar with it and Git. The entry point of Qu is `qube/src/main.rs` and the api to the language is `qu/src/lib.rs`. These are the most important files to understand first. If you have any **qu**estions contact me, even through the `issues` page, and I'll do my best to help!

## Future Goals
Easy and intuitive Rust API and maybe the same for C. A cargo-like project manager. Maybe a linter called **Qu**cumber and documentation tutorials called the **Qu**rriculum.

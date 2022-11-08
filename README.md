
# Qu Script
![alt text](https://github.com/GsLogiMaker/qu-script/blob/e4c8fddc010feb76daadbda53f2bdfd27078d6ee/Logo.png)

![example workflow](https://github.com/GsLogiMaker/qu-script/actions/workflows/rust.yml/badge.svg)
```
fn main():
  print("Hello world!")
```

> __Warning__ 'Qu' related puns lie ahead!

## What is Qu?
Good **qu**estion, glad you asked! Qu is both a dynamicly and staticly typed interpreted language that utilizes traits rather than object inheritance. Qu is geared toward being lightweight and fast for the purpose of embedding into games and game engines as a main programming language and/or as modding scripts. Qu was inspired by GDScript, Rust, Python, Wren, Lua, and C# in that order.

> __Note__: The following are part of the design goals; they are not implemented yet. The following text is also a work in progress.

Easy to interop with C, maybe, WHOO! You can remove the default Qu standard library and replace it with your own standard library if you need to ensure scripts can't be used for anything malicous (Pariculary useful in modding!) Qu comes with a Rust-like package manager called **Qu**be for creating projects, running projects, and installing dependencies. Also may add a linter called **Qu**cumber and documentation called **Qu**rriculum.

## What Does it Look Like?
``` GDScript
# A single line comment

#/ A multi
	line comment /#

const JUMP_VELOCITY = 100.0

trait Life:
	# For an object to implement this trait it
	# must declare the following variables and functions.
	var health = 100.0
	var attack = 20.0
	
	fn die()
	fn eat()
	fn revive()

class Player:
	impl Life
	
	var health = 10.0
	var attack = 2.0
	
	# Optional static typing
	var velocity_x float = 0.0
	var velocity_y float = 0.0
	
	fn jump():
		# 'self' is implicite; it is not required by default, but
		# accessing global variables is explicite; they require
		# the 'glob' keyword to be accessed.
		velocity_y -= glob.JUMP_VELOCITY
		self.velocity_x = 0.0


	# A static function, the 'self' keyword does not
	# work here.
	static fn player(name String, health float):
		return name + health.str()


	# Implement the 'Life' trait's functions
	fn Life.die():
		print("died")


	fn Life.eat(food String):
		print("That", food, "was delicous!")


	# The preceding 'Life.' is optional as long as
	# no other functions share the same name.
	fn revive():
		print("revived")


fn main():
	var character = Player()
	character.jump()
```

## Want to Contribute?
You are more than welcome to contribute whatever you have, wheather that be an idea or code!
### Contributing Ideas
If you want to contribute your thoughts and ideas to the project navigate over to `issues` and create an new issue with your thoughts, even criticism is welcome!
### Contributing Code
Qu is written in Rust and I'll assume you are already familiar with it and Git. The entry point of Qu is `qube/src/main.rs` and the api to the language is `qu/src/lib.rs`. These are the most important files to understand first. If you have any **qu**estions contact me, even through the `issues` page, and I'll do my best to help!

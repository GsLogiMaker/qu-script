
# Qu Script
![alt text](https://github.com/GsLogiMaker/qu-script/blob/6d593de1afebed6874ffff39b046e8f8c071e020/Logo.png)

```
fn main():
  print("Hello world!")
```

> __Warning__ 'Qu' related puns lie ahead!


## What is Qu?
Good **qu**estion, glad you asked! Qu is both a dynamicly and staticly typed interpreted language that utilizes traits rather than object inheritance. Qu is geared toward being lightweight and fast for the purpose of embedding into games and game engines as a main programming language and/or as modding scripts. Qu was inspired by GDScript, Rust, Python, Wren, Lua, and C# in that order.

> __Note__: The following are part of the design goals; they are not implemented yet. The following is also a progress.

Easy to interop with C, maybe, WHOO! You can remove the default Qu standard library and replace it with your own standard library if you need to ensure scripts can't be used for anything malicous (Pariculary useful in modding!) Qu comes with a Rust-like package manager called **Qu**be for creating projects, running projects, and installing dependencies.

## What Does it Look Like?
```
# A single line comment

#/ A multi
line comment /#

const JUMP_VELOCITY = 100.0

trait Life:
  #/ For an object to implement this trait it
  must declare the following variables and functions. /#
  vl health = 100.0
  vl attack = 20.0
  
  fn die()
  fn eat()
  fn revive()
  
  
class Player:
  impl Life
  
  # Optional static typing
  vl velocity_x float = 0.0
  vl velocity_y float = 0.0
  
  fn jump():
    #/ The 'self' is not required by default, but
    accessing global variables requires the 'glob'
    keyword /#
    velocity_y -= glob.JUMP_VELOCITY
    self.velocity_x = 0.0
  
  # A static function, the 'self' keyword does not
  # work here.
  static fn player():
    return "John Doe"
  
  fn Life.die():
    print("died")

  fn Life.eat(food String):
    print("That", food, "was delicous!")
    
  #/ The preceding 'Life.' is optional as long as
  no other functions share the same name. /#
  fn revive():
    print("revived")
```

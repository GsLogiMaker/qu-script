
# Qu Script
![alt text](https://github.com/GsLogiMaker/qu-script/blob/6d593de1afebed6874ffff39b046e8f8c071e020/Logo.png)

Qu is an interpreted scripting language with a focus on portability, speed, and ease of use. I was inspired to make Qu when looking for a modding script language and found that the most popular one was Lua. In my humble opinion, a good language needs at least optional static typing for saftey, a minmal yet readable syntax, and array indexes to begin at 0, so Lua is no good to me so I decided to make my own, thus Qu was born! I also find that there's also not enough programming languages that use indents instead of curly braces.

> __Note__: Qu is in early development and is likely to change.

### Here's what Qu looks like:
```
# A single line comment

#/ A multi
line comment /#

const JUMP_VELOCITY = 100.0

trait Life:
  #/ For an object to implement this trait it must declare the following variables and functions. /#
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
    #/ The 'self' is not required by default, but accessing global variables requires the 'glob' keyword /#
    velocity_y -= glob.JUMP_VELOCITY
    self.velocity_y -= glob.JUMP_VELOCITY
  
  # A static function, the 'self' keyword does not
  # work here.
  static fn player():
    return "John Doe"
  
  fn Life.die():
    print("died")

  fn Life.eat(food String):
    print("That", food, "was delicous!")
    
  #/ The preceding 'Life.' is optional as long as no other functions share the same name. /#
  fn revive():
    print("revived")
```

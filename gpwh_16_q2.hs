data Shape = CircleShape Circle
           | SquareShape Square 
           | RectangleShape Rectangle

data Circle = Circle Int
data Square = Square Int
data Rectangle = Rectangle Int Int

perimeter :: Shape -> Int
perimeter (CircleShape (Circle r)) = 3 * 2 * r
perimeter (SquareShape (Square s)) = 4 * s
perimeter (RectangleShape (Rectangle w h)) = 2 * (w + h)

area :: Shape -> Int
area (CircleShape (Circle r)) = 3 * r * r
area (SquareShape (Square s)) = s * s
area (RectangleShape (Rectangle w h)) = w * h


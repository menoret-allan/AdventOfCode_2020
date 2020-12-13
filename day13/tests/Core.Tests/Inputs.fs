module Inputs

    let smallSet = """939
7,13,x,x,59,x,31,19"""

    let bigSet = """1001171
17,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,367,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,29,x,613,x,x,x,x,x,x,x,x,x,x,x,x,13"""

    let bigSet2 = """17,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,367,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,29,x,613,x,x,x,x,x,x,x,x,x,x,x,x,13"""

// 17*x+7 = 41*y


// x = 17*y
// x = 41*z-7
// => y= (41*z-7)/17
// x = 367*a-11
// => y= (367*a-11)/17


// y = 17*x
// y = 41*x-7
// => y= (41*z-7)/17
// x = 367*a-11
// => y= (367*a-11)/17


// 5 13

// 9 10
// 7 13


// (613*c) - i + i2 % v = 4
// c + 1
// 613 + 4 % v
// (613*x) + 4 % v = 0
// v * (613*x) + 4

// (613*1) - i + i2 % v = rest1
// (613*2) - i + i2 % v = rest2


// (613*c) - i + i2 % 41 = 0

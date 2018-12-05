
var x = 3;
var y = true;
if (y) { x = x + 1 } else { x };
x

# Good (IntV 4)

var x = 3;
var y = 7;
x = x + y;
y = y * x;
y

# Good (IntV 70)

var x = 3; 
var f = function() { x = x+1 };
f();
x

# Good (IntV 4)

var x = 3;
var y = x + 2;
x = y * 2;
x

# Good (IntV 10)

x

# Error "Variable x undefined"

9 = true

# Error "Can't assign to Literal (IntV 9)"

if (5) { true } else { false }

# Error "Expected boolean but found IntV 5"

3(true)

# Error "Expected function but found IntV 3"

try { 
 4 / 0
} catch {
 99
}

# Good (IntV 99)

var x = 5;
try { 
 x = x / 2;
 x = x / 0
} catch {
};
x

# Good (IntV 2)

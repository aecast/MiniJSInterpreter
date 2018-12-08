
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

var Counter = function(init) {
    var count = init;
    {
       inc: function() { count = this.get() + 1 },
       get: function() { count }
    }
};
var BigCounter = function(init) {
    {
       prototype: Counter(init),
       get: function() { 2 * this.prototype.get() }
    }
};
var c = Counter(1);
var bc = BigCounter(1);
c.inc(); c.inc();
bc.inc(); bc.inc();
{ c: c.get(), bc: bc.get() }

# Good (RecordV [("c", IntV 3), ("bc", IntV 14)])

var x = 3;
var y = x + 2;
x = y * 2;
x

# Good (IntV 10)

var f = function(a) { a + this.g(a) };
var g = function(b) { b * b };
var r = { f:f, g:g };
r.f(10)

# Good (IntV 110)

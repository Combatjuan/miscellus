--------------------------------------------------------------------------------
-- Vars
tests = false

--------------------------------------------------------------------------------
-- Convenience Functions
local p = print

pt = function(t)
	for i, v in ipairs(t) do
		
	end
end
	
--end

pair = {
	1=nil,
	2=nil
}

--------------------------------------------------------------------------------
-- Let's implement pairs and lists
Pair = {}
Pair.__index = Pair

function Pair.create(x, y)
	local pair = {}
	setmetatable(pair, Pair)
	pair[1] = x
	pair[2] = y
	return pair
end

function Pair:car()
	return self[1]
end

function Pair:cdr()
	return self[2]
end

function cons(x, y)
	local t = {
		0: Pair.create(x, y)
end

function 

--------------------------------------------------------------------------------
-- Implementations
function cons(x, y)
	return {x, y}
end

function car(pair)
	return pair[1]
end

function cdr(pair)
	return pair[2]
end

--------------------------------------------------------------------------------
-- Tests
p("Running tests...")

assert(car(cons(1, 2)) == 1)
p(cons(car(cons(1, 2)), cdr(cons(1, 2))))
assert(cons(car(cons(1, 2)), cdr(cons(1, 2))) == cons(1, 2))

p("Done.")

--------------------------------------------------------------------------------



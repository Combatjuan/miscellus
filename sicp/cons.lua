require 'class'

--------------------------------------------------------------------------------
-- Pair class
Pair = class(function(pair, x, y)
	pair:set(x, y)
end)

function Pair.__eq(p1, p2)
	return p1[1] == p2[1] and p1[2] == p2[2]
end

function Pair.set(pair, x, y)
	pair[1] = x
	pair[2] = y
end

function Pair.__tostring(pair)
	return string.format('(%s %s)', tostring(pair[1]), tostring(pair[2]))
end

--------------------------------------------------------------------------------
-- List class
List = class(function(list, ...)
	list:set({...})
end)

function List.set(list, t)
	if (#t == 0) then
		list.null = true
	else
		local length = #t
		local val = nil
		list.value = 
		for i=length, i > 0 do
			list.value = cons(t[i], list.value)
		end
	end
end

function List.__tostring(list)
	local values = {}
	local k = list.value
	while k ~= nil do
		values
	end
	return "'(" .. table.concat(list, ' ') .. ")"
end

--------------------------------------------------------------------------------
function cons(x, y)
	return Pair(x, y)
end

function car(pair)
	return pair[1]
end

function cdr(pair)
	return pair[2]
end

--------------------------------------------------------------------------------
p = Pair(1, 2)
print(p)
print(car(p))
print(cdr(p))
print(cons(1, cons(2, cons(cons(3, 4), 5))))
print(cons(cons(1, 2), cons(3, 4)))

function newline_args(...)
	for i, v in ipairs({...}) do
		print(v)
	end
end

l = List(1, 2, 3, 4, 5)
print(l)
newline_args('hi', 'how', 'are', 'you')

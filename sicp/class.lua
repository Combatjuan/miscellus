-- class.lua
-- Compatible with Lua 5.1 (not 5.0).
function class(base, init)
	local c = {} -- A new class instance
	if not init and type(base) == 'function' then
		init = base
		base = nil
	elseif type(base) == 'table' then
	 -- Our new class is a shallow copy of the base class!
		for i,v in pairs(base) do
			c[i] = v
		end
		c._base = base
	end
	-- The class will be the metatable for all its objects,
	-- and they will look up their methods in it.
	c.__index = c

	-- Expose a constructor which can be called by <classname>(<args>)
	local mt = {}
	mt.__call = function(class_tbl, ...)
	local obj = {}
	setmetatable(obj,c)
	if init then
		init(obj,...)
	else 
		-- Make sure that any stuff from the base class is initialized!
		if base and base.init then
		base.init(obj, ...)
		end
	end
	return obj
	end
	c.init = init
	c.is_a = function(self, klass)
		local m = getmetatable(self)
		while m do 
			if m == klass then return true end
			m = m._base
		end
		return false
	end
	setmetatable(c, mt)
	return c
end


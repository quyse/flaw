local allTestsOk = true

local function test(s, f)
	print("test " .. s .. " ...")
	local t, ok, msg = measure_time(function() return pcall(f) end)
	print(t .. "s")
	if ok then
		print("OK")
	else
		allTestsOk = false
		if msg then
			print("FAILED " .. msg)
		else
			print("FAILED")
		end
	end
end

local chunkArg1, chunkArg2 = ...
test("chunk_args", function()
	assert(chunkArg1 == 123 and chunkArg2 == "hello")
end)

test("random", function()
	local a = 1
	for i = 1, 100000000 do
		a = (a * 1103515245 + 12345) & 0x7fffffff
	end
	assert(a == 660469505)
end)

test("table", function()
	local t = {}
	for i = 1, 1000000 do
		t[i] = i * 2
		assert(#t == i)
	end
	for i = 1000000, 1, -1 do
		assert(t[i] == i * 2);
	end
	for i = 1, 1000000 do
		t[i] = nil
		assert(#t == 1000000 - i);
	end
end)

test("ipairs", function()
	local t = { 1, 3, 4, 2, 5, [7] = 11 }
	local i = 0;
	local tt = {}
	for k,v,z in ipairs(t) do
		assert(t[k] == v)
		tt[k] = v
	end
	assert(#tt == 5)
end)

test("metatables", function()
	local t = { 5, 4, 3, 2, 1 }
	local indexCalled, newIndexCalled, callCalled = 0, 0, 0
	local mt =
		{ __index = function(t, i)
				indexCalled = indexCalled + 1
				return i
			end
		, __newindex = function(t, i, v)
				newIndexCalled = newIndexCalled + 1
			end
		, __call = function(tt, a, b, c)
			assert(t == tt and a == 1 and b == 2 and c == 3)
			callCalled = callCalled + 1
			end
		}
	setmetatable(t, mt)
	for i = 1, 10 do
		assert((i <= 5 and t[i] == 6 - i) or (i > 5 and t[i] == i))
	end
	assert(indexCalled == 5)
	t(1, 2, 3, 4);
	assert(callCalled == 1)
	for i = 1, 10 do
		t[i] = i
	end
	assert(newIndexCalled == 5)
end)

test("pcall", function()
	local a, b = pcall(f, 1, 2)
	assert(a == false)
	print(b)
	a, b = pcall(tostring, 1)
	assert(a and b == "1")
end)

test("select", function()
	local a, b, c = select(1, 1, 2, 3)
	assert(a == 1 and b == 2 and c == 3)
	a, b, c = select(2, 1, 2, 3)
	assert(a == 2 and b == 3 and c == nil)
	a, b, c = select(4, 1, 2, 3)
	assert(a == nil and b == nil and c == nil)
	a, b, c = select(-1, 1, 2, 3)
	assert(a == 3 and b == nil and c == nil)
	a, b, c = select(-2, 1, 2, 3)
	assert(a == 2 and b == 3 and c == nil)
end)

test("dofile", function()
	local ok = false
	_G._chunks =
		{ ["routine.lua"] = function()
			ok = true
			end
		}
	loadfile("routine.lua")()
	assert(ok)
	ok = false
	dofile("routine.lua")
	assert(ok)
end)

test("dynamic_values", function()
	local function f()
		return 1, 2, 3
	end
	local function p(...)
		return ...
	end
	local function q()
		return 6, p(5, p(4, f()))
	end
	local a1, a2, a3, a4, a5, a6 = q()
	assert(a1 == 6 and a2 == 5 and a3 == 4 and a4 == 1 and a5 == 2 and a6 == 3)
	local t = { q() }
	assert(t[1] == 6 and t[2] == 5 and t[3] == 4 and t[4] == 1 and t[5] == 2 and t[6] == 3)
end)

assert(allTestsOk, "SOME TESTS FAILED")
print("ALL TESTS OK")

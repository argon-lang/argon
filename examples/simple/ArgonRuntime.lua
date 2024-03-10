
local ArgonRuntime = {}


local function is_equal_deep(a, b)
    local t = type(a);
    if t ~= type(b) then
        return false;
    elseif type(a) == "number" and math.type(a) ~= math.type(b) then
        return false;
    elseif t == "table" then
        for k, _ in pairs(b) do
            if not a[k] then
                return false;
            end
        end

        for k, v in pairs(a) do
            if not is_equal_deep(v, b[k]) then
                return false;
            end
        end

        return true;
    else
        return a == b;
    end
end

local function intern_value(cache, value)
    if type(value) ~= 'table' then
        return value;
    end

    for i = 1, #cache do
        local cachedValue = cache[i];
        if is_equal_deep(cachedValue, value) then
            return cachedValue;
        end
    end

    cache[#cache + 1] = value;
    return value;
end



function ArgonRuntime:intern_name(name)
    if not self.name_cache then
        self.name_cahce = {}
    end
    return intern_value(self.name_cache, name)
end

function ArgonRuntime:intern_signature(sig)
    if not self.sig_cache then
        self.sig_cache = {}
    end
    return intern_value(self.sig_cache, sig)
end

ArgonRuntime.int_type = { type = "builtin", name = "int" }
ArgonRuntime.bool_type = { type = "builtin", name = "bool" }
ArgonRuntime.string_type = { type = "builtin", name = "string" }
ArgonRuntime.never_type = { type = "builtin", name = "never" }
ArgonRuntime.puts = function(s)
    print(s)
end

return ArgonRuntime


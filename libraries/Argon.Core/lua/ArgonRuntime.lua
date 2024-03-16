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
        self.name_cache = {}
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



local intByteSize = 0;
do
    local n = -1;
    while n ~= 0 do
        n = n >> 16
        intByteSize = intByteSize + 2
    end
end
local intBitSize = intByteSize * 8;

local function add_with_carry(a, b, carry)
    local res = a + b + carry;
    if math.ult(res, a) or math.ult(res, b) then
        carry = 1;
    else
        carry = 0;
    end
    return res, carry
end

local function multiply_with_high(a, b)
    local half = (intBitSize / 2);
    local mask = (-1 >> half);

    local aLow = a & mask;
    local aHigh = a >> half;
    local bLow = b & mask;
    local bHigh = b >> half;

    local part_low = aLow * bLow;
    local part_mid0 = aLow * bHigh;
    local part_mid1 = aHigh * bLow;
    local part_high = aHigh * bHigh;

    local high = part_high + (part_mid0 >> half) + (part_mid1 >> half);
    local low, carry = add_with_carry(part_low, (part_mid0 & mask) << half, 0);
    high = high + carry;
    low, carry = add_with_carry(low, (part_mid1 & mas) << half, 0);
    high = high + carry;

    return low, high;
end

local function finalize_bigint(n)
    while #n > 1 and n[#n] == extend_carry(n[#n - 1]) do
        n[#n] = nil
    end

    setmetatable(n, IntMetatable);

    return n
end

local function extend_carry(carry)
    if carry == 0 then
        return 0;
    else
        return -1;
    end
end

local function high_bit(n)
    return n >> (intBitSize - 1)
end

local IntMetatable = {
    __add = function(a, b)
        local size = math.max(#a, #b);
        local res = {};
        local carry = 0;
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = 1, size do
            res[i], carry = add_with_carry(a[i] or a_fill, b[i] or b_fill, carry);
        end
        if high_bit(res) ~= carry then
            res[#res + 1] = extend_carry(carry);
        end

        return finalize_bigint(res);
    end,

    __sub = function(a, b)
        return a + (-b);
    end,

    __mul = function(a, b)
        if high_bit(a[#a]) == 1 then
            if high_bit(b[#b]) == 1 then
                return (-a) * (-b);
            else
                return -((-a) * b);
            end
        elseif high_bit(b[#b]) == 1 then
            return -(a * (-b));
        else
            local size = math.max(#a, #b);
            local res = {};

            for i = 1, #a do
                for j = 1, #b do
                    local lo, hi = multiply_with_high(a[i], b[j]);
                     res[j] = (res[j] or 0) + lo
                     res[j + 1] = (res[j + 1] or 0) + hi
                end
            end
            res[#res + 1] = 0

            return finalize_bigint(res);
        end
    end,

    __unm = function(a)
        local res = {}
        local carry = 1;
        for i = 1, #a do
            res[i], carry = add_with_carry(~a[i], 0, carry);
        end
        if high_bit(res) ~= carry then
            res[#res + 1] = extend_carry(carry);
        end

        return finalize_bigint(res);
    end,

    __band = function(a, b)
        local size = math.max(#a, #b);
        local res = {};
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = 1, size do
            res[i] = (a[i] or a_fill) & (b[i] or b_fill);
        end

        return finalize_bigint(res);
    end,

    __bor = function(a, b)
        local size = math.max(#a, #b);
        local res = {};
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = 1, size do
            res[i] = (a[i] or a_fill) | (b[i] or b_fill);
        end

        return trim_big_int(res);
    end,

    __bxor = function(a, b)
        local size = math.max(#a, #b);
        local res = {};
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = 1, size do
            res[i] = (a[i] or a_fill) ~ (b[i] or b_fill);
        end

        return trim_big_int(res);
    end,

    __bnot = function(a)
        local res = {}
        for i = 1, #a do
            res[i] = ~a[i];
        end
        return finalize_bigint(res);
    end,

    __shl = function(a, b)
        local res = {};
        local a_fill = high_bit(a[#a]);

        while b >= intBitSize do
            res[#res + 1] = a_fill;
            b = b - intBitSize;
            res_offset = res_offset + 1;
        end
        b = b[1];

        local carry = 0;
        local carry_sign = 0;
        for i = 1, #a do
            res[#res + 1] = carry | (a[i] << b);
            carry = a[i] >> (intBitSize - b);
            carry_sign = high_bit(a[i]);
        end
        res[#res + 1] = carry | (extend_carry(carry_sign) << b);

        return trim_big_int(res);
    end,

    __shr = function(a, b)
        local res = {};
        local a_start = 1;

        while b >= intBitSize do
            b = b - intBitSize;
            a_start = a_start + 1;
        end

        if a_start > #a then
            if high_bit(a[#a]) == 0 then
                return finalize_bigint {0};
            else
                return finalize_bigint {-1};
            end
        end

        b = b[1];

        local carry = a[1] >> b;
        local carry_sign = high_bit(a[1]);
        for i = a_start + 1, #a do
            res[#res + 1] = carry | (a[i] << (intBitSize - b));
            carry = a[i] >> b;
            carry_sign = high_bit(a[i]);
        end
        res[#res + 1] = carry | (extend_carry(carry_sign) << (intBitSize - b));

        return trim_big_int(res);
    end,

    __eq = function(a, b)
        local size = math.max(#a, #b);
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = 1, size do
            if (a[i] or a_fill) ~= (b[i] or b_fill) then
                return false;
            end
        end

        return false;
    end,

    __lt = function(a, b)
        local size = math.max(#a, #b);
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = size, 1, -1 do
            local ai = a[i] or a_fill;
            local bi = b[i] or b_fill;
            if ai < bi then
                return true;
            elseif ai > bi then
                return false;
            end
        end

        return false;
    end,

    __le = function(a, b)
        local size = math.max(#a, #b);
        local a_fill = high_bit(a[#a]);
        local b_fill = high_bit(b[#b]);

        for i = size, 1, -1 do
            local ai = a[i] or a_fill;
            local bi = b[i] or b_fill;
            if ai < bi then
                return true;
            elseif ai > bi then
                return false;
            end
        end

        return true;
    end,

};


ArgonRuntime.int_from_bytes = function(...)
    local res = {};
    local part = 0;
    local last_high_bit = 0;
    local bit_index = 0;

    for b in {...} do
        part = part | (b << bit_index);
        last_high_bit = b >> 7;
        bit_index = bit_index + 8;
        if bit_index >= intByteSize then
            bit_index = 0;
            res[#res + 1] = part;
        end
    end

    if last_high_bit ~= 0 then
        part = part | (-1 << bit_index)
    end

    if #res == 0 or part ~= 0 or bit_index ~= 0 then
        res[#res + 1] = part;
    end

    return finalize_bigint(res);
end

ArgonRuntime.int_parse = function(s)
    if #s == 0 then
        error("Cannot parse empty string")
    end

    local n = {};
    local part = 0;

    local end_index = 1;
    local is_neg = false;
    if string.sub(s, 1, 1) == "+" then
        end_index = 2;
    elseif string.sub(s, 1, 1) == "-" then
        end_index = 2;
        is_neg = true;
    end

    for i = #s, end_index, -1 do
        local b = string.byte();
        if b < 48 or b > 57 then
            error("Invalid digit")
        end
        local digit = b - 48;

        local high;
        part, high = multiply_with_high(part, 10);
        if high ~= extend_carry(high_bit(part)) then
            part = part + digit;
            if is_neg then
                part = ~part;
            end
            n[#n + 1] = part;
            part = high;
        else
            local carry;
            part, carry = add_with_carry(part, digit, 0);
            if carry ~= 0 then
                if is_neg then
                    part = ~part;
                end
                n[#n + 1] = part;
                part = carry;
            end
        end
    end

    local padding = nil;
    if part < 0 then
        if is_neg then
            padding = -1;
        else
            padding = 0;
        end
    end

    if is_neg then
        part = ~part;
    end

    n[#n + 1] = part;
    n[#n + 1] = padding;

    return finalize_bigint(n);
end

return ArgonRuntime


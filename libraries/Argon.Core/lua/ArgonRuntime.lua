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


local finalize_bigint, IntMetatable;

ArgonRuntime.int_to_s = function(i)
    local neg = false;
    if i.sign < 0 then
        neg = true;
        i = -i;
    end

    local s = "";
    while i.sign > 0 do
        local digit = i[1] % 10;
        s = digit .. s;
        i = ArgonRuntime.int_divide(i, finalize_bigint {10, sign = 1})
    end

    if s == "" then
        return "0";
    elseif neg then
        return "-" .. s;
    else
        return s;
    end
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
    local result = a + b + carry;

    local res_carry = 0;
    if math.ult(result, a) or math.ult(result, b) then
            res_carry = 1;
    end

    return result, res_carry;
end

local function sub_with_borrow(a, b, borrow)
    local result = a - b + borrow;

    local res_borrow = 0;
    if (borrow == 0 and math.ult(a, b)) or (borrow < 0 and not math.ult(b, a)) then
            res_borrow = -1;
    end

    return result, res_borrow;
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

local function extend_carry(carry)
    if carry == 0 then
        return 0;
    else
        return -1;
    end
end

function finalize_bigint(n)
    while #n > 1 and n[#n] == 0 do
        n[#n] = nil;
    end

    if #n == 1 and n[1] == 0 then
        n.sign = 0;
    end

    setmetatable(n, IntMetatable);
    return n;
end

local function high_bit(n)
    return n >> (intBitSize - 1)
end

IntMetatable = {
    __add = function(a, b)
        if a.sign == 0 then
            return b;
        elseif b.sign == 0 then
            return a;
        elseif a.sign ~= b.sign then
            return b - (-a);
        else
            local carry = 0;
            local n = math.max(#a, #b);
            local result = { sign = a.sign };
            for i = 1, n do
                local digit;
                digit, carry = add_with_carry(a[i] or 0, b[i] or 0, carry);
                result[i] = digit;
            end
            if carry ~= 0 then
                result[n + 1] = carry;
            end
            return finalize_bigint(result);
        end
    end,

    __sub = function(a, b)
        if a.sign == 0 then
            return -b;
        elseif b.sign == 0 then
            return a;
        elseif a.sign ~= b.sign then
            return a + (-b);
        elseif a.sign < 0 then
            return -(-a - -b);
        elseif a < b then
            return -(b - a)
        else
            local borrow = 0;
            local n = math.max(#a, #b);
            local result = { sign = 1 };
            for i = 1, n do
                local digit;
                digit, borrow = sub_with_borrow(a[i] or 0, b[i] or 0, borrow);
                result[i] = digit;
            end
            return finalize_bigint(result);
        end
    end,

    __mul = function(a, b)
        if a.sign == 0 then
            return a;
        elseif b.sign == 0 then
            return b;
        elseif a.sign < 0 then
            return -a * b;
        elseif b.sign < 0 then
            return a * -b;
        else
            local result = finalize_bigint { 0, sign = 0 };

            while b.sign > 0 do
                result = result + a;
                b = b - finalize_bigint { 1, sign = 1 };
            end

            return result;
        end
    end,

    __unm = function(a)
        local result = {};
        if a.sign == 0 then
            return a;
        elseif a.sign < 0 then
            result.sign = 1;
        else
            result.sign = -1;
        end
        for i = 1, #a do
            result[i] = a[i];
        end
        return finalize_bigint(result)
    end,

    __band = function(a, b)
        local abytes = ArgonRuntime.int_to_bytes(a);
        local bbytes = ArgonRuntime.int_to_bytes(b);
        local afill;
        local bfill;
        if a.sign >= 0 then
            afill = 0;
        else
            afill = 0xFF;
        end
        if b.sign >= 0 then
            bfill = 0;
        else
            bfill = 0xFF;
        end
        
        local result = {};
        for i = 1, math.max(#a, #b) do
            result[i] = (abytes[i] or afill) & (bbytes[i] or bfill);
        end
        return ArgonRuntime.int_from_bytes(table.unpack(result));
    end,

    __bor = function(a, b)
        local abytes = ArgonRuntime.int_to_bytes(a);
        local bbytes = ArgonRuntime.int_to_bytes(b);
        local afill;
        local bfill;
        if a.sign >= 0 then
            afill = 0;
        else
            afill = 0xFF;
        end
        if b.sign >= 0 then
            bfill = 0;
        else
            bfill = 0xFF;
        end
        
        local result = {};
        for i = 1, math.max(#a, #b) do
            result[i] = (abytes[i] or afill) | (bbytes[i] or bfill);
        end
        return ArgonRuntime.int_from_bytes(table.unpack(result));
    end,

    __bxor = function(a, b)
        local abytes = ArgonRuntime.int_to_bytes(a);
        local bbytes = ArgonRuntime.int_to_bytes(b);
        local afill;
        local bfill;
        if a.sign >= 0 then
            afill = 0;
        else
            afill = 0xFF;
        end
        if b.sign >= 0 then
            bfill = 0;
        else
            bfill = 0xFF;
        end
        
        local result = {};
        for i = 1, math.max(#a, #b) do
            result[i] = (abytes[i] or afill) ~ (bbytes[i] or bfill);
        end
        return ArgonRuntime.int_from_bytes(table.unpack(result));
    end,

    __bnot = function(a)
        local abytes = ArgonRuntime.int_to_bytes(a);        
        local result = {};
        for i = 1, #a do
            result[i] = ~abytes[i];
        end
        return ArgonRuntime.int_from_bytes(table.unpack(result));
    end,

    __shl = function(a, b)
        if b.sign < 0 then
            return a >> -b;
        end

        local abytes = ArgonRuntime.int_to_bytes(a);
        local afill;
        if a.sign >= 0 then
            afill = 0;
        else
            afill = 0xFF;
        end

        local q, r = ArgonRuntime.int_divmod(b, finalize_bigint { 8, sign = 1 });
        r = r[1];
        abytes[#abytes + 1] = afill;
        local prev = 0;
        for i = 1, #abytes do
            local next = abytes[i];
            abytes[i] = ((abytes[i] << r) & 0xFF) | (prev >> (8 - r));
            prev = next;
        end

        while q.sign > 0 do
            table.insert(abytes, 1, 0);
            q = q - finalize_bigint { 1, sign = 1 };
        end
        
        return ArgonRuntime.int_from_bytes(table.unpack(abytes));
    end,

    __shr = function(a, b)
        if b.sign < 0 then
            return a << -b;
        end

        local abytes = ArgonRuntime.int_to_bytes(a);
        local afill;
        if a.sign >= 0 then
            afill = 0;
        else
            afill = 0xFF;
        end

        local q, r = ArgonRuntime.int_divmod(b, finalize_bigint { 8, sign = 1 });
        r = r[1];

        while q.sign > 0 do
            table.remove(abytes, 1);
            q = q - finalize_bigint { 1, sign = 1 };
        end

        local prev = afill;
        for i = #abytes, 1, -1 do
            local next = abytes[i];
            abytes[i] = ((prev << (8 - r)) & 0xFF) | (abytes[i] >> r);
            prev = next;
        end
        
        return ArgonRuntime.int_from_bytes(table.unpack(abytes));
    end,

    __eq = function(a, b)
        if a.sign ~= b.sign then
            return false;
        end
        if #a ~= #b then
            return false;
        end

        for i = 1, #a do
            if a[i] ~= b[i] then
                return false;
            end
        end

        return true;
    end,

    __lt = function(a, b)
        if a.sign == 0 and b.sign == 0 then
            return false;
        end
        if a.sign < b.sign then
            return true;
        end
        if b.sign < a.sign then
            return false;
        end

        local abslt = false;
        if #a < #b then
            abslt = true;
        elseif #a == #b then
            for i = #a, 1, -1 do
                if math.ult(a[i], b[i]) then
                    abslt = true;
                    break;
                elseif a[i] ~= b[i] then
                    break;
                end
            end
        end

        if a.sign < 0 then
            return not abslt;
        else
            return abslt;
        end
    end,

    __le = function(a, b)
        return not (b < a);
    end,

    __tostring = function(a)
        return ArgonRuntime.int_to_s(a);
    end,

};


ArgonRuntime.int_from_bytes = function(...)
    local bytes = table.pack(...);

    if (bytes[bytes.n] & 0x80) == 0x80 then
        local bytes2 = { n = bytes.n };
        for i = 1, bytes.n do
            bytes2[i] = (~bytes[i]) & 0xFF;
        end

        return -ArgonRuntime.int_from_bytes(table.unpack(bytes2)) - finalize_bigint {1, sign = 1};
    end

    local res = { sign = 1 };
    local part = 0;
    local bit_index = 0;

    for i = 1, bytes.n do
        local b = bytes[i] & 0xFF;

        part = part | (b << bit_index);
        bit_index = bit_index + 8;
        if bit_index >= intBitSize then
            bit_index = 0;
            res[#res + 1] = part;
            part = 0;
        end
    end

    if #res == 0 or part ~= 0 or bit_index ~= 0 then
        res[#res + 1] = part;
    end

    return finalize_bigint(res);
end



ArgonRuntime.int_to_bytes = function(n)
    if n.sign == 0 then
        return { 0 };
    elseif n.sign > 0 then
        local bytes = {};
        for i = 1, #n do
            local word = n[i];
            for j = 1, intByteSize do
                bytes[#bytes + 1] = word & 0xFF;
                word = word >> 8;
            end
        end
        bytes[#bytes + 1] = 0;
        return bytes;
    else
        local bytes = int_to_bytes(-n - 1);
        for i = 1, #bytes do
            bytes[i] = ~bytes[i];
        end
    end
end

ArgonRuntime.int_parse = function(s)
    if #s == 0 then
        error("Cannot parse empty string")
    end


    local end_index = 1;
    local is_neg = false;
    if string.sub(s, 1, 1) == "+" then
        end_index = 2;
    elseif string.sub(s, 1, 1) == "-" then
        end_index = 2;
        is_neg = true;
    end


    local n = finalize_bigint { 0, sign = 0 };

    for i = #s, end_index, -1 do
        local b = string.byte();
        if b < 48 or b > 57 then
            error("Invalid digit")
        end
        local digit = b - 48;

        n = (n * finalize_bigint { 10, sign = 1 }) + finalize_bigint { digit, sign = 1 };
    end

    if is_neg then
        return -n;
    else
        return n;
    end
end

ArgonRuntime.int_divmod = function(a, b)
    if b.sign == 0 then
        error("divide by zero")
    end

    if b.sign < 0 then
        local q, r = ArgonRuntime.int_divide(a, -b);
        return -q, r;
    elseif a.sign < 0 then
        local q, r = ArgonRuntime.int_divide(-a, b);
        if r.sign == 0 then
            return -q, r;
        else
            return -q - 1, b - r;
        end
    end


    local q = finalize_bigint {0, sign = 0};
    local r = a;

    while r >= b do
        q = q + finalize_bigint {1, sign = 1};
        r = r - b;
    end

    return q, r;
end

ArgonRuntime.int_divide = function(a, b)
    local q = ArgonRuntime.int_divmod(a, b);
    return q;
end

return ArgonRuntime


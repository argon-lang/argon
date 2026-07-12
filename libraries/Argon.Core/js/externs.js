
externFunction("int_to_s", function int_to_s(i) {
    return i.toString();
});

externFunction("u8_to_s", function u8_to_s(u8) {
    return (u8 & 0xFF).toString();
})

externFunction("i8_to_s", function i8_to_s(i8) {
    return i8.toString();
})

externFunction("u16_to_s", function u16_to_s(u16) {
    return (u16 & 0xFFFF).toString();
})

externFunction("i16_to_s", function i16_to_s(i16) {
    return i16.toString();
})

externFunction("puts", function puts(s) {
    console.log(s);
});

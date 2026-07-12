
externFunction("int_to_s", function int_to_s(i) {
    return i.toString();
});

externFunction("u8_to_s", function u8_to_s(u8) {
    return (u8 & 0xFF).toString();
})

externFunction("puts", function puts(s) {
    console.log(s);
});


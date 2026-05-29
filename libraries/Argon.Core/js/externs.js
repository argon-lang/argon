
externFunction("int_to_s", function int_to_s(i) {
    return i.toString();
});

externFunction("puts", function puts(s) {
    console.log(s);
});


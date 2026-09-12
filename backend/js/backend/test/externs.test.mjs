import assert from "node:assert/strict";
import test from "node:test";

import { JsExtern } from "@argon-lang/js-backend-api";

import { loadExterns } from "../lib/externs.js";

function inputFile(source) {
    const contents = new TextEncoder().encode(source);
    return {
        fileName: "externs.js",
        async open() {
            let offset = 0;
            return {
                async read(buffer) {
                    const count = Math.min(buffer.length, contents.length - offset);
                    buffer.set(contents.subarray(offset, offset + count));
                    offset += count;
                    return count;
                },
                async close() {},
            };
        },
    };
}

async function decodeExtern(source, name = "test", kind = "extern-function") {
    const externs = await loadExterns([inputFile(source)]);
    const metadata = externs.get(name);
    assert.ok(metadata);
    assert.equal(metadata.$type, kind);
    const decoded = JsExtern.codec.decode(metadata.implementation);
    assert.equal(decoded.success, true);
    return decoded.value;
}

test("method factories emit their distinct metadata kinds", async () => {
    const method = await decodeExtern(`externMethod("test", self => self);`, "test", "extern-method");
    assert.equal(method.declaration.type, "ArrowFunctionExpression");

    const staticMethod = await decodeExtern(
        `externStaticMethod("test", () => "static");`, "test", "extern-static-method",
    );
    assert.equal(staticMethod.declaration.type, "ArrowFunctionExpression");
});

test("extern names are unique across all kinds", async () => {
    await assert.rejects(loadExterns([inputFile(`
        externFunction("same", () => 1);
        externMethod("same", () => 2);
    `)]), /Duplicate extern: same/);
});

test("unknown extern factories are rejected", async () => {
    await assert.rejects(loadExterns([inputFile(`externUnknown("test", () => 1);`)]),
        /Unknown extern factory function: externUnknown/);
});

test("arrow functions are retained as extern expressions", async () => {
    const extern = await decodeExtern(`
        externFunction("test", (left, right) => left + right);
    `);

    assert.equal(extern.declaration.type, "ArrowFunctionExpression");
    assert.equal(extern.declaration.params.length, 2);
});

test("function expressions remain supported", async () => {
    const extern = await decodeExtern(`
        externFunction("test", function test(value) { return value; });
    `);

    assert.equal(extern.declaration.type, "FunctionExpression");
});

test("arbitrary expressions retain their referenced imports", async () => {
    const extern = await decodeExtern(`
        import { join as importedJoin } from "node:path";
        externFunction("test", createFunction(importedJoin));
    `);

    assert.equal(extern.declaration.type, "CallExpression");
    assert.deepEqual(extern.imports, [{
        localAlias: "importedJoin",
        source: "node:path",
        member: "join",
    }]);
});

test("non-function expressions are preserved without loader validation", async () => {
    const extern = await decodeExtern(`externFunction("test", 42);`);

    assert.equal(extern.declaration.type, "Literal");
    assert.equal(extern.declaration.value, 42);
});

test("array expressions are scanned for imported values", async () => {
    const extern = await decodeExtern(`
        import { join as importedJoin } from "node:path";
        externFunction("test", [importedJoin]);
    `);

    assert.equal(extern.declaration.type, "ArrayExpression");
    assert.equal(extern.imports[0].localAlias, "importedJoin");
});

test("spread arguments are not valid extern expressions", async () => {
    await assert.rejects(
        loadExterns([inputFile(`externFunction("test", ...implementations);`)]),
        /must be an expression/,
    );
});
